;;; eca-chat-test.el --- Tests for eca-chat -*- lexical-binding: t; -*-
;;; Commentary:
;; Tests for `eca-chat--key-pressed-deletion' prompt boundary logic.
;;; Code:
(require 'buttercup)
(require 'eca-chat)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun eca-chat-test--make-prompt-buffer (prompt-text)
  "Create a test buffer with PROMPT-TEXT in a simulated prompt.
Mirrors the real prompt-block layout: history, then the
separator, task, progress and context areas, then the prompt
field.  Returns the buffer.  Caller must kill it."
  (let ((buf (generate-new-buffer " *test-chat-deletion*")))
    (with-current-buffer buf
      ;; History (read-only region).
      (insert "header")
      ;; Prompt area starts at the separator newline.
      (let ((area-start (point)))
        (insert "\n---")
        (let ((task-start (point)))       ; task area (a space)
          (insert " ")
          (let ((progress-start (point))) ; progress area (a newline)
            (insert "\n")
            (let ((context-start (point))) ; context area ("@")
              (insert "@\n")
              (let ((prompt-start (point)))
                (insert prompt-text)
                ;; Overlays matching the real layout.
                (overlay-put (make-overlay area-start (1+ area-start))
                             'eca-chat-prompt-area t)
                (overlay-put (make-overlay task-start task-start)
                             'eca-chat-task-area t)
                (overlay-put (make-overlay progress-start progress-start)
                             'eca-chat-progress-area t)
                (overlay-put (make-overlay context-start (1+ context-start))
                             'eca-chat-context-area t)
                (overlay-put (make-overlay prompt-start (1+ prompt-start))
                             'eca-chat-prompt-field t))))))
      (setq major-mode 'eca-chat-mode))
    buf))

(defun eca-chat-test--prompt-text (buf)
  "Return the prompt field text from BUF."
  (with-current-buffer buf
    (buffer-substring-no-properties
     (eca-chat--prompt-field-start-point) (point-max))))

(defun eca-chat-test--call-on (text marker fn)
  "Fontify TEXT in a gfm buffer, move point onto MARKER, then call FN.
TEXT is inserted on the third line (after a heading) so markdown
does not treat the first line as metadata.  Returns FN's value."
  (with-temp-buffer
    (insert "# Title\n\n" text)
    (delay-mode-hooks (gfm-mode))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward marker)
    (goto-char (match-beginning 0))
    (funcall fn)))

(defmacro eca-chat-test--with-display-buffers (buffers &rest body)
  "Create BUFFERS and evaluate BODY in an isolated display setup."
  (declare (indent 1) (debug (sexp body)))
  `(let (,@(mapcar (lambda (buffer)
                     `(,buffer
                       (generate-new-buffer
                        ,(format " *test-chat-display-%s*" buffer))))
                   buffers)
         (display-buffer-alist nil)
         (display-buffer-overriding-action nil)
         (display-buffer-base-action nil)
         (display-buffer-fallback-action nil)
         (pop-up-frames nil)
         (pop-up-windows t))
     (unwind-protect
         (save-window-excursion
           (delete-other-windows)
           ,@body)
       (dolist (buffer (list ,@buffers))
         (when (buffer-live-p buffer)
           (kill-buffer buffer))))))

(defun eca-chat-test--make-render-buffer ()
  "Create a minimal chat buffer suitable for render-content tests."
  (let ((buf (eca-chat-test--make-prompt-buffer "")))
    (with-current-buffer buf
      (setq-local eca-chat--id "chat-1")
      (setq-local eca-chat-expandable--id->ov
                  (make-hash-table :test 'equal))
      (setq-local eca-chat--tool-call-prepare-counters
                  (make-hash-table :test 'equal))
      (setq-local eca-chat--tool-call-prepare-content-cache
                  (make-hash-table :test 'equal))
      (setq-local eca-chat--tool-call-elapsed-times
                  (make-hash-table :test 'equal))
      (setq-local eca-chat--subagent-chat-id->tool-call-id
                  (make-hash-table :test 'equal))
      (setq-local eca-chat--subagent-usage
                  (make-hash-table :test 'equal)))
    buf))

(defun eca-chat-test--tool-call-content (type id &optional manual)
  "Build a generic tool-call content plist of TYPE and ID.
When MANUAL is non-nil the tool call requires manual approval."
  (list :type type
        :id id
        :name "testTool"
        :server "testServer"
        :arguments "{}"
        :manualApproval manual
        :details (list :type "generic")))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(describe "eca-chat--has-pending-approvals-p"

  (it "caches the buffer scan between calls"
    (with-temp-buffer
      (insert (propertize "[accept]"
                          'eca-tool-call-pending-approval-accept t))
      (expect (eca-chat--has-pending-approvals-p) :to-be-truthy)
      (spy-on 'text-property-search-forward)
      (expect (eca-chat--has-pending-approvals-p) :to-be-truthy)
      (expect 'text-property-search-forward :not :to-have-been-called)))

  (it "rescans only after the cache is invalidated"
    (with-temp-buffer
      (insert (propertize "[accept]"
                          'eca-tool-call-pending-approval-accept t))
      (expect (eca-chat--has-pending-approvals-p) :to-be-truthy)
      (erase-buffer)
      ;; Stale until invalidated: redisplay code must never rescan.
      (expect (eca-chat--has-pending-approvals-p) :to-be-truthy)
      (eca-chat--invalidate-pending-approvals-cache)
      (expect (eca-chat--has-pending-approvals-p) :to-be nil)))

  (it "tracks approvals through the tool call lifecycle rendering"
    (let ((buf (eca-chat-test--make-render-buffer))
          (session (make-eca--session)))
      (unwind-protect
          (eca-chat--with-current-buffer buf
            (expect (eca-chat--has-pending-approvals-p) :to-be nil)
            (eca-chat--render-content
             session buf "assistant"
             (eca-chat-test--tool-call-content "toolCallRun" "tool-1" t)
             nil)
            (expect (eca-chat--has-pending-approvals-p) :to-be-truthy)
            (eca-chat--render-content
             session buf "assistant"
             (eca-chat-test--tool-call-content "toolCalled" "tool-1")
             nil)
            (expect (eca-chat--has-pending-approvals-p) :to-be nil))
        (kill-buffer buf))))

  (it "resets the cache when clearing the chat"
    (let ((buf (eca-chat-test--make-render-buffer))
          (session (make-eca--session)))
      (unwind-protect
          (eca-chat--with-current-buffer buf
            (eca-chat--render-content
             session buf "assistant"
             (eca-chat-test--tool-call-content "toolCallRun" "tool-1" t)
             nil)
            (expect (eca-chat--has-pending-approvals-p) :to-be-truthy)
            (eca-chat--clear)
            (expect (eca-chat--has-pending-approvals-p) :to-be nil))
        (kill-buffer buf))))

  (it "does not resurrect completed approvals from older history"
    (let ((buf (eca-chat-test--make-render-buffer))
          (session (make-eca--session)))
      (unwind-protect
          (eca-chat--with-current-buffer buf
            (spy-on 'font-lock-ensure)
            (spy-on 'eca-chat--align-tables)
            (spy-on 'eca-chat--beautify-tables)
            (eca-chat--render-content
             session buf "assistant"
             (eca-chat-test--tool-call-content "toolCalled" "tool-1")
             nil)
            ;; The run event of an already-finished tool call arrives
            ;; later from an older history page.
            (eca-chat--render-history-contents
             session buf
             (list (list :role "assistant"
                         :content (eca-chat-test--tool-call-content
                                   "toolCallRun" "tool-1" t))))
            (expect (eca-chat--has-pending-approvals-p) :to-be nil)
            (goto-char (point-min))
            (expect (text-property-search-forward
                     'eca-tool-call-pending-approval-accept t t)
                    :to-be nil))
        (kill-buffer buf))))

  (it "keeps a subagent parent pending while a sibling child is pending"
    (let ((buf (eca-chat-test--make-render-buffer))
          (session (make-eca--session)))
      (unwind-protect
          (eca-chat--with-current-buffer buf
            (eca-chat--tool-call-subagent-details
             "parent-1" (list :agent "test-agent" :task "task")
             "Calling subagent" nil nil
             eca-chat-mcp-tool-call-loading-symbol nil
             (list :type "subagent" :model "test-model"))
            (eca-chat--render-content
             session buf "assistant"
             (eca-chat-test--tool-call-content "toolCallRun" "tool-1" t)
             nil "parent-1" "child-chat")
            (eca-chat--render-content
             session buf "assistant"
             (eca-chat-test--tool-call-content "toolCallRun" "tool-2" t)
             nil "parent-1" "child-chat")
            (expect (overlay-get (eca-chat--get-expandable-content "parent-1")
                                 'eca-chat--tool-call-status)
                    :to-equal eca-chat-mcp-tool-call-pending-approval-symbol)
            (eca-chat--render-content
             session buf "assistant"
             (eca-chat-test--tool-call-content "toolCalled" "tool-1")
             nil "parent-1" "child-chat")
            (expect (overlay-get (eca-chat--get-expandable-content "parent-1")
                                 'eca-chat--tool-call-status)
                    :to-equal eca-chat-mcp-tool-call-pending-approval-symbol)
            (eca-chat--render-content
             session buf "assistant"
             (eca-chat-test--tool-call-content "toolCalled" "tool-2")
             nil "parent-1" "child-chat")
            (expect (overlay-get (eca-chat--get-expandable-content "parent-1")
                                 'eca-chat--tool-call-status)
                    :to-equal eca-chat-mcp-tool-call-loading-symbol))
        (kill-buffer buf)))))

(describe "eca-chat--apply-markdown-markup-visibility"
  (it "keeps the historical hidden-markup default"
    (with-temp-buffer
      (let ((eca-chat-hide-markdown-markup t))
        (eca-chat--apply-markdown-markup-visibility)
        (expect (memq 'markdown-markup buffer-invisibility-spec)
                :to-be-truthy))))

  (it "allows markdown markup to stay visible"
    (with-temp-buffer
      (add-to-invisibility-spec 'markdown-markup)
      (let ((eca-chat-hide-markdown-markup nil))
        (eca-chat--apply-markdown-markup-visibility)
        (expect (memq 'markdown-markup buffer-invisibility-spec)
                :to-be nil)))))

(describe "eca-chat--key-pressed-deletion"

  (describe "multi-line prompt"

    (it "deletes newline at beginning of second line"
      (let ((buf (eca-chat-test--make-prompt-buffer "hello\nfooo")))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (eca-chat--prompt-field-start-point))
              (forward-line 1)
              (let ((this-command 'backward-delete-char))
                (eca-chat--key-pressed-deletion
                 (lambda (n &optional _) (delete-char (- n)))
                 1))
              (expect (eca-chat-test--prompt-text buf)
                      :to-equal "hellofooo"))
          (kill-buffer buf))))

    (it "deletes newline even with non-prompt overlays on line"
      (let ((buf (eca-chat-test--make-prompt-buffer "hello\nfooo")))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (eca-chat--prompt-field-start-point))
              (forward-line 1)
              ;; Simulate an hl-line-like overlay on this line
              (make-overlay (line-beginning-position)
                            (line-end-position))
              (let ((this-command 'backward-delete-char))
                (eca-chat--key-pressed-deletion
                 (lambda (n &optional _) (delete-char (- n)))
                 1))
              (expect (eca-chat-test--prompt-text buf)
                      :to-equal "hellofooo"))
          (kill-buffer buf)))))

  (describe "prompt boundary"

    (it "blocks deletion at prompt field start"
      (let ((buf (eca-chat-test--make-prompt-buffer "hello")))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (eca-chat--prompt-field-start-point))
              (let ((this-command 'backward-delete-char)
                    (side-effect-called nil))
                (cl-letf (((symbol-function 'ding) #'ignore))
                  (eca-chat--key-pressed-deletion
                   (lambda (&rest _) (setq side-effect-called t))
                   1))
                (expect side-effect-called :to-be nil)
                (expect (eca-chat-test--prompt-text buf)
                        :to-equal "hello")))
          (kill-buffer buf))))

    (it "allows forward deletion at prompt field start"
      ;; Regression: a forward `delete-char' at the prompt start - plain C-d
      ;; and, under evil, `evil-invert-char' (~), `evil-replace' (r),
      ;; `evil-delete-char' (x) and `evil-substitute' (s) - must remove the
      ;; first prompt char.  The boundary guard used to block it too, so ~
      ;; prepended the inverted char instead of replacing it in place.
      (dolist (cmd '(delete-char evil-invert-char evil-replace
                     evil-delete-char evil-substitute))
        (let ((buf (eca-chat-test--make-prompt-buffer "hello")))
          (unwind-protect
              (with-current-buffer buf
                (goto-char (eca-chat--prompt-field-start-point))
                (let ((this-command cmd)
                      (side-effect-called nil))
                  (eca-chat--key-pressed-deletion
                   (lambda (n &optional _)
                     (setq side-effect-called t)
                     (delete-char n))
                   1)
                  (expect side-effect-called :to-be t)
                  (expect (eca-chat-test--prompt-text buf)
                          :to-equal "ello")))
            (kill-buffer buf)))))))

(describe "eca-chat--protect-non-prompt"

  (it "blocks editing inside the history area"
    (let ((buf (eca-chat-test--make-prompt-buffer "hello"))
          (eca-chat-read-only-history t))
      (unwind-protect
          (with-current-buffer buf
            (eca-chat--protect-non-prompt)
            (goto-char (+ (point-min) 3))
            (expect (insert "x") :to-throw 'text-read-only)
            (goto-char (+ (point-min) 3))
            (expect (delete-char 1) :to-throw 'text-read-only))
        (kill-buffer buf))))

  (it "blocks inserting at the very top of the buffer"
    (let ((buf (eca-chat-test--make-prompt-buffer "hello"))
          (eca-chat-read-only-history t))
      (unwind-protect
          (with-current-buffer buf
            (eca-chat--protect-non-prompt)
            (goto-char (point-min))
            (expect (insert "x") :to-throw 'text-read-only))
        (kill-buffer buf))))

  (it "keeps the prompt editable"
    (let ((buf (eca-chat-test--make-prompt-buffer "hello"))
          (eca-chat-read-only-history t))
      (unwind-protect
          (with-current-buffer buf
            (eca-chat--protect-non-prompt)
            (goto-char (point-max))
            (insert " world")
            (expect (eca-chat-test--prompt-text buf) :to-equal "hello world"))
        (kill-buffer buf))))

  (it "marks newly inserted (streamed) content read-only"
    (let ((buf (eca-chat-test--make-prompt-buffer "hello"))
          (eca-chat-read-only-history t))
      (unwind-protect
          (with-current-buffer buf
            (eca-chat--protect-non-prompt)
            (let ((inhibit-read-only t))
              (goto-char (eca-chat--content-insertion-point))
              (insert "streamed text"))
            (eca-chat--protect-non-prompt)
            (expect (get-text-property (+ (point-min) 8) 'read-only) :to-be t))
        (kill-buffer buf))))

  (it "does nothing when eca-chat-read-only-history is nil"
    (let ((buf (eca-chat-test--make-prompt-buffer "hello"))
          (eca-chat-read-only-history nil))
      (unwind-protect
          (with-current-buffer buf
            (eca-chat--protect-non-prompt)
            (goto-char (point-min))
            (insert "x")
            (expect (char-after (point-min)) :to-equal ?x))
        (kill-buffer buf))))

  (it "locks the separator and task area but keeps context/prompt editable"
    (let ((buf (eca-chat-test--make-prompt-buffer "hi"))
          (eca-chat-read-only-history t))
      (unwind-protect
          (with-current-buffer buf
            (eca-chat--protect-non-prompt)
            (let ((sep (eca-chat--prompt-area-start-point))
                  (prog (overlay-start (eca-chat--prompt-progress-field-ov)))
                  (ctx (overlay-start (eca-chat--prompt-context-field-ov))))
              ;; The separator newline and the "---" text are read-only.
              (goto-char sep)
              (expect (insert "x") :to-throw 'text-read-only)
              ;; The task area (last char before the progress) is read-only.
              (goto-char (1- prog))
              (expect (insert "x") :to-throw 'text-read-only)
              ;; The progress area start stays editable.
              (goto-char prog)
              (insert "p")
              ;; The context area stays editable.
              (goto-char ctx)
              (insert "c")
              ;; The prompt stays editable.
              (goto-char (point-max))
              (insert "!")
              (expect (eca-chat-test--prompt-text buf) :to-equal "hi!")))
        (kill-buffer buf)))))

(describe "eca-chat completion trigger detection"
  (it "triggers context completion when a char like ( precedes @"
    (let ((eca-chat--id "chat-123")
          (eca-chat--context '())
          (session (make-eca--session)))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca--session-workspace-folders :and-return-value '("/local/path"))
      (spy-on 'eca-api-request-while-no-input :and-return-value '(:contexts []))
      (spy-on 'eca-chat--find-typed-query :and-return-value "")
      (spy-on 'eca-chat--point-at-new-context-p :and-return-value nil)
      (with-temp-buffer
        (insert "(@")
        (let* ((capf-res (eca-chat-completion-at-point))
               (completion-fn (nth 2 capf-res)))
          (funcall completion-fn "" nil t)
          (expect 'eca-chat--find-typed-query :to-have-been-called-with eca-chat-context-prefix)))))

  (it "triggers file completion when a char like ( precedes #"
    (let ((eca-chat--id "chat-123")
          (session (make-eca--session)))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca--session-workspace-folders :and-return-value '("/local/path"))
      (spy-on 'eca-api-request-while-no-input :and-return-value '(:files []))
      (spy-on 'eca-chat--find-typed-query :and-return-value "")
      (spy-on 'eca-chat--point-at-new-context-p :and-return-value nil)
      (with-temp-buffer
        (insert "(#")
        (let* ((capf-res (eca-chat-completion-at-point))
               (completion-fn (nth 2 capf-res)))
          (funcall completion-fn "" nil t)
          (expect 'eca-chat--find-typed-query :to-have-been-called-with eca-chat-filepath-prefix)))))

  (it "does not trigger when @ is preceded by a word char"
    (spy-on 'eca-api-request-while-no-input :and-return-value '(:contexts []))
    (spy-on 'eca-chat--point-at-new-context-p :and-return-value nil)
    (spy-on 'eca-chat--point-at-prompt-field-p :and-return-value nil)
    (with-temp-buffer
      (insert "foo@bar")
      (expect (eca-chat-completion-at-point) :to-be nil)
      (expect 'eca-api-request-while-no-input :not :to-have-been-called)))

  (it "spans the completion region from just after @ to point"
    (let ((eca-chat--id "chat-123")
          (eca-chat--context '())
          (session (make-eca--session)))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca--session-workspace-folders :and-return-value '("/ws"))
      (spy-on 'eca-chat--point-at-new-context-p :and-return-value nil)
      (with-temp-buffer
        (insert "see @src/ec")
        (let ((capf-res (eca-chat-completion-at-point)))
          (expect (nth 0 capf-res)
                  :to-equal (+ (point-min) (length "see @")))
          (expect (nth 1 capf-res) :to-equal (point)))))))

(describe "eca-chat--completion-path-label"
  (it "keeps the typed directory part as the label prefix"
    (expect (eca-chat--completion-path-label "src/e" '("/ws") "/ws/src/eca/chat.el" nil)
            :to-equal "src/eca/chat.el"))

  (it "appends a slash to directory labels"
    (expect (eca-chat--completion-path-label "src/e" '("/ws") "/ws/src/eca" t)
            :to-equal "src/eca/"))

  (it "relativizes against a workspace root for plain queries"
    (expect (eca-chat--completion-path-label "chat" '("/ws") "/ws/src/eca/chat.el" nil)
            :to-equal "src/eca/chat.el"))

  (it "keeps ~ prefixed queries as typed"
    (expect (eca-chat--completion-path-label "~/dev/fo" '("/ws")
                                             (expand-file-name "~/dev/foo") nil)
            :to-equal "~/dev/foo"))

  (it "falls back to the absolute path outside any root"
    (expect (eca-chat--completion-path-label "x" '("/ws") "/other/x.el" nil)
            :to-equal "/other/x.el")))

(describe "eca-chat completion directory drill-in"
  (it "keeps completing instead of finalizing a directory item"
    (spy-on 'eca-chat--completion-retrigger)
    (with-temp-buffer
      (let ((item (propertize "src/eca/"
                              'eca-chat-completion-item
                              '(:type "directory" :path "/ws/src/eca")
                              'face 'eca-chat-context-file-face)))
        (insert "@" item)
        (eca-chat--completion-context-from-prompt-exit-function item 'finished))
      (expect (buffer-string) :to-equal "@src/eca/")
      (expect (get-text-property 2 'eca-chat-completion-item) :to-be nil)
      (expect 'eca-chat--completion-retrigger :to-have-been-called)))

  (it "finalizes a file item into a chip with trailing space"
    (let ((session (make-eca--session :workspace-folders '("/ws"))))
      (spy-on 'eca-session :and-return-value session)
      (with-temp-buffer
        (let ((item (propertize "src/eca/chat.el"
                                'eca-chat-completion-item
                                '(:type "file" :path "/ws/src/eca/chat.el"))))
          (insert "@" item)
          (eca-chat--completion-context-from-prompt-exit-function item 'finished))
        (expect (buffer-string) :to-equal "@chat.el "))))

  (it "keeps completing for directories from the # prompt"
    (spy-on 'eca-chat--completion-retrigger)
    (with-temp-buffer
      (let ((item (propertize "src/eca/"
                              'eca-chat-completion-item
                              '(:type "directory" :path "/ws/src/eca"))))
        (insert "#" item)
        (eca-chat--completion-file-from-prompt-exit-function item 'finished))
      (expect (buffer-string) :to-equal "#src/eca/")
      (expect 'eca-chat--completion-retrigger :to-have-been-called))))

(describe "eca-chat--completion-cached-items"
  (it "does not cache interrupted (nil) responses"
    (with-temp-buffer
      (let* ((calls 0)
             (fetch (lambda () (setq calls (1+ calls)) nil)))
        (expect (eca-chat--completion-cached-items
                 'eca-chat--context-completion-cache "q" fetch :contexts #'identity)
                :to-be nil)
        (eca-chat--completion-cached-items
         'eca-chat--context-completion-cache "q" fetch :contexts #'identity)
        (expect calls :to-equal 2))))

  (it "caches successful responses including empty ones"
    (with-temp-buffer
      (let* ((calls 0)
             (fetch (lambda () (setq calls (1+ calls)) '(:contexts []))))
        (eca-chat--completion-cached-items
         'eca-chat--context-completion-cache "q" fetch :contexts #'identity)
        (expect (eca-chat--completion-cached-items
                 'eca-chat--context-completion-cache "q" fetch :contexts #'identity)
                :to-equal nil)
        (expect calls :to-equal 1))))

  (it "caches per query"
    (with-temp-buffer
      (expect (eca-chat--completion-cached-items
               'eca-chat--context-completion-cache "a"
               (lambda () '(:contexts ["a"])) :contexts #'identity)
              :to-equal '("a"))
      (expect (eca-chat--completion-cached-items
               'eca-chat--context-completion-cache "b"
               (lambda () '(:contexts ["b"])) :contexts #'identity)
              :to-equal '("b"))
      (expect (eca-chat--completion-cached-items
               'eca-chat--context-completion-cache "a"
               (lambda () (error "should not fetch")) :contexts #'identity)
              :to-equal '("a")))))

(describe "eca-chat--raw-prompt-contexts"
  (it "resolves workspace-relative raw tokens into contexts"
    (let* ((root (make-temp-file "eca-test-ws" t))
           (file (expand-file-name "foo.el" root))
           (dir (expand-file-name "sub" root))
           (session (make-eca--session)))
      (unwind-protect
          (progn
            (write-region "" nil file)
            (make-directory dir)
            (setf (eca--session-workspace-folders session) (list root))
            (spy-on 'eca-session :and-return-value session)
            (let ((buf (eca-chat-test--make-prompt-buffer
                        "check @foo.el and @sub/ and @missing.el")))
              (unwind-protect
                  (with-current-buffer buf
                    ;; Built outside `expect': on Emacs 29 buttercup's
                    ;; interpreted oclosure thunks shadow the `:type'
                    ;; keyword inside expect args.
                    (let ((expected (list (list :type "file" :path file)
                                          (list :type "directory" :path dir))))
                      (expect (eca-chat--raw-prompt-contexts)
                              :to-equal expected)))
                (kill-buffer buf))))
        (delete-directory root t))))

  (it "skips chip tokens and absolute or ~ mentions"
    (let ((session (make-eca--session))
          (buf (eca-chat-test--make-prompt-buffer "")))
      (spy-on 'eca-session :and-return-value session)
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-max))
            (insert (propertize "@chip.el" 'eca-chat-context-item
                                '(:type "file" :path "/x/chip.el")))
            (insert " @/absolute/path @~/thing")
            (expect (eca-chat--raw-prompt-contexts) :to-be nil))
        (kill-buffer buf)))))

(describe "eca-chat--maybe-finalize-context-token"
  (it "turns a raw @dir token into a context chip after a space"
    (let* ((root (make-temp-file "eca-test-ws" t))
           (dir (expand-file-name "sub" root))
           (session (make-eca--session)))
      (unwind-protect
          (progn
            (make-directory dir)
            (setf (eca--session-workspace-folders session) (list root))
            (spy-on 'eca-session :and-return-value session)
            (spy-on 'eca-chat--point-at-new-context-p :and-return-value nil)
            (let ((buf (eca-chat-test--make-prompt-buffer "@sub/ ")))
              (unwind-protect
                  (with-current-buffer buf
                    (goto-char (point-max))
                    (eca-chat--maybe-finalize-context-token)
                    (expect (eca-chat-test--prompt-text buf) :to-equal "@sub ")
                    ;; Built outside `expect', see eca-chat--raw-prompt-contexts
                    ;; test above.
                    (let ((expected (list :type "directory" :path dir)))
                      (expect (get-text-property
                               (eca-chat--prompt-field-start-point)
                               'eca-chat-context-item)
                              :to-equal expected)))
                (kill-buffer buf))))
        (delete-directory root t))))

  (it "leaves non-path tokens alone"
    (let ((session (make-eca--session))
          (buf (eca-chat-test--make-prompt-buffer "hello @nope ")))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-chat--point-at-new-context-p :and-return-value nil)
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-max))
            (eca-chat--maybe-finalize-context-token)
            (expect (eca-chat-test--prompt-text buf) :to-equal "hello @nope "))
        (kill-buffer buf)))))

(describe "eca-chat path mapping"
  (describe "chat/queryContext"
    (it "maps paths in :contexts to remote"
      (let ((eca-chat--id "chat-123")
            (eca-chat--context '((:type "file" :path "/local/path/file.txt")))
            (session (make-eca--session)))
        (spy-on 'eca-session :and-return-value session)
        (spy-on 'eca--session-workspace-folders :and-return-value '("/local/path"))
        (spy-on 'eca--path-local-to-remote :and-return-value "/remote/path/file.txt")
        (spy-on 'eca-api-request-while-no-input :and-return-value '(:contexts []))
        (spy-on 'eca-chat--find-typed-query :and-return-value "")
        (spy-on 'eca-chat--point-at-new-context-p :and-return-value nil)
        (with-temp-buffer
          (insert "@")
          (let* ((capf-res (eca-chat-completion-at-point))
                 (completion-fn (nth 2 capf-res)))
            (funcall completion-fn "" nil t)
            (expect 'eca--path-local-to-remote :to-have-been-called-with "/local/path/file.txt")
            (expect 'eca-api-request-while-no-input :to-have-been-called-with
                    session
                    :method "chat/queryContext"
                    :params (list :chatId "chat-123"
                                  :query ""
                                  :contexts [(:type "file" :path "/remote/path/file.txt")])))))))

  (describe "chat/promptSteer"
    (it "normalizes the message prompt"
      (let ((eca-chat--id "chat-456")
            (session (make-eca--session)))
        (spy-on 'eca-api-notify)
        (spy-on 'eca--path-local-to-remote :and-return-value "/remote/path/file.txt")
        (let ((prompt (propertize "@file.txt"
                                  'eca-chat-expanded-item-str "@file.txt"
                                  'eca-chat-item-type 'context)))
          (eca-chat--steer-prompt session prompt)
          (expect 'eca--path-local-to-remote :to-have-been-called-with "file.txt")
          (expect 'eca-api-notify :to-have-been-called-with
                  session
                  :method "chat/promptSteer"
                  :params (list :chatId "chat-456"
                                :message "@/remote/path/file.txt"))))))

  (describe "eca-chat--normalize-prompt"
    (it "handles relative paths in expansions"
      (let ((default-directory "/local/path/"))
        (spy-on 'eca--path-local-to-remote :and-return-value "/remote/path/file.txt")
        (let ((prompt (propertize "@file.txt"
                                  'eca-chat-expanded-item-str "@file.txt"
                                  'eca-chat-item-type 'context)))
          (expect (eca-chat--normalize-prompt prompt)
                  :to-equal "@/remote/path/file.txt")
          (expect 'eca--path-local-to-remote :to-have-been-called-with "file.txt"))))))

(describe "eca-chat copy command"
  (it "copies fenced code block content"
    (let (kill-ring
          kill-ring-yank-pointer)
      (with-temp-buffer
        (setq major-mode 'eca-chat-mode)
        (insert "```elisp\n(+ 1 2)\n```\n")
        (eca-chat--refresh-code-copy-scopes (point-min) (point-max))
        (goto-char (point-min))
        (search-forward "(+ 1 2)")
        (eca-chat-copy-at-point)
        (expect (current-kill 0 t) :to-equal "(+ 1 2)"))))

  (it "copies two-backtick fenced code block content"
    (let (kill-ring
          kill-ring-yank-pointer)
      (with-temp-buffer
        (setq major-mode 'eca-chat-mode)
        (insert "``bash\naz login\n``\n")
        (eca-chat--refresh-code-copy-scopes (point-min) (point-max))
        (goto-char (point-min))
        (search-forward "az login")
        (eca-chat-copy-at-point)
        (expect (current-kill 0 t) :to-equal "az login"))))

  (it "copies the whole assistant response"
    (let (kill-ring
          kill-ring-yank-pointer)
      (with-temp-buffer
        (setq major-mode 'eca-chat-mode)
        (insert "Answer\n")
        (eca-chat--refresh-response-copy-scope (point-min) (point-max))
        (goto-char (point-min))
        (eca-chat-copy-at-point)
        (expect (current-kill 0 t) :to-equal "Answer"))))

  (it "does not insert visible copy controls"
    (with-temp-buffer
      (setq major-mode 'eca-chat-mode)
      (insert "Answer\n```elisp\n(+ 1 2)\n```\n")
      (let ((original (buffer-string)))
        (eca-chat--refresh-response-copy-scope (point-min) (point-max))
        (eca-chat--refresh-code-copy-scopes (point-min) (point-max))
        (expect (buffer-string) :to-equal original)
        (expect (-first (lambda (overlay)
                          (overlay-get overlay 'eca-chat--response-copy-scope))
                        (overlays-in (point-min) (point-max)))
                :not :to-be nil)
        (expect (-first (lambda (overlay)
                          (overlay-get overlay 'eca-chat--code-copy-scope))
                        (overlays-in (point-min) (point-max)))
                :not :to-be nil))))

  (it "adds copy scopes to each fenced code block"
    (let (kill-ring
          kill-ring-yank-pointer)
      (with-temp-buffer
        (setq major-mode 'eca-chat-mode)
        (insert "```bash\naz webapp list-runtimes --os linux -o table\n```\n\n")
        (insert "```bash\naz webapp show --name <APP_NAME> --resource-group <RG> ")
        (insert "--query \"siteConfig.linuxFxVersion\" -o tsv\n```\n")
        (eca-chat--refresh-code-copy-scopes (point-min) (point-max))
        (let ((overlays (seq-filter
                         (lambda (overlay)
                           (overlay-get overlay 'eca-chat--code-copy-scope))
                         (overlays-in (point-min) (point-max)))))
          (expect (length overlays) :to-be 2))
        (goto-char (point-min))
        (search-forward "az webapp list-runtimes")
        (eca-chat-copy-at-point)
        (search-forward "az webapp show")
        (eca-chat-copy-at-point)
        (expect (car kill-ring)
                :to-equal
                "az webapp show --name <APP_NAME> --resource-group <RG> --query \"siteConfig.linuxFxVersion\" -o tsv")
        (expect (cadr kill-ring)
                :to-equal
                "az webapp list-runtimes --os linux -o table"))))

  (it "does not add copy scopes to rendered user messages"
    (with-temp-buffer
      (setq major-mode 'eca-chat-mode)
      (insert "User says:\n```elisp\n(+ 1 2)\n```\n")
      (setq-local eca-chat--last-user-message-pos (point-max))
      (let ((ov (make-overlay (point-max) (point-max))))
        (overlay-put ov 'eca-chat-prompt-area t))
      (eca-chat--refresh-copy-scopes)
      (expect (-first (lambda (overlay)
                        (overlay-get overlay 'eca-chat--code-copy-scope))
                      (overlays-in (point-min) (point-max)))
              :to-be nil)
      (expect (-first (lambda (overlay)
                        (overlay-get overlay 'eca-chat--response-copy-scope))
                      (overlays-in (point-min) (point-max)))
              :to-be nil)))

  (it "copies only assistant text after a tool interruption"
    (let (kill-ring
          kill-ring-yank-pointer)
      (with-temp-buffer
        (setq major-mode 'eca-chat-mode)
        (insert "Intro\n")
        (eca-chat--mark-response-copy-break "toolCalled" nil)
        (let ((final-start (point)))
          (insert "Final answer\n")
          (let ((ov (make-overlay (point) (point))))
            (overlay-put ov 'eca-chat-prompt-area t))
          (setq-local eca-chat--last-response-copy-start final-start)
          (eca-chat--refresh-copy-scopes)
          (goto-char final-start)
          (eca-chat-copy-at-point)
          (expect (current-kill 0 t) :to-equal "Final answer")))))

  (it "copies response text including fenced code"
    (let (kill-ring
          kill-ring-yank-pointer)
      (with-temp-buffer
        (setq major-mode 'eca-chat-mode)
        (insert "Answer\n```elisp\n(+ 1 2)\n```\n")
        (let ((ov (make-overlay (point) (point))))
          (overlay-put ov 'eca-chat-prompt-area t))
        (setq-local eca-chat--last-response-copy-start (point-min))
        (eca-chat--refresh-copy-scopes)
        (goto-char (point-min))
        (eca-chat-copy-at-point)
        (expect (current-kill 0 t)
                :to-equal "Answer\n```elisp\n(+ 1 2)\n```"))))

  (it "prefers code block scopes over response scopes"
    (let (kill-ring
          kill-ring-yank-pointer)
      (with-temp-buffer
        (setq major-mode 'eca-chat-mode)
        (insert "Answer\n```elisp\n(+ 1 2)\n```\n")
        (let ((ov (make-overlay (point) (point))))
          (overlay-put ov 'eca-chat-prompt-area t))
        (setq-local eca-chat--last-response-copy-start (point-min))
        (eca-chat--refresh-copy-scopes)
        (goto-char (point-min))
        (search-forward "(+ 1 2)")
        (eca-chat-copy-at-point)
        (expect (current-kill 0 t) :to-equal "(+ 1 2)"))))

  (it "falls back to the latest response"
    (let (kill-ring
          kill-ring-yank-pointer)
      (with-temp-buffer
        (setq major-mode 'eca-chat-mode)
        (insert "Previous assistant response\n")
        (insert "User question that should not be copied\n")
        (setq-local eca-chat--last-response-copy-start nil)
        (let ((latest-start (point)))
          (insert "Latest answer only\n")
          (let ((ov (make-overlay (point) (point))))
            (overlay-put ov 'eca-chat-prompt-area t))
          (setq-local eca-chat--last-response-copy-start latest-start)
          (eca-chat--refresh-copy-scopes)
          (goto-char (point-min))
          (eca-chat-copy-at-point)
          (expect (current-kill 0 t)
                  :to-equal "Latest answer only")))))

  (it "keeps an older response scoped to that response"
    (let (kill-ring
          kill-ring-yank-pointer)
      (with-temp-buffer
        (setq major-mode 'eca-chat-mode)
        (insert "Previous assistant response\n")
        (let ((ov (make-overlay (point) (point))))
          (overlay-put ov 'eca-chat-prompt-area t))
        (setq-local eca-chat--last-response-copy-start (point-min))
        (eca-chat--refresh-copy-scopes)
        (save-excursion
          (goto-char (eca-chat--content-insertion-point))
          (insert "User question that should not be copied\n")
          (insert "Latest answer only\n"))
        (goto-char (point-min))
        (eca-chat-copy-at-point)
        (expect (current-kill 0 t)
                :to-equal "Previous assistant response")))))

(describe "eca-chat--font-lock-ensure"
  (it "fontifies and requests a redisplay update"
    (with-temp-buffer
      (spy-on 'font-lock-ensure :and-return-value 'fontified)
      (spy-on 'force-window-update)
      (expect (eca-chat--font-lock-ensure 1 1) :to-equal 'fontified)
      (expect 'font-lock-ensure :to-have-been-called-with 1 1)
      (expect 'force-window-update
              :to-have-been-called-with (current-buffer)))))

(describe "eca-chat--schedule-fontify"
  (it "uses stable fontification from the current turn"
    (with-temp-buffer
      (insert "abc")
      (setq-local eca-chat--last-user-message-pos 2)
      (setq-local eca-chat-fontify-debounce-interval 0.15)
      (let (scheduled)
        (cl-letf (((symbol-function 'run-with-idle-timer)
                   (lambda (_secs _repeat fn &rest args)
                     (setq scheduled (lambda () (apply fn args)))
                     'test-timer)))
          (spy-on 'eca-chat--font-lock-ensure)
          (eca-chat--schedule-fontify)
          (expect scheduled :to-be-truthy)
          (funcall scheduled)
          (expect 'eca-chat--font-lock-ensure
                  :to-have-been-called-with 2 (point-max))
          (expect eca-chat--fontify-timer :to-be nil))))))

(describe "eca-chat--render-content"
  (describe "progress finished"
    (it "clears progress text and spinner when chat-loading is nil"
      ;; Regression: a `progress' / `finished' notification that arrives
      ;; while `eca-chat--chat-loading' is nil (e.g. after the 10s
      ;; stopping safety-timer reset the flag, or for server-driven
      ;; progress not initiated by `eca-chat--send-prompt') must still
      ;; clear the visible spinner and progress text.  Previously the
      ;; wildcard `pcase' arm silently dropped the event.
      (let ((buf (generate-new-buffer " *test-chat-progress*"))
            (session (make-eca--session)))
        (unwind-protect
            (with-current-buffer buf
              (setq-local eca-chat--progress-text "thinking...")
              (setq-local eca-chat--chat-loading nil)
              (setq-local eca-chat--spinner-timer
                          (run-with-timer 100 100 #'ignore))
              (spy-on 'eca-chat--refresh-progress)
              (spy-on 'eca-chat--tool-call-elapsed-stop-all)
              (eca-chat--render-content
               session buf "system"
               (list :type "progress" :state "finished")
               nil)
              (expect eca-chat--progress-text :to-equal "")
              (expect eca-chat--spinner-timer :to-be nil)
              (expect 'eca-chat--refresh-progress :to-have-been-called)
              (expect 'eca-chat--tool-call-elapsed-stop-all
                      :to-have-been-called))
          (when (timerp eca-chat--spinner-timer)
            (cancel-timer eca-chat--spinner-timer))
          (kill-buffer buf))))

    (it "uses stable fontification for normal completion"
      (let ((buf (eca-chat-test--make-prompt-buffer ""))
            (session (make-eca--session)))
        (unwind-protect
            (with-current-buffer buf
              (setq-local eca-chat--progress-text "thinking...")
              (setq-local eca-chat--chat-loading t)
              (setq-local eca-chat--last-user-message-pos (point-min))
              (spy-on 'eca-chat--font-lock-ensure)
              (spy-on 'eca-chat--align-tables)
              (spy-on 'eca-chat--beautify-tables)
              (spy-on 'eca-chat--refresh-progress)
              (spy-on 'eca-chat--set-chat-loading)
              (spy-on 'eca-chat--send-steered-prompt)
              (spy-on 'eca-chat--send-queued-prompt)
              (eca-chat--render-content
               session buf "system"
               (list :type "progress" :state "finished")
               nil)
              (expect 'eca-chat--font-lock-ensure
                      :to-have-been-called-with (point-min) (point-max))
              (expect 'eca-chat--align-tables :to-have-been-called)
              (expect 'eca-chat--beautify-tables :to-have-been-called))
          (kill-buffer buf))))

    (it "does not error when the prompt area overlay is missing"
      ;; Regression for #283: a `chat/statusChanged' idle notification
      ;; synthesizes a `progress' / `finished' event; when the buffer
      ;; lost its prompt-block overlays, `eca-chat--add-text-content',
      ;; `eca-chat--align-tables', `eca-chat--beautify-tables' and
      ;; `eca-chat--refresh-progress' all crashed on a nil position or
      ;; overlay.  They run unspied here on a bare buffer.
      (let ((buf (generate-new-buffer " *test-chat-progress*"))
            (session (make-eca--session)))
        (unwind-protect
            (with-current-buffer buf
              (insert "content")
              (setq-local eca-chat--progress-text "thinking...")
              (setq-local eca-chat--chat-loading t)
              (spy-on 'eca-chat--font-lock-ensure)
              (spy-on 'eca-chat--set-chat-loading)
              (spy-on 'eca-chat--send-steered-prompt)
              (spy-on 'eca-chat--send-queued-prompt)
              ;; Built outside `expect', see eca-chat--raw-prompt-contexts
              ;; test above.
              (let ((content (list :type "progress" :state "finished")))
                (expect
                 (eca-chat--render-content session buf "system" content nil)
                 :not :to-throw))
              ;; The trailing turn-end newline was appended at the
              ;; `point-max' fallback insertion point.
              (expect (buffer-string) :to-equal "content\n"))
          (kill-buffer buf))))))

(describe "eca-chat-content-received"
  ;; Regression: streaming content into the chat must not move the user's
  ;; cursor.  The render runs from the async process filter, so without a
  ;; `save-excursion' guard a block/label rewrite above the prompt would drag
  ;; point up (the "cursor bounces to the middle while thinking" bug).
  (it "keeps point where the user left it while streaming"
    (let ((buf (eca-chat-test--make-prompt-buffer "hi"))
          (session (make-eca--session)))
      (unwind-protect
          (with-current-buffer buf
            (setq-local eca-chat--last-user-message-pos nil)
            (spy-on 'eca-chat--get-chat-buffer :and-return-value buf)
            (spy-on 'eca--session-workspace-folders :and-return-value nil)
            (spy-on 'eca-chat--protect-non-prompt)
            (spy-on 'eca-chat--maybe-notify-status-changed)
            ;; Simulate a streamed render that moves point up into the
            ;; history, like a mid-stream block/label rewrite does.
            (spy-on 'eca-chat--render-content :and-call-fake
                    (lambda (&rest _) (goto-char (point-min))))
            (goto-char (point-max))
            (let ((before (point)))
              (eca-chat-content-received
               session (list :chatId "chat-1" :role "assistant"
                             :content (list :type "text" :text "x")))
              (expect (point) :to-equal before)))
        (kill-buffer buf)))))

(describe "eca-chat--transient-segment-loading"
  (it "shows the stop button while a question is pending even when idle"
    ;; A pending question keeps the turn active server-side, so the stop
    ;; affordance must stay available even though `eca-chat--chat-loading'
    ;; is nil (the chat reports idle while awaiting the answer).
    (with-temp-buffer
      (setq-local eca-chat--chat-loading nil)
      (setq-local eca-chat--pending-question
                  (list :session (make-eca--session) :request 1))
      (let ((seg (eca-chat--transient-segment-loading)))
        (expect seg :to-be-truthy)
        (expect (string-match-p "stop" seg) :to-be-truthy))))

  (it "returns nil when idle and no question is pending"
    (with-temp-buffer
      (setq-local eca-chat--chat-loading nil)
      (setq-local eca-chat--pending-question nil)
      (expect (eca-chat--transient-segment-loading) :to-be nil))))

(describe "eca-chat--stop-prompt"
  (it "cancels the pending question and notifies the server when idle"
    ;; Regression: stopping must work while a question is pending even if
    ;; `eca-chat--chat-loading' is nil, since the question blocks the turn.
    (with-temp-buffer
      (let ((session (make-eca--session)))
        (setq-local eca-chat--id "chat-1")
        (setq-local eca-chat--chat-loading nil)
        (setq-local eca-chat--pending-question
                    (list :session session :request 1))
        (spy-on 'eca-chat--cancel-question)
        (spy-on 'eca-api-notify)
        (spy-on 'eca-chat--set-chat-loading)
        (eca-chat--stop-prompt session)
        (expect 'eca-chat--cancel-question :to-have-been-called)
        (expect 'eca-api-notify :to-have-been-called-with
                session
                :method "chat/promptStop"
                :params (list :chatId "chat-1"))
        (expect 'eca-chat--set-chat-loading :to-have-been-called-with
                session 'stopping)))))

(describe "eca-chat--dismiss-pending-question-for-tool-call"
  ;; When another client answers/cancels the same `ask_user' question first,
  ;; the server resolves the tool call and we receive a `toolCalled' /
  ;; `toolCallRejected' for that id. This client must then drop its now-stale
  ;; pending-question state so the prompt leaves answer mode.
  (it "clears the pending question when the tool-call id matches"
    (with-temp-buffer
      (setq-local eca-chat--pending-question
                  (list :session (make-eca--session) :request 1
                        :tool-call-id "tc-1" :allow-freeform t))
      (spy-on 'eca-chat--set-question-prompt-prefix)
      (spy-on 'eca-chat--refresh-transient-area)
      (eca-chat--dismiss-pending-question-for-tool-call "tc-1")
      (expect eca-chat--pending-question :to-be nil)
      (expect 'eca-chat--set-question-prompt-prefix
              :to-have-been-called-with nil)
      (expect 'eca-chat--refresh-transient-area :to-have-been-called)))

  (it "leaves the pending question intact when the id does not match"
    (with-temp-buffer
      (let ((pending (list :session (make-eca--session) :request 1
                           :tool-call-id "tc-1" :allow-freeform t)))
        (setq-local eca-chat--pending-question pending)
        (spy-on 'eca-chat--set-question-prompt-prefix)
        (spy-on 'eca-chat--refresh-transient-area)
        (eca-chat--dismiss-pending-question-for-tool-call "tc-2")
        (expect eca-chat--pending-question :to-equal pending)
        (expect 'eca-chat--set-question-prompt-prefix
                :not :to-have-been-called))))

  (it "does nothing when there is no pending question"
    (with-temp-buffer
      (setq-local eca-chat--pending-question nil)
      (spy-on 'eca-chat--refresh-transient-area)
      (eca-chat--dismiss-pending-question-for-tool-call "tc-1")
      (expect eca-chat--pending-question :to-be nil)
      (expect 'eca-chat--refresh-transient-area :not :to-have-been-called))))

(describe "eca-chat--normalize-question-option"
  ;; Regression: a `chat/askQuestion' option that is a plain string or a
  ;; plist without `:label' must not crash rendering with
  ;; `(wrong-type-argument stringp nil)'.
  (it "returns label and description from a plist option"
    (expect (eca-chat--normalize-question-option '(:label "Yes" :description "do it"))
            :to-equal '("Yes" . "do it")))

  (it "treats a plain string as the label with no description"
    (expect (eca-chat--normalize-question-option "Yes")
            :to-equal '("Yes" . nil)))

  (it "always returns a non-nil string label when :label is missing"
    (let ((res (eca-chat--normalize-question-option '(:description "no label"))))
      (expect (stringp (car res)) :to-be-truthy)
      (expect (cdr res) :to-equal "no label"))))

(describe "eca-chat--face-at-point-member-p"
  ;; A bare URL wrapped in emphasis (**url**, _url_) gets a list-valued
  ;; `face' property, so detection must not rely on `eq' to a symbol.
  (it "matches when the face is a single symbol"
    (with-temp-buffer
      (insert (propertize "x" 'face 'markdown-plain-url-face))
      (goto-char (point-min))
      (expect (eca-chat--face-at-point-member-p '(markdown-plain-url-face))
              :to-be-truthy)))

  (it "matches when the face is a list (emphasized URL)"
    (with-temp-buffer
      (insert (propertize "x" 'face '(markdown-plain-url-face markdown-bold-face)))
      (goto-char (point-min))
      (expect (eca-chat--face-at-point-member-p '(markdown-plain-url-face))
              :to-be-truthy)))

  (it "returns nil when no listed face is present"
    (with-temp-buffer
      (insert (propertize "x" 'face 'font-lock-comment-face))
      (goto-char (point-min))
      (expect (eca-chat--face-at-point-member-p '(markdown-plain-url-face))
              :to-be nil)))

  (it "returns nil when there is no face at point"
    (with-temp-buffer
      (insert "x")
      (goto-char (point-min))
      (expect (eca-chat--face-at-point-member-p '(markdown-plain-url-face))
              :to-be nil))))

(describe "eca-chat--follow-link-at-point"

  (it "opens a bare URL"
    (eca-chat-test--call-on
     "see https://github.com/nubank/nucli/pull/10063 here" "github.com"
     (lambda ()
       (spy-on 'browse-url)
       (spy-on 'markdown-follow-thing-at-point)
       (eca-chat--follow-link-at-point)
       (expect 'browse-url :to-have-been-called-with
               "https://github.com/nubank/nucli/pull/10063")
       (expect 'markdown-follow-thing-at-point :not :to-have-been-called))))

  (it "opens a bold URL without the trailing ** markers"
    ;; Regression: **https://...** fontifies the URL with a list face and
    ;; `thing-at-point' captures the trailing "**"; both used to break RET.
    (eca-chat-test--call-on
     "see **https://github.com/nubank/nucli/pull/10063** here" "github.com"
     (lambda ()
       (spy-on 'browse-url)
       (spy-on 'markdown-follow-thing-at-point)
       (eca-chat--follow-link-at-point)
       (expect 'browse-url :to-have-been-called-with
               "https://github.com/nubank/nucli/pull/10063")
       (expect 'markdown-follow-thing-at-point :not :to-have-been-called))))

  (it "opens an italic URL without the trailing _ marker"
    (eca-chat-test--call-on
     "see _https://github.com/nubank/nucli/pull/10063_ here" "github.com"
     (lambda ()
       (spy-on 'browse-url)
       (eca-chat--follow-link-at-point)
       (expect 'browse-url :to-have-been-called-with
               "https://github.com/nubank/nucli/pull/10063"))))

  (it "defers a proper [text](url) link to markdown-follow-thing-at-point"
    (eca-chat-test--call-on
     "see [PR](https://github.com/nubank/nucli/pull/10063) here" "github.com"
     (lambda ()
       (spy-on 'browse-url)
       (spy-on 'markdown-follow-thing-at-point)
       (eca-chat--follow-link-at-point)
       (expect 'markdown-follow-thing-at-point :to-have-been-called)
       (expect 'browse-url :not :to-have-been-called)))))

(describe "eca-chat--apply-history-meta"
  (it "sets buffer-local pagination cursors from a meta plist"
    (with-temp-buffer
      (eca-chat--apply-history-meta
       '(:total 412 :returned 50 :beforeCursor "b" :afterCursor "a" :compactionCursor "c"))
      (expect eca-chat--history-total :to-equal 412)
      (expect eca-chat--history-before-cursor :to-equal "b")
      (expect eca-chat--history-after-cursor :to-equal "a")
      (expect eca-chat--history-compaction-cursor :to-equal "c")))

  (it "stores nil cursors at the ends (nil-punning)"
    (with-temp-buffer
      (eca-chat--apply-history-meta
       '(:total 3 :returned 3 :beforeCursor nil :afterCursor nil :compactionCursor nil))
      (expect eca-chat--history-before-cursor :to-be nil)
      (expect eca-chat--history-after-cursor :to-be nil))))

(describe "eca-chat--refresh-load-older-control"
  (it "inserts the control at the top when an older page is available"
    (let ((buf (eca-chat-test--make-prompt-buffer "hi")))
      (unwind-protect
          (with-current-buffer buf
            (setq-local eca-chat--history-before-cursor "cursor")
            (eca-chat--refresh-load-older-control)
            (expect (eca-chat--load-older-control-region) :not :to-be nil)
            (expect (buffer-substring-no-properties
                     (point-min) (cdr (eca-chat--load-older-control-region)))
                    :to-match "Load older messages"))
        (kill-buffer buf))))

  (it "removes the control when there is no older page"
    (let ((buf (eca-chat-test--make-prompt-buffer "hi")))
      (unwind-protect
          (with-current-buffer buf
            (setq-local eca-chat--history-before-cursor "cursor")
            (eca-chat--refresh-load-older-control)
            (setq-local eca-chat--history-before-cursor nil)
            (eca-chat--refresh-load-older-control)
            (expect (eca-chat--load-older-control-region) :to-be nil))
        (kill-buffer buf)))))

(describe "eca-chat--content-insertion-point"
  (it "returns the override marker position when bound"
    (let ((buf (eca-chat-test--make-prompt-buffer "hi")))
      (unwind-protect
          (with-current-buffer buf
            (let ((eca-chat--insertion-point-override (copy-marker (point-min))))
              (expect (eca-chat--content-insertion-point) :to-equal (point-min))))
        (kill-buffer buf))))

  (it "falls back to just before the prompt area when override is nil"
    (let ((buf (eca-chat-test--make-prompt-buffer "hi")))
      (unwind-protect
          (with-current-buffer buf
            (expect (eca-chat--content-insertion-point)
                    :to-equal (1- (eca-chat--prompt-area-start-point))))
        (kill-buffer buf))))

  (it "falls back to point-max when the prompt area overlay is missing"
    ;; Regression for #283: `(1- nil)' signaled wrong-type-argument when
    ;; the buffer lost its prompt-area overlay (inconsistent state).
    (with-temp-buffer
      (insert "content")
      (expect (eca-chat--content-insertion-point) :to-equal (point-max)))))

(describe "eca-chat--render-history-contents"
  ;; A plain buffer is enough: the insertion-point override short-circuits the
  ;; prompt-area layout, and the table/fontify helpers are stubbed.
  (it "prepends items in chronological order, separated from existing content"
    (with-temp-buffer
      (insert "EXISTING")
      (let ((session (make-eca--session)))
        (spy-on 'eca--session-workspace-folders :and-return-value nil)
        (spy-on 'font-lock-ensure)
        (spy-on 'eca-chat--align-tables)
        (spy-on 'eca-chat--beautify-tables)
        ;; Stub the renderer to insert the item text at the (overridable)
        ;; insertion point, mirroring how the real renderer appends text.
        (spy-on 'eca-chat--render-content :and-call-fake
                (lambda (_session _buf _role content _roots &rest _)
                  (goto-char (eca-chat--content-insertion-point))
                  (insert (plist-get content :text))))
        (eca-chat--render-history-contents
         session (current-buffer)
         (list '(:role "user" :content (:type "text" :text "m0"))
               '(:role "assistant" :content (:type "text" :text "m1"))))
        ;; Older page on top, in order, with a single separating newline so the
        ;; last older line is not glued to the first existing line.
        (expect (buffer-string) :to-equal "m0m1\nEXISTING"))))

  (it "does not add a separator when the older page already ends with a newline"
    (with-temp-buffer
      (insert "EXISTING")
      (let ((session (make-eca--session)))
        (spy-on 'eca--session-workspace-folders :and-return-value nil)
        (spy-on 'font-lock-ensure)
        (spy-on 'eca-chat--align-tables)
        (spy-on 'eca-chat--beautify-tables)
        (spy-on 'eca-chat--render-content :and-call-fake
                (lambda (_session _buf _role content _roots &rest _)
                  (goto-char (eca-chat--content-insertion-point))
                  (insert (plist-get content :text))))
        (eca-chat--render-history-contents
         session (current-buffer)
         (list '(:role "assistant" :content (:type "text" :text "m0\n"))))
        (expect (buffer-string) :to-equal "m0\nEXISTING"))))

  (it "restores eca-chat--last-user-message-pos after prepending"
    (with-temp-buffer
      (insert "EXISTING")
      (setq-local eca-chat--last-user-message-pos 42)
      (let ((session (make-eca--session)))
        (spy-on 'eca--session-workspace-folders :and-return-value nil)
        (spy-on 'font-lock-ensure)
        (spy-on 'eca-chat--align-tables)
        (spy-on 'eca-chat--beautify-tables)
        (spy-on 'eca-chat--render-content :and-call-fake
                (lambda (_session _buf _role _content _roots &rest _)
                  (setq-local eca-chat--last-user-message-pos 999)))
        (eca-chat--render-history-contents
         session (current-buffer) (list '(:role "user" :content (:type "text" :text "m0"))))
        (expect eca-chat--last-user-message-pos :to-equal 42)))))

(describe "eca-chat--render-content user message scrolling"
  ;; Issue #279: after sending a long message the prompt is pushed
  ;; below the window end, so the scroll must be forced.
  (it "forces the prompt visible after rendering a sent user message"
    (let ((buf (eca-chat-test--make-prompt-buffer "hi")))
      (unwind-protect
          (with-current-buffer buf
            (let ((session (make-eca--session)))
              (spy-on 'font-lock-ensure)
              (spy-on 'eca-chat--ensure-prompt-visible)
              (eca-chat--render-content
               session buf "user"
               '(:type "text" :text "a sent message" :contentId "issue-279-live")
               nil)
              (expect 'eca-chat--ensure-prompt-visible
                      :to-have-been-called-with t)))
        (kill-buffer buf))))

  (it "does not scroll when prepending an older history page"
    (let ((buf (eca-chat-test--make-prompt-buffer "hi")))
      (unwind-protect
          (with-current-buffer buf
            (let ((session (make-eca--session))
                  (eca-chat--insertion-point-override (copy-marker (point-min) t)))
              (spy-on 'font-lock-ensure)
              (spy-on 'eca-chat--ensure-prompt-visible)
              (eca-chat--render-content
               session buf "user"
               '(:type "text" :text "an old message" :contentId "issue-279-prepend")
               nil)
              (expect 'eca-chat--ensure-prompt-visible
                      :not :to-have-been-called)))
        (kill-buffer buf)))))

(describe "eca-chat-opened"
  ;; Regression: resuming after a restart must not replay into a stale
  ;; closed buffer left in the registry by `eca-chat-exit'.
  (it "creates a fresh buffer when the registered chat buffer is closed"
    (spy-on 'eca-chat--force-tab-line-update)
    (let* ((session (make-eca--session))
           (closed-buf (generate-new-buffer " *test-closed-chat*")))
      (unwind-protect
          (progn
            (with-current-buffer closed-buf
              (setq major-mode 'eca-chat-mode)
              (setq-local eca-chat--id "chat-AAA")
              (setq-local eca-chat--closed t))
            (setf (eca--session-chats session)
                  (eca-assoc (eca--session-chats session) "chat-AAA" closed-buf))
            (eca-chat-opened session (list :chatId "chat-AAA" :title "My chat"))
            (let ((registered (eca-get (eca--session-chats session) "chat-AAA")))
              ;; The registry now points at a brand new, live, writable buffer.
              (expect (buffer-live-p registered) :to-be-truthy)
              (expect registered :not :to-be closed-buf)
              (expect (buffer-local-value 'eca-chat--closed registered) :to-be nil)
              (expect (buffer-local-value 'eca-chat--id registered)
                      :to-equal "chat-AAA")
              ;; The stale closed buffer is cleaned up, not left lingering.
              (expect (buffer-live-p closed-buf) :to-be nil)
              (when (and (buffer-live-p registered)
                         (not (eq registered closed-buf)))
                (kill-buffer registered))))
        (when (buffer-live-p closed-buf)
          (kill-buffer closed-buf)))))

  (it "reuses the existing buffer when it is live and not closed"
    (spy-on 'eca-chat--force-tab-line-update)
    (let* ((session (make-eca--session))
           (live-buf (generate-new-buffer " *test-open-chat*")))
      (unwind-protect
          (progn
            (with-current-buffer live-buf
              (setq major-mode 'eca-chat-mode)
              (setq-local eca-chat--id "chat-BBB")
              (setq-local eca-chat--closed nil)
              (setq-local eca-chat--title "old title"))
            (setf (eca--session-chats session)
                  (eca-assoc (eca--session-chats session) "chat-BBB" live-buf))
            (eca-chat-opened session (list :chatId "chat-BBB" :title "new title"))
            (let ((registered (eca-get (eca--session-chats session) "chat-BBB")))
              ;; No duplicate buffer; the title is propagated in place.
              (expect registered :to-be live-buf)
              (expect (buffer-local-value 'eca-chat--title live-buf)
                      :to-equal "new title")))
        (when (buffer-live-p live-buf)
          (kill-buffer live-buf))))))

(describe "eca-chat--context-category-face"
  (it "maps known categories to their faces"
    (expect (eca-chat--context-category-face "System prompt")
            :to-be 'eca-chat-context-system-prompt-face)
    (expect (eca-chat--context-category-face "Rules")
            :to-be 'eca-chat-context-rules-face)
    (expect (eca-chat--context-category-face "Skills")
            :to-be 'eca-chat-context-skills-face)
    (expect (eca-chat--context-category-face "AGENTS.md")
            :to-be 'eca-chat-context-agents-face)
    (expect (eca-chat--context-category-face "Tool definitions")
            :to-be 'eca-chat-context-tool-definitions-face)
    (expect (eca-chat--context-category-face "Tool calls")
            :to-be 'eca-chat-context-tool-calls-face)
    (expect (eca-chat--context-category-face "Conversation")
            :to-be 'eca-chat-context-conversation-face))
  (it "falls back to the conversation face for unknown categories"
    (expect (eca-chat--context-category-face "Something else")
            :to-be 'eca-chat-context-conversation-face)))

(describe "eca-chat--context-bar-allocate"
  (it "gives the single cell to the largest category when tight"
    (let ((alloc (eca-chat--context-bar-allocate '(50 50) 1)))
      (expect (apply #'+ alloc) :to-equal 1)
      (expect (length alloc) :to-equal 2)))
  (it "guarantees one cell per positive category then shares the rest"
    (expect (eca-chat--context-bar-allocate '(80 20) 10) :to-equal '(7 3)))
  (it "puts every cell in the single category"
    (expect (eca-chat--context-bar-allocate '(100) 8) :to-equal '(8)))
  (it "returns all zeros when there are no tokens"
    (expect (eca-chat--context-bar-allocate '(0 0) 5) :to-equal '(0 0)))
  (it "covers the largest categories when cells < categories"
    (let ((alloc (eca-chat--context-bar-allocate '(50 50 50) 2)))
      (expect (apply #'+ alloc) :to-equal 2)
      (expect (seq-count (lambda (n) (> n 0)) alloc) :to-equal 2))))

(describe "eca-chat--context-bar"
  (it "returns nil when there is no breakdown"
    (let ((eca-chat--context-breakdown nil))
      (expect (eca-chat--context-bar) :to-be nil)))

  (it "returns nil when the breakdown has no categories"
    (let ((eca-chat--context-breakdown (list :categories [] :usedTokens 0)))
      (expect (eca-chat--context-bar) :to-be nil)))

  (it "renders a bar that totals the configured width, with no label"
    (let ((eca-chat-context-bar-width 10)
          (eca-chat--context-breakdown
           (list :categories (vector (list :name "System prompt" :tokens 50)
                                     (list :name "Conversation" :tokens 50))
                 :usedTokens 100
                 :freeTokens 900
                 :contextLimit 1000)))
      (let ((bar (eca-chat--context-bar)))
        (expect (stringp bar) :to-be t)
        ;; colored + edge + free cells always total the configured width
        (expect (length bar) :to-equal 10)
        ;; the visible bar carries no percentage/number, only blocks
        (expect (string-match-p "[0-9%]" bar) :to-be nil))))

  (it "uses a fractional block at the used/free edge for sub-cell precision"
    (let ((eca-chat-context-bar-width 10)
          (eca-chat--context-breakdown
           (list :categories (vector (list :name "System prompt" :tokens 250))
                 :usedTokens 250 :freeTokens 750 :contextLimit 1000)))
      (let ((bar (eca-chat--context-bar)))
        (expect (length bar) :to-equal 10)
        ;; 25% of 10 cells = 2.5 -> 2 full cells plus a half block
        (expect (string-match-p "[▏▎▍▌▋▊▉]" bar) :to-be-truthy))))

  (it "renders a full-width bar when the context window is unknown"
    (let ((eca-chat-context-bar-width 8)
          (eca-chat--context-breakdown
           (list :categories (vector (list :name "Conversation" :tokens 4000))
                 :usedTokens 4000)))
      (let ((bar (eca-chat--context-bar)))
        (expect (length bar) :to-equal 8)
        (expect (string-match-p "[0-9%]" bar) :to-be nil))))

  (it "colors segments with the server-provided color"
    (let ((eca-chat-context-bar-width 4)
          (eca-chat--context-breakdown
           (list :categories (vector (list :name "System prompt" :tokens 100 :color "#ff0000"))
                 :usedTokens 100 :freeTokens 0 :contextLimit 100 :freeColor "#222222")))
      (let ((bar (eca-chat--context-bar)))
        (expect (length bar) :to-equal 4)
        (expect (get-text-property 0 'face bar) :to-equal '(:foreground "#ff0000"))))))

(describe "eca-chat--context-bar-help"
  (it "lists categories with server emoji swatches, free space and the hint"
    (let* ((breakdown (list :categories (vector (list :name "System prompt" :tokens 5300 :emoji "🟦")
                                                (list :name "Conversation" :tokens 1600 :emoji "🟩"))
                            :usedTokens 6900 :freeTokens 193100 :freeEmoji "⬜" :contextLimit 200000))
           (help (eca-chat--context-bar-help breakdown 6900 193100 200000)))
      (expect help :to-match "System prompt")
      (expect help :to-match "Conversation")
      (expect help :to-match "Free space")
      (expect help :to-match "/context")
      ;; server emoji swatches correlate colors to categories
      (expect help :to-match "🟦")
      (expect help :to-match "🟩")
      (expect help :to-match "⬜")))

  (it "falls back to a colored block swatch when no emoji is provided"
    (let* ((breakdown (list :categories (vector (list :name "System prompt" :tokens 5300))
                            :usedTokens 5300 :freeTokens 194700 :contextLimit 200000))
           (help (eca-chat--context-bar-help breakdown 5300 194700 200000)))
      (expect help :to-match "█"))))

(describe "eca-chat--context-category-color"
  (it "prefers the server-provided color"
    (expect (eca-chat--context-category-color (list :name "Rules" :color "#abcdef"))
            :to-equal "#abcdef"))
  (it "falls back to a string color when the server sent none"
    (expect (stringp (eca-chat--context-category-color (list :name "Conversation")))
            :to-be t)))

(describe "eca-chat--context-bar-pixels"
  (it "renders pixel-width background-colored segments"
    (let ((bar (eca-chat--context-bar-pixels
                (list (list :name "System prompt" :tokens 100 :color "#ff0000"))
                (list :freeColor "#222222")
                16 0.5)))
      (expect (stringp bar) :to-be t)
      ;; the first segment is a pixel-width space colored via :background
      (expect (car (get-text-property 0 'display bar)) :to-be 'space)
      (expect (get-text-property 0 'face bar) :to-equal '(:background "#ff0000")))))

(describe "eca-chat--string-pixel-width"
  (it "returns 0 for the empty string"
    (expect (eca-chat--string-pixel-width "") :to-equal 0))

  (it "honors pixel-width display specs instead of counting chars"
    (unless (fboundp 'string-pixel-width)
      (buttercup-skip "string-pixel-width not available"))
    ;; A single space carrying a 40px display width must measure ~40, not
    ;; 1 (its `length').  Counting it as 1 char is what pushed the :usage
    ;; and :trust mode-line segments off the right edge once the context
    ;; bar started emitting pixel-width spaces.
    (let ((s (propertize " " 'display (list 'space :width (list 40)))))
      (expect (length s) :to-equal 1)
      (expect (eca-chat--string-pixel-width s) :to-equal 40)))

  (it "measures a pixel context-bar wider than its char length"
    (unless (fboundp 'string-pixel-width)
      (buttercup-skip "string-pixel-width not available"))
    (let ((bar (eca-chat--context-bar-pixels
                (list (list :name "System prompt" :tokens 100 :color "#ff0000"))
                (list :freeColor "#222222")
                16 0.5)))
      (expect (> (eca-chat--string-pixel-width bar) (length bar)) :to-be t))))

(describe "eca-chat context-bar compaction marker"
  (it "keeps the pixel-bar total width when the marker is overlaid"
    (unless (fboundp 'string-pixel-width)
      (buttercup-skip "string-pixel-width not available"))
    (let* ((cats (list (list :name "System prompt" :tokens 100 :color "#ff0000")))
           (bd (list :freeColor "#222222"))
           (plain (eca-chat--context-bar-pixels cats bd 16 0.5))
           (marked (eca-chat--context-bar-pixels cats bd 16 0.5 0.75)))
      (expect (eca-chat--string-pixel-width marked)
              :to-equal (eca-chat--string-pixel-width plain))))

  (it "draws the marker glyph on the terminal bar without changing length"
    (let* ((cats (list (list :name "System prompt" :tokens 100 :color "#ff0000")))
           (bd (list :freeColor "#222222"))
           (plain (eca-chat--context-bar-chars cats bd 8 1.0))
           (marked (eca-chat--context-bar-chars cats bd 8 1.0 0.5)))
      (expect (length marked) :to-equal (length plain))
      (expect (string-match-p "│" marked) :to-be-truthy)))

  (it "notes the auto-compaction threshold in the tooltip"
    (let* ((breakdown (list :categories (vector (list :name "System prompt" :tokens 5300 :emoji "🟦"))
                            :usedTokens 5300 :freeTokens 194700 :contextLimit 200000))
           (help (eca-chat--context-bar-help breakdown 5300 194700 200000 75)))
      (expect help :to-match "Auto-compaction at 75%"))))

;; ---------------------------------------------------------------------------
;; eca-chat--display-buffer
;; ---------------------------------------------------------------------------

(describe "eca-chat--display-buffer"

  (it "uses the selected window when no side is configured"
    (eca-chat-test--with-display-buffers (source chat)
      (let ((window (selected-window))
            (eca-chat-window-side nil)
            (eca-chat-use-side-window t)
            (eca-chat-focus-on-open t))
        (set-window-buffer window source)
        (expect (eca-chat--display-buffer chat) :to-be window)
        (expect (window-buffer window) :to-be chat)
        (expect (length (window-list)) :to-equal 1)
        (expect (window-dedicated-p window) :to-be nil)
        (expect (window-parameter window 'window-side) :to-be nil))))

  (it "overrides display rules that would create another window"
    (eca-chat-test--with-display-buffers (source chat)
      (let* ((window (selected-window))
             (display-buffer-alist
              `((,(regexp-quote (buffer-name chat))
                 (display-buffer-pop-up-window)
                 (inhibit-same-window . t))))
             (display-buffer-overriding-action
              '((display-buffer-pop-up-window)
                (inhibit-same-window . t)))
             (eca-chat-window-side nil)
             (eca-chat-focus-on-open nil))
        (set-window-buffer window source)
        (expect (eca-chat--display-buffer chat) :to-be window)
        (expect (window-buffer window) :to-be chat)
        (expect (length (window-list)) :to-equal 1))))

  (it "does not split when the selected window is dedicated"
    (eca-chat-test--with-display-buffers (source chat)
      (let ((window (selected-window))
            (eca-chat-window-side nil))
        (set-window-buffer window source)
        (set-window-dedicated-p window 'side)
        (unwind-protect
            (progn
              (expect (eca-chat--display-buffer chat)
                      :to-throw 'user-error)
              (expect (window-buffer window) :to-be source)
              (expect (length (window-list)) :to-equal 1))
          (set-window-dedicated-p window nil)))))

  (it "does not split when the selected window is a minibuffer"
    (eca-chat-test--with-display-buffers (source chat)
      (let ((window (selected-window))
            (eca-chat-window-side nil))
        (set-window-buffer window source)
        (spy-on 'window-minibuffer-p :and-return-value t)
        (expect (eca-chat--display-buffer chat) :to-throw 'user-error)
        (expect (window-buffer window) :to-be source)
        (expect (length (window-list)) :to-equal 1))))

  (it "opens a new chat in the selected window instead of another chat window"
    (eca-chat-test--with-display-buffers (source visible-chat new-chat)
      (let* ((selected (selected-window))
             (chat-window (split-window selected nil 'below))
             (eca-chat-window-side nil)
             (eca-chat-use-side-window nil)
             (eca-chat-focus-on-open t))
        (set-window-buffer selected source)
        (set-window-buffer chat-window visible-chat)
        (with-current-buffer visible-chat
          (setq-local eca-chat--id "visible-chat"))
        (select-window selected)
        (expect (eca-chat--display-buffer new-chat) :to-be selected)
        (expect (window-buffer selected) :to-be new-chat)
        (expect (window-buffer chat-window) :to-be visible-chat)
        (expect (length (window-list)) :to-equal 2))))

  (it "keeps an already visible chat in its existing window"
    (eca-chat-test--with-display-buffers (source chat)
      (let* ((selected (selected-window))
             (chat-window (split-window selected nil 'below))
             (eca-chat-window-side nil)
             (eca-chat-focus-on-open nil))
        (set-window-buffer selected source)
        (set-window-buffer chat-window chat)
        (select-window selected)
        (expect (eca-chat--display-buffer chat) :to-be chat-window)
        (expect (selected-window) :to-be selected)
        (setq eca-chat-focus-on-open t)
        (expect (eca-chat--display-buffer chat) :to-be chat-window)
        (expect (selected-window) :to-be chat-window))))

  (it "restores the previous buffer when the chat is toggled"
    (eca-chat-test--with-display-buffers (source chat)
      (let ((window (selected-window))
            (eca-chat-window-side nil)
            (eca-chat-focus-on-open t))
        (set-window-buffer window source)
        (spy-on 'eca-session :and-return-value 'session)
        (spy-on 'eca-assert-session-running)
        (spy-on 'eca-chat--get-last-buffer :and-return-value chat)
        (eca-chat--display-buffer chat)
        (expect (window-buffer window) :to-be chat)
        (eca-chat-toggle-window)
        (expect (window-buffer window) :to-be source)
        (eca-chat-toggle-window)
        (expect (window-buffer window) :to-be chat)
        (expect (length (window-list)) :to-equal 1))))

  (it "uses a dedicated side window when a side is configured"
    (eca-chat-test--with-display-buffers (source chat)
      (set-window-buffer (selected-window) source)
      (let ((eca-chat-window-side 'right)
            (eca-chat-use-side-window t)
            (eca-chat-focus-on-open nil))
        (let ((window (eca-chat--display-buffer chat)))
          (expect (window-buffer window) :to-be chat)
          (expect (window-parameter window 'window-side) :to-be 'right)
          (expect (window-dedicated-p window) :to-be 'side)))))

  (it "reuses the selected chat window when the chat is visible twice"
    (eca-chat-test--with-display-buffers (visible-chat new-chat)
      (let* ((selected (selected-window))
             (other-window (split-window selected nil 'below))
             (eca-chat-window-side 'right)
             (eca-chat-use-side-window t)
             (eca-chat-focus-on-open nil))
        (set-window-buffer selected visible-chat)
        (set-window-buffer other-window visible-chat)
        (with-current-buffer visible-chat
          (setq-local eca-chat--id "visible-chat"))
        (select-window selected)
        (expect (eca-chat--display-buffer new-chat) :to-be selected)
        (expect (selected-window) :to-be selected)
        (expect (window-buffer selected) :to-be new-chat)
        (expect (window-buffer other-window) :to-be visible-chat)
        (expect (length (window-list)) :to-equal 2))))

  (it "reuses another visible chat window in side-based modes"
    (eca-chat-test--with-display-buffers (source visible-chat new-chat)
      (let* ((selected (selected-window))
             (chat-window (split-window selected nil 'below))
             (eca-chat-window-side 'right)
             (eca-chat-use-side-window t)
             (eca-chat-focus-on-open nil))
        (set-window-buffer selected source)
        (set-window-buffer chat-window visible-chat)
        (with-current-buffer visible-chat
          (setq-local eca-chat--id "visible-chat"))
        (expect (eca-chat--display-buffer new-chat) :to-be chat-window)
        (expect (window-buffer chat-window) :to-be new-chat)
        (expect (length (window-list)) :to-equal 2))))

  (it "uses a regular directional window when side windows are disabled"
    (eca-chat-test--with-display-buffers (source chat)
      (let ((selected (selected-window))
            (eca-chat-window-side 'right)
            (eca-chat-use-side-window nil)
            (eca-chat-focus-on-open nil))
        (set-window-buffer selected source)
        (let ((window (eca-chat--display-buffer chat)))
          (expect window :not :to-be selected)
          (expect (window-buffer window) :to-be chat)
          (expect (window-parameter window 'window-side) :to-be nil)
          (expect (window-dedicated-p window) :to-be nil)
          (expect (length (window-list)) :to-equal 2))))))

;; ---------------------------------------------------------------------------
;; eca-chat--select-window
;; ---------------------------------------------------------------------------

(describe "eca-chat--select-window"

  (it "selects the window already showing the chat buffer"
    (let ((buf (generate-new-buffer " *test-chat-select-window*")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) buf)
            (with-current-buffer buf
              (eca-chat--select-window))
            (expect (window-buffer (selected-window)) :to-be buf))
        (kill-buffer buf))))

  (it "displays and selects the chat buffer when not visible (#266)"
    (let ((buf (generate-new-buffer " *test-chat-select-window*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (expect (get-buffer-window buf) :to-be nil)
            (with-current-buffer buf
              (eca-chat--select-window))
            (expect (window-buffer (selected-window)) :to-be buf))
        (progn
          (when-let* ((win (get-buffer-window buf)))
            (when (window-parent win)
              (delete-window win)))
          (kill-buffer buf))))))

(describe "eca-chat subagent elapsed time"
  (it "keeps elapsed tracking isolated when another chat finishes"
    (spy-on 'eca-chat--force-tab-line-update)
    (let ((session (make-eca--session))
          chat-a chat-b)
      (unwind-protect
          (progn
            (eca-chat-opened session '(:chatId "chat-A" :title "A"))
            (eca-chat-opened session '(:chatId "chat-B" :title "B"))
            (setq chat-a (eca-get (eca--session-chats session) "chat-A")
                  chat-b (eca-get (eca--session-chats session) "chat-B"))
            (with-current-buffer chat-a
              (puthash "subagent-tool-call" (current-time)
                       eca-chat--tool-call-elapsed-times))
            (with-current-buffer chat-b
              (eca-chat--tool-call-elapsed-stop-all))
            (with-current-buffer chat-a
              (expect (gethash "subagent-tool-call"
                               eca-chat--tool-call-elapsed-times)
                      :to-be-truthy)))
        (dolist (buffer (list chat-a chat-b))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (setq-local eca-chat--closed t))
            (kill-buffer buffer)))))))

;; ---------------------------------------------------------------------------
;; eca-chat-mode-map TAB bindings
;; ---------------------------------------------------------------------------

(describe "eca-chat-mode-map TAB bindings"

  (it "binds TAB to eca-chat--key-pressed-tab"
    (expect (lookup-key eca-chat-mode-map (kbd "TAB"))
            :to-be #'eca-chat--key-pressed-tab))

  ;; Binding raw <tab> would block the <tab> -> TAB key translation in
  ;; GUI frames and shadow completion UIs' TAB bindings (corfu-map).
  ;; lookup-key follows the parent map, so this also catches a future
  ;; <tab> binding inherited from markdown-mode-map.  See #281.
  (it "does not bind the raw <tab> event (#281)"
    (expect (lookup-key eca-chat-mode-map (kbd "<tab>")) :to-be nil)))

;; ---------------------------------------------------------------------------
;; eca-chat-mode-map RET bindings
;; ---------------------------------------------------------------------------

(describe "eca-chat-mode-map RET bindings"

  (it "binds RET to eca-chat--key-pressed-return"
    (expect (lookup-key eca-chat-mode-map (kbd "RET"))
            :to-be #'eca-chat--key-pressed-return))

  ;; Binding raw <return> would block the <return> -> RET key translation
  ;; in GUI frames, so transient/overriding keymaps binding only "RET"
  ;; (e.g. embark-file-map) would never receive Enter.  lookup-key follows
  ;; the parent map, so this also catches a future <return> binding
  ;; inherited from markdown-mode-map.
  (it "does not bind the raw <return> event"
    (expect (lookup-key eca-chat-mode-map (kbd "<return>")) :to-be nil))

  ;; Modified Enter chords are unaffected: they are distinct events with
  ;; no ASCII equivalent, so they must stay bound as function keys.
  (it "still binds the modified <return> chords"
    (expect (lookup-key eca-chat-mode-map (kbd "S-<return>"))
            :to-be #'eca-chat--key-pressed-newline)
    (expect (lookup-key eca-chat-mode-map (kbd "C-<return>"))
            :to-be #'eca-chat--key-pressed-queue)))

;; ---------------------------------------------------------------------------
;; Expandable block label keymap
;; ---------------------------------------------------------------------------

(defun eca-chat-test--expandable-label-keymap ()
  "Render an expandable block and return the label's `keymap' property."
  (with-temp-buffer
    (let ((eca-chat-expandable--id->ov (make-hash-table :test 'equal)))
      (eca-chat--insert-expandable-block "test-id" "Label" "content" "" "" ""))
    (get-text-property (point-min) 'keymap)))

(describe "expandable block label keymap"

  (it "binds TAB and RET to the toggle"
    (let ((km (eca-chat-test--expandable-label-keymap)))
      (expect (functionp (lookup-key km (kbd "TAB"))) :to-be t)
      (expect (functionp (lookup-key km (kbd "RET"))) :to-be t)))

  ;; The label keymap comes from a `keymap' text property, which outranks
  ;; the mode map, so binding the raw events here would block the
  ;; <tab> -> TAB and <return> -> RET translations while point is on a
  ;; label and shadow any layered keymap binding only TAB/RET.
  (it "does not bind the raw <tab> or <return> events"
    (let ((km (eca-chat-test--expandable-label-keymap)))
      (expect (lookup-key km (kbd "<tab>")) :to-be nil)
      (expect (lookup-key km (kbd "<return>")) :to-be nil))))

;; ---------------------------------------------------------------------------
;; eca-chat--shell-command-state-face
;; ---------------------------------------------------------------------------

(describe "eca-chat--shell-command-state-face"

  (it "uses the plain face for commands that always ask"
    (expect (eca-chat--shell-command-state-face '(:command "rm"))
            :to-be 'eca-chat-shell-command-face)
    (expect (eca-chat--shell-command-state-face '(:command "rm") t)
            :to-be 'eca-chat-shell-command-face))

  (it "uses the approved face for remembered commands"
    (expect (eca-chat--shell-command-state-face
             '(:command "ls" :approvalKey "ls" :remembered t))
            :to-be 'eca-chat-shell-command-remembered-face))

  (it "uses the pending face while waiting for manual approval"
    (expect (eca-chat--shell-command-state-face
             '(:command "ls" :approvalKey "ls"))
            :to-be 'eca-chat-shell-command-not-remembered-face))

  (it "uses the approved face when the tool call is already approved"
    (expect (eca-chat--shell-command-state-face
             '(:command "ls" :approvalKey "ls") t)
            :to-be 'eca-chat-shell-command-remembered-face))

  (it "propagates the approved state to breakdown lines"
    (let ((line (eca-chat--shell-command-breakdown-line
                 '(:command "ls" :args ["-la"] :approvalKey "ls") "$ " nil t)))
      (expect (get-text-property 2 'font-lock-face line)
              :to-be 'eca-chat-shell-command-remembered-face))))

(defvar eca-chat-test--ws-session nil
  "Session the workspace-root command specs operate on.")

(defun eca-chat-test--add-root (picked)
  "Run `eca-chat-add-workspace-root' as if the user picked PICKED."
  (cl-letf (((symbol-function 'read-directory-name) (lambda (&rest _) picked)))
    (call-interactively #'eca-chat-add-workspace-root)))

(defun eca-chat-test--remove-root (choose)
  "Run `eca-chat-remove-workspace-root' picking via CHOOSE.
CHOOSE receives the candidate list the command offers."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt collection &rest _) (funcall choose collection))))
    (call-interactively #'eca-chat-remove-workspace-root)))

(describe "workspace root commands end to end"
  (before-each
    (spy-on 'eca-api-notify)
    (spy-on 'eca-info)
    (spy-on 'eca-warn)
    (spy-on 'force-mode-line-update)
    (setq eca-chat-test--ws-session (make-eca--session))
    ;; Read the variable at call time: specs that rebuild it via
    ;; `eca-create-session' must be seen by the commands under test.
    (spy-on 'eca-session :and-call-fake (lambda () eca-chat-test--ws-session)))

  (after-each
    (setq eca-chat-test--ws-session nil))

  ;; The bug: the dir was added and shown in the mode line, but removing
  ;; it reported "Workspace folder not found".
  (it "removes a dir the user just added"
    (setf (eca--session-workspace-folders eca-chat-test--ws-session)
          (list (expand-file-name "/tmp/root")))
    (eca-chat-test--add-root "/tmp/added/")
    (expect (eca--session-workspace-folders eca-chat-test--ws-session)
            :to-equal (list (expand-file-name "/tmp/root")
                            (expand-file-name "/tmp/added")))
    (eca-chat-test--remove-root #'cadr)
    (expect (eca--session-workspace-folders eca-chat-test--ws-session)
            :to-equal (list (expand-file-name "/tmp/root"))))

  (it "removes a dir added in ~ form"
    (setf (eca--session-workspace-folders eca-chat-test--ws-session)
          (list (expand-file-name "/tmp/root")))
    (eca-chat-test--add-root "~/added/")
    (eca-chat-test--remove-root #'cadr)
    (expect (eca--session-workspace-folders eca-chat-test--ws-session)
            :to-equal (list (expand-file-name "/tmp/root"))))

  (it "removes an original session root stored in ~ form"
    (let ((eca--sessions '()))
      (setq eca-chat-test--ws-session (eca-create-session (list "~/root/" "/tmp/other/")))
      (eca-chat-test--add-root "/tmp/added")
      (eca-chat-test--remove-root #'car)
      (expect (eca--session-workspace-folders eca-chat-test--ws-session)
              :to-equal (list (expand-file-name "/tmp/other")
                              (expand-file-name "/tmp/added")))))

  (it "offers no candidate that cannot be removed"
    (dolist (dir '("~/root" "/tmp/other" "/tmp/added"))
      (let ((eca--sessions '()))
        (setq eca-chat-test--ws-session (eca-create-session (list "~/root/" "/tmp/other/")))
        (eca-chat-test--add-root "/tmp/added/")
        (let ((stored (directory-file-name (expand-file-name dir)))
              (before (eca--session-workspace-folders eca-chat-test--ws-session)))
          (eca-chat-test--remove-root
           (lambda (candidates) (--first (string= it stored) candidates)))
          (expect (eca--session-workspace-folders eca-chat-test--ws-session)
                  :to-equal (remove stored before))))))

  (it "keeps the mode line in sync with what can be removed"
    (setf (eca--session-workspace-folders eca-chat-test--ws-session)
          (list (expand-file-name "/tmp/root")))
    (expect (eca-chat--mode-line-module eca-chat-test--ws-session :remove-workspace-button)
            :to-be nil)
    (eca-chat-test--add-root "/tmp/added/")
    (expect (eca-chat--mode-line-module eca-chat-test--ws-session :remove-workspace-button)
            :not :to-be nil)
    (eca-chat-test--remove-root #'cadr)
    (expect (eca-chat--mode-line-module eca-chat-test--ws-session :remove-workspace-button)
            :to-be nil)))

(describe "eca-chat--rollback-prompt-text"

  (it "returns the rolled-back text when there is no draft"
    (with-temp-buffer
      (expect (eca-chat--rollback-prompt-text "redo this") :to-equal "redo this")))

  (it "returns nil when both text and draft are empty"
    (with-temp-buffer
      (expect (eca-chat--rollback-prompt-text nil) :to-be nil)
      (expect (eca-chat--rollback-prompt-text "   ") :to-be nil)))

  (it "appends the draft after the rolled-back text"
    (let ((buf (eca-chat-test--make-prompt-buffer "draft edit")))
      (unwind-protect
          (with-current-buffer buf
            (expect (eca-chat--rollback-prompt-text "old msg")
                    :to-equal "old msg\n\ndraft edit"))
        (kill-buffer buf))))

  (it "keeps a lone draft when there is no rolled-back text"
    (let ((buf (eca-chat-test--make-prompt-buffer "draft edit")))
      (unwind-protect
          (with-current-buffer buf
            (expect (eca-chat--rollback-prompt-text nil) :to-equal "draft edit"))
        (kill-buffer buf))))

  (it "ignores a whitespace-only draft"
    (let ((buf (eca-chat-test--make-prompt-buffer "   ")))
      (unwind-protect
          (with-current-buffer buf
            (expect (eca-chat--rollback-prompt-text "old msg") :to-equal "old msg"))
        (kill-buffer buf)))))

(describe "eca-chat--rollback"

  (it "stashes restored text plus draft for a messages rollback"
    (let ((buf (eca-chat-test--make-prompt-buffer "draft edit"))
          (session (make-eca--session))
          (captured 'unset))
      (unwind-protect
          (with-current-buffer buf
            (setq-local eca-chat--id "chat-1")
            (spy-on 'completing-read
                    :and-return-value "2. Rollback only messages")
            ;; Capture the stash as the server request sees it: the
            ;; `chat/cleared' notification consumes it during the sync wait.
            (spy-on 'eca-api-request-sync :and-call-fake
                    (lambda (&rest _)
                      (setq captured eca-chat--prompt-after-clear)))
            (eca-chat--rollback session "content-1" "old msg")
            (expect captured :to-equal "old msg\n\ndraft edit")
            (expect 'eca-api-request-sync :to-have-been-called)
            ;; The unwind reset drops any unconsumed leftover.
            (expect eca-chat--prompt-after-clear :to-be nil))
        (kill-buffer buf))))

  (it "does not stash for a tools-only rollback"
    (let ((buf (eca-chat-test--make-prompt-buffer "draft edit"))
          (session (make-eca--session))
          (captured 'unset))
      (unwind-protect
          (with-current-buffer buf
            (setq-local eca-chat--id "chat-1")
            (spy-on 'completing-read
                    :and-return-value "3. Rollback only changes done by tool calls")
            (spy-on 'eca-api-request-sync :and-call-fake
                    (lambda (&rest _)
                      (setq captured eca-chat--prompt-after-clear)))
            (eca-chat--rollback session "content-1" "old msg")
            (expect captured :to-be nil)
            (expect 'eca-api-request-sync :to-have-been-called))
        (kill-buffer buf)))))

(describe "eca-chat-cleared"

  (it "restores the stashed prompt into the rebuilt prompt field"
    (let ((buf (eca-chat-test--make-prompt-buffer "draft edit"))
          (session (make-eca--session)))
      (unwind-protect
          (with-current-buffer buf
            (setq-local eca-chat--id "chat-1")
            (setq-local eca-chat--prompt-after-clear "old msg\n\ndraft edit")
            (spy-on 'eca-chat--get-chat-buffer :and-return-value buf)
            (spy-on 'eca-chat--refresh-context)
            (eca-chat-cleared session (list :chatId "chat-1" :messages t))
            (expect (eca-chat-test--prompt-text buf)
                    :to-equal "old msg\n\ndraft edit")
            (expect eca-chat--prompt-after-clear :to-be nil))
        (kill-buffer buf))))

  (it "leaves the prompt empty when nothing is stashed"
    (let ((buf (eca-chat-test--make-prompt-buffer "leftover")))
      (unwind-protect
          (with-current-buffer buf
            (setq-local eca-chat--id "chat-1")
            (spy-on 'eca-chat--get-chat-buffer :and-return-value buf)
            (spy-on 'eca-chat--refresh-context)
            (eca-chat-cleared (make-eca--session)
                              (list :chatId "chat-1" :messages t))
            (expect (eca-chat-test--prompt-text buf) :to-equal ""))
        (kill-buffer buf))))

  (it "does not clear nor consume the stash when messages is nil"
    (let ((buf (eca-chat-test--make-prompt-buffer "draft edit")))
      (unwind-protect
          (with-current-buffer buf
            (setq-local eca-chat--id "chat-1")
            (setq-local eca-chat--prompt-after-clear "old msg")
            (spy-on 'eca-chat--get-chat-buffer :and-return-value buf)
            (spy-on 'eca-chat--clear)
            (eca-chat-cleared (make-eca--session)
                              (list :chatId "chat-1" :messages nil))
            (expect 'eca-chat--clear :not :to-have-been-called)
            (expect eca-chat--prompt-after-clear :to-equal "old msg"))
        (kill-buffer buf)))))

;;; eca-chat-test.el ends here
