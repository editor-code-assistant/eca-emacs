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

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

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
          (kill-buffer buf))))))

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
      (let* ((capf-res (eca-chat-completion-at-point))
             (completion-fn (nth 2 capf-res)))
        (funcall completion-fn "" nil t)
        (expect 'eca-api-request-while-no-input :not :to-have-been-called)))))

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
          (kill-buffer buf))))))

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
        (kill-buffer buf)))))

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

;;; eca-chat-test.el ends here
