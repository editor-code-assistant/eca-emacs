;;; eca-chat-context-test.el --- Tests for eca-chat-context -*- lexical-binding: t; -*-
;;; Commentary:
;; Tests for buffer (text) contexts and related helpers.
;;; Code:
(require 'buttercup)
(require 'eca-chat)

(describe "eca-chat-context-buffer-include-p"
  (it "includes non-file buffers"
    (let ((buf (generate-new-buffer "*compilation-ctx-test*")))
      (unwind-protect
          (expect (eca-chat-context-buffer-include-p buf) :to-be-truthy)
        (kill-buffer buf))))

  (it "excludes hidden buffers"
    (let ((buf (generate-new-buffer " *hidden-ctx-test*")))
      (unwind-protect
          (expect (eca-chat-context-buffer-include-p buf) :to-be nil)
        (kill-buffer buf))))

  (it "excludes eca own buffers"
    (let ((chat (generate-new-buffer "<eca-chat[proj]:1:1>"))
          (other (generate-new-buffer "*eca-diff-orig:foo*")))
      (unwind-protect
          (progn
            (expect (eca-chat-context-buffer-include-p chat) :to-be nil)
            (expect (eca-chat-context-buffer-include-p other) :to-be nil))
        (kill-buffer chat)
        (kill-buffer other))))

  (it "excludes file-visiting buffers"
    (let ((buf (generate-new-buffer "file-visiting-ctx-test")))
      (unwind-protect
          (with-current-buffer buf
            (setq buffer-file-name "/tmp/eca-ctx-test-file.txt")
            (expect (eca-chat-context-buffer-include-p buf) :to-be nil))
        (with-current-buffer buf
          (setq buffer-file-name nil)
          (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(describe "eca-chat--buffer-contexts"
  (it "returns text contexts for eligible buffers"
    (let ((buf (generate-new-buffer "*ctx-listing-test*")))
      (unwind-protect
          ;; Built outside `expect': on Emacs 29 buttercup's interpreted
          ;; oclosure thunks shadow the `:type' keyword inside expect args.
          (let ((ctx (list :type "text" :label "*ctx-listing-test*")))
            (expect (member ctx (eca-chat--buffer-contexts))
                    :to-be-truthy))
        (kill-buffer buf))))

  (it "skips buffers already added to the context"
    (let ((buf (generate-new-buffer "*ctx-added-test*")))
      (unwind-protect
          (let* ((ctx (list :type "text" :label "*ctx-added-test*"))
                 (eca-chat--context (list ctx)))
            (expect (member ctx (eca-chat--buffer-contexts))
                    :to-be nil))
        (kill-buffer buf)))))

(describe "eca-chat--buffer-context-content"
  (it "returns whole content when under the limit"
    (with-temp-buffer
      (insert "hello")
      (let ((eca-chat-context-buffer-max-chars 100))
        (expect (eca-chat--buffer-context-content (current-buffer))
                :to-equal "hello"))))

  (it "keeps the tail when over the limit"
    (with-temp-buffer
      (insert "0123456789")
      (let ((eca-chat-context-buffer-max-chars 4))
        (expect (eca-chat--buffer-context-content (current-buffer))
                :to-equal "6789"))))

  (it "returns whole content when limit is nil"
    (with-temp-buffer
      (insert "0123456789")
      (let ((eca-chat-context-buffer-max-chars nil))
        (expect (eca-chat--buffer-context-content (current-buffer))
                :to-equal "0123456789"))))

  (it "ignores narrowing"
    (with-temp-buffer
      (insert "abcdef")
      (narrow-to-region 1 3)
      (let ((eca-chat-context-buffer-max-chars nil))
        (expect (eca-chat--buffer-context-content (current-buffer))
                :to-equal "abcdef"))))

  (it "restricts content to the given lines range"
    (with-temp-buffer
      (insert "l1\nl2\nl3\nl4\n")
      (let ((eca-chat-context-buffer-max-chars nil))
        (expect (eca-chat--buffer-context-content
                 (current-buffer) (list :start 2 :end 3))
                :to-equal "l2\nl3"))))

  (it "keeps the range tail when over the limit"
    (with-temp-buffer
      (insert "l1\nl2\nl3\nl4\n")
      (let ((eca-chat-context-buffer-max-chars 2))
        (expect (eca-chat--buffer-context-content
                 (current-buffer) (list :start 2 :end 3))
                :to-equal "l3"))))

  (it "returns empty when the range is past the buffer end"
    (with-temp-buffer
      (insert "l1\nl2\n")
      (let ((eca-chat-context-buffer-max-chars nil))
        (expect (eca-chat--buffer-context-content
                 (current-buffer) (list :start 5 :end 8))
                :to-equal "")))))

(describe "eca-chat--materialize-context"
  (it "fills text context with fresh buffer content"
    (let ((buf (generate-new-buffer "*materialize-test*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (insert "output line"))
            (let ((context (eca-chat--materialize-context
                            (list :type "text" :label "*materialize-test*"))))
              (expect (plist-get context :content) :to-equal "output line")))
        (kill-buffer buf))))

  (it "slices text context content to its lines range"
    (let ((buf (generate-new-buffer "*materialize-range-test*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (insert "l1\nl2\nl3\n"))
            ;; Built outside `expect', see eca-chat--buffer-contexts
            ;; test above.
            (let* ((ctx (list :type "text" :label "*materialize-range-test*"
                              :linesRange (list :start 2 :end 2)))
                   (context (eca-chat--materialize-context ctx)))
              (expect (plist-get context :content) :to-equal "l2")
              (expect (plist-get context :linesRange) :to-be nil)))
        (kill-buffer buf))))

  (it "does not mutate the original context"
    (let ((buf (generate-new-buffer "*materialize-orig-test*")))
      (unwind-protect
          (let ((original (list :type "text" :label "*materialize-orig-test*")))
            (eca-chat--materialize-context original)
            (expect (plist-get original :content) :to-be nil))
        (kill-buffer buf))))

  (it "drops contexts of killed buffers"
    ;; Built outside `expect', see eca-chat--buffer-contexts test above.
    (let ((ctx (list :type "text" :label "*no-such-buffer-eca-test*")))
      (expect (eca-chat--materialize-context ctx) :to-be nil)))

  (it "drops cursor contexts with no tracked position"
    (let ((ctx (list :type "cursor" :path nil :position nil)))
      (expect (eca-chat--materialize-context ctx) :to-be nil)))

  (it "passes cursor contexts with a position through"
    (let ((context (list :type "cursor" :path "/tmp/foo.el"
                         :position (list :start (list :line 1 :character 1)
                                         :end (list :line 1 :character 1)))))
      (expect (eca-chat--materialize-context context) :to-be context)))

  (it "passes other contexts through unchanged"
    (let ((context (list :type "file" :path "/tmp/foo.txt")))
      (expect (eca-chat--materialize-context context) :to-be context))))

(describe "eca-chat--context->str for text contexts"
  (it "renders the buffer name with the context prefix"
    ;; Built outside `expect', see eca-chat--buffer-contexts test above.
    (let* ((ctx (list :type "text" :label "*compilation*"))
           (str (eca-chat--context->str ctx)))
      (expect (substring-no-properties str) :to-equal "@*compilation*")
      (expect (get-text-property 0 'eca-chat-context-item str)
              :to-equal ctx)))

  (it "renders the lines range when present"
    ;; Built outside `expect', see eca-chat--buffer-contexts test above.
    (let* ((ctx (list :type "text" :label "*vterm*"
                      :linesRange (list :start 5 :end 10)))
           (str (eca-chat--context->str ctx)))
      (expect (substring-no-properties str) :to-equal "@*vterm*(5-10)")
      (expect (get-text-property 0 'eca-chat-expanded-item-str str)
              :to-equal "@*vterm*:L5-L10"))))

(describe "eca-chat--context->str for cursor contexts"
  (it "renders a placeholder when no position was tracked yet"
    ;; Contexts built outside `expect', see eca-chat--buffer-contexts
    ;; test above.
    (let ((eca-chat--cursor-context nil)
          (ctx (list :type "cursor")))
      (expect (substring-no-properties (eca-chat--context->str ctx))
              :to-equal "@cursor(no file)")))

  (it "renders file name and position when tracked"
    (let ((eca-chat--cursor-context
           (list :path "/tmp/proj-a/foo.el"
                 :position (list :start (list :line 12 :character 3)
                                 :end (list :line 12 :character 3))))
          (ctx (list :type "cursor")))
      (expect (substring-no-properties (eca-chat--context->str ctx))
              :to-equal "@cursor(foo.el 12:3)")))

  (it "renders statically without dynamic values"
    (let ((eca-chat--cursor-context nil)
          (ctx (list :type "cursor")))
      (expect (substring-no-properties (eca-chat--context->str ctx 'static))
              :to-equal "@cursor"))))

(describe "eca-chat--get-contexts-dwim"
  (it "returns a ranged text context for a region in a non-file buffer"
    (let ((buf (generate-new-buffer "*dwim-region-test*")))
      (unwind-protect
          (with-current-buffer buf
            (insert "l1\nl2\nl3\nl4\n")
            (let ((transient-mark-mode t))
              (goto-char (point-min))
              (forward-line 1)
              (push-mark (point) t t)
              (forward-line 1)
              (end-of-line)
              ;; Built outside `expect', see eca-chat--buffer-contexts
              ;; test above.
              (let ((ctx (list :type "text" :label "*dwim-region-test*"
                               :linesRange (list :start 2 :end 3))))
                (expect (eca-chat--get-contexts-dwim) :to-equal (list ctx)))))
        (kill-buffer buf))))

  (it "bypasses the buffer predicate when a region is selected"
    (let ((buf (generate-new-buffer "<eca-chat-dwim-test>")))
      (unwind-protect
          (with-current-buffer buf
            (insert "previous response\n")
            (let ((transient-mark-mode t))
              (goto-char (point-min))
              (push-mark (point) t t)
              (end-of-line)
              (let ((ctx (list :type "text" :label "<eca-chat-dwim-test>"
                               :linesRange (list :start 1 :end 1))))
                (expect (eca-chat--get-contexts-dwim) :to-equal (list ctx)))))
        (kill-buffer buf))))

  (it "returns a whole-buffer text context when no region is selected"
    (let ((buf (generate-new-buffer "*dwim-whole-test*")))
      (unwind-protect
          (with-current-buffer buf
            (insert "l1\n")
            (let ((ctx (list :type "text" :label "*dwim-whole-test*")))
              (expect (eca-chat--get-contexts-dwim) :to-equal (list ctx))))
        (kill-buffer buf))))

  (it "returns nil for excluded buffers without a region"
    (let ((buf (generate-new-buffer "<eca-chat-no-region-test>")))
      (unwind-protect
          (with-current-buffer buf
            (expect (eca-chat--get-contexts-dwim) :to-be nil))
        (kill-buffer buf))))

  (it "keeps returning a file context for a region in a file buffer"
    (let ((buf (generate-new-buffer "dwim-file-region-test")))
      (unwind-protect
          (with-current-buffer buf
            (insert "l1\nl2\n")
            (setq buffer-file-name "/tmp/eca-dwim-file-test.txt")
            (let ((transient-mark-mode t))
              (goto-char (point-min))
              (push-mark (point) t t)
              (forward-line 1)
              (end-of-line)
              (let ((ctx (list :type "file" :path "/tmp/eca-dwim-file-test.txt"
                               :linesRange (list :start 1 :end 2))))
                (expect (eca-chat--get-contexts-dwim) :to-equal (list ctx)))))
        (with-current-buffer buf
          (setq buffer-file-name nil)
          (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(describe "eca-chat--get-last-visited-buffer"
  (it "skips more recent file buffers outside any session workspace"
    (let ((eca--sessions '())
          (eca--session-ids 0))
      (eca-create-session (list (expand-file-name "/tmp/proj-a")))
      (let ((outside (generate-new-buffer "outside-workspace-file"))
            (inside (generate-new-buffer "inside-workspace-file")))
        (unwind-protect
            (progn
              (with-current-buffer inside
                (setq buffer-file-name
                      (expand-file-name "/tmp/proj-a/inside.el")))
              (with-current-buffer outside
                (setq buffer-file-name
                      (expand-file-name "/tmp/other-proj/outside.el")))
              ;; `outside' is the most recently used buffer.
              (spy-on 'buffer-list
                      :and-return-value (list outside inside))
              (expect (eca-chat--get-last-visited-buffer) :to-be inside))
          (dolist (buf (list outside inside))
            (with-current-buffer buf
              (setq buffer-file-name nil)
              (set-buffer-modified-p nil))
            (kill-buffer buf))))))

  (it "returns nil when no workspace file buffer exists"
    (let ((eca--sessions '())
          (eca--session-ids 0))
      (eca-create-session (list (expand-file-name "/tmp/proj-a")))
      (let ((outside (generate-new-buffer "only-outside-file")))
        (unwind-protect
            (progn
              (with-current-buffer outside
                (setq buffer-file-name
                      (expand-file-name "/tmp/other-proj/outside.el")))
              (spy-on 'buffer-list :and-return-value (list outside))
              (expect (eca-chat--get-last-visited-buffer) :to-be nil))
          (with-current-buffer outside
            (setq buffer-file-name nil)
            (set-buffer-modified-p nil))
          (kill-buffer outside))))))

(describe "eca-chat--context-to-completion for text contexts"
  (it "uses the buffer name as label"
    (let ((item (eca-chat--context-to-completion
                 "" '("/tmp") (list :type "text" :label "*shell*"))))
      (expect (substring-no-properties item) :to-equal "*shell*")
      ;; Extracted outside `expect', see eca-chat--buffer-contexts test above.
      (let ((type (plist-get (get-text-property 0 'eca-chat-completion-item item)
                             :type)))
        (expect type :to-equal "text")))))

(describe "eca-chat--normalize-prompt with text context chips"
  (it "keeps non-path context labels as-is"
    (let* ((chip (eca-chat--context->str (list :type "text" :label "*compilation*") 'static))
           (prompt (concat "check " chip " please")))
      (expect (eca-chat--normalize-prompt prompt)
              :to-equal "check @*compilation* please"))))

(describe "eca-chat--session-for-path"
  (it "returns the session owning the path and nil otherwise"
    (let ((eca--sessions '())
          (eca--session-ids 0))
      (let ((s1 (eca-create-session (list (expand-file-name "/tmp/proj-a"))))
            (s2 (eca-create-session (list (expand-file-name "/tmp/proj-b")))))
        (expect (eca-chat--session-for-path
                 (expand-file-name "/tmp/proj-a/src/foo.el"))
                :to-be s1)
        (expect (eca-chat--session-for-path
                 (expand-file-name "/tmp/proj-b/bar.el"))
                :to-be s2)
        (expect (eca-chat--session-for-path
                 (expand-file-name "/tmp/other/baz.el"))
                :to-be nil)))))

(describe "eca-chat--track-cursor"
  (it "never probes git nor eca-session, even outside workspaces (#275)"
    (let ((eca--sessions '())
          (eca--session-ids 0))
      (eca-create-session (list (expand-file-name "/tmp/proj-a")))
      (spy-on 'eca-session :and-call-through)
      (spy-on 'eca--git-common-dir :and-call-through)
      (spy-on 'process-file)
      (let ((buf (generate-new-buffer "other-project-file")))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    (expand-file-name "/tmp/other-proj/file.el"))
              (spy-on 'eca-chat--get-last-visited-buffer
                      :and-return-value buf)
              ;; Buffer outside any workspace: no-op, no probing.
              (eca-chat--track-cursor)
              ;; Buffer inside a workspace: still no probing.
              (setq buffer-file-name
                    (expand-file-name "/tmp/proj-a/file.el"))
              (eca-chat--track-cursor)
              (expect 'eca-session :not :to-have-been-called)
              (expect 'eca--git-common-dir :not :to-have-been-called)
              (expect 'process-file :not :to-have-been-called))
          (with-current-buffer buf
            (setq buffer-file-name nil)
            (set-buffer-modified-p nil))
          (kill-buffer buf))))))

(provide 'eca-chat-context-test)
;;; eca-chat-context-test.el ends here
