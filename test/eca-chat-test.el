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
Returns the buffer.  Caller must kill it."
  (let ((buf (generate-new-buffer " *test-chat-deletion*")))
    (with-current-buffer buf
      ;; Simulate content above the prompt
      (insert "header\n---\n@\n\n")
      (let ((prompt-start (point)))
        (insert prompt-text)
        ;; 1-char prompt-field overlay, matching real layout
        (let ((ov (make-overlay prompt-start (1+ prompt-start))))
          (overlay-put ov 'eca-chat-prompt-field t)))
      (setq major-mode 'eca-chat-mode))
    buf))

(defun eca-chat-test--prompt-text (buf)
  "Return the prompt field text from BUF."
  (with-current-buffer buf
    (buffer-substring-no-properties
     (eca-chat--prompt-field-start-point) (point-max))))

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

(describe "eca-chat copy buttons"
  (it "copies fenced code block content"
    (with-temp-buffer
      (setq major-mode 'eca-chat-mode)
      (insert "```elisp\n(+ 1 2)\n```\n")
      (eca-chat--refresh-code-copy-buttons (point-min) (point-max))
      (let* ((ov (-first (lambda (overlay)
                           (overlay-get overlay
                                        'eca-chat--code-copy-button))
                         (overlays-in (point-min) (point-max))))
             (button (overlay-get ov 'after-string))
             (action (get-text-property 1 'eca-button-on-action button)))
        (funcall action)
        (expect (current-kill 0 t) :to-equal "(+ 1 2)"))))

  (it "copies the whole assistant response"
    (with-temp-buffer
      (setq major-mode 'eca-chat-mode)
      (insert "Answer\n")
      (eca-chat--refresh-response-copy-button (point-min) (point-max))
      (let* ((ov (-first (lambda (overlay)
                           (overlay-get overlay
                                        'eca-chat--response-copy-button))
                         (overlays-in (point-min) (point-max))))
             (button (overlay-get ov 'before-string))
             (action (get-text-property 0 'eca-button-on-action button)))
        (funcall action)
        (expect (current-kill 0 t) :to-equal "Answer"))))

  (it "does not add copy buttons to rendered user messages"
    (with-temp-buffer
      (setq major-mode 'eca-chat-mode)
      (insert "User says:\n```elisp\n(+ 1 2)\n```\n")
      (setq-local eca-chat--last-user-message-pos (point-max))
      (let ((ov (make-overlay (point-max) (point-max))))
        (overlay-put ov 'eca-chat-prompt-area t))
      (eca-chat--refresh-copy-buttons)
      (expect (-first (lambda (overlay)
                        (overlay-get overlay 'eca-chat--code-copy-button))
                      (overlays-in (point-min) (point-max)))
              :to-be nil)
      (expect (-first (lambda (overlay)
                        (overlay-get overlay 'eca-chat--response-copy-button))
                      (overlays-in (point-min) (point-max)))
              :to-be nil))))

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

;;; eca-chat-test.el ends here
