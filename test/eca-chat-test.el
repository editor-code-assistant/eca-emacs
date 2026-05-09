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

;;; eca-chat-test.el ends here
