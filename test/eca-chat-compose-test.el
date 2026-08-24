;;; eca-chat-compose-test.el --- Tests for eca-chat-compose -*- lexical-binding: t; -*-
;;; Commentary:
;; Tests for the compose-buffer prompt flow: targeting, sending and
;; cancelling.
;;; Code:
(require 'buttercup)
(require 'eca-chat-compose)

(describe "eca-chat-compose"
  (let (target-buffer session)
    (before-each
      (setq target-buffer (generate-new-buffer "*eca-compose-test-chat*"))
      (with-current-buffer target-buffer
        (setq major-mode 'eca-chat-mode)
        (setq-local eca-chat--id "chat-1"))
      (setq session (make-eca--session :id "session-1"
                                       :last-chat-buffer target-buffer))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca--session-workspace-folders :and-return-value '("/ws")))

    (after-each
      (when (buffer-live-p target-buffer)
        (kill-buffer target-buffer)))

    (describe "eca-chat-compose"
      (it "signals a user-error when no session is running"
        (spy-on 'eca-session :and-return-value nil)
        (expect (eca-chat-compose) :to-throw 'user-error))

      (it "targets the session's last chat buffer when not called from a chat"
        (let ((compose-buffer nil))
          (spy-on 'pop-to-buffer :and-call-fake
                  (lambda (buf) (setq compose-buffer buf)))
          (with-temp-buffer
            (eca-chat-compose))
          (unwind-protect
              (with-current-buffer compose-buffer
                (expect (eq eca-chat-compose--target-buffer target-buffer) :to-be-truthy)
                (expect major-mode :to-be 'eca-chat-compose-mode)
                (expect eca-chat--id :to-equal "chat-1"))
            (kill-buffer compose-buffer))))

      (it "targets the current buffer when called from a chat buffer"
        (let ((compose-buffer nil))
          (spy-on 'pop-to-buffer :and-call-fake
                  (lambda (buf) (setq compose-buffer buf)))
          (with-current-buffer target-buffer
            (eca-chat-compose))
          (unwind-protect
              (with-current-buffer compose-buffer
                (expect (eq eca-chat-compose--target-buffer target-buffer) :to-be-truthy))
            (kill-buffer compose-buffer)))))

    (describe "eca-chat-compose-send"
      (it "signals a user-error for an empty prompt"
        (let ((buf (generate-new-buffer " *compose-empty*")))
          (unwind-protect
              (with-current-buffer buf
                (eca-chat-compose-mode)
                (setq eca-chat-compose--target-buffer target-buffer)
                (expect (eca-chat-compose-send) :to-throw 'user-error))
            (when (buffer-live-p buf) (kill-buffer buf)))))

      (it "signals a user-error when the target buffer is gone"
        (let ((buf (generate-new-buffer " *compose-dead-target*"))
              (dead (generate-new-buffer " *compose-dead*")))
          (kill-buffer dead)
          (unwind-protect
              (with-current-buffer buf
                (eca-chat-compose-mode)
                (insert "hello")
                (setq eca-chat-compose--target-buffer dead)
                (expect (eca-chat-compose-send) :to-throw 'user-error))
            (when (buffer-live-p buf) (kill-buffer buf)))))

      (it "sends the buffer text to the target chat and kills the compose buffer"
        (let ((buf (generate-new-buffer " *compose-send*"))
              (sent-session nil)
              (sent-prompt nil))
          (spy-on 'eca-chat--send-prompt
                  :and-call-fake (lambda (s p)
                                  (setq sent-session s
                                        sent-prompt p)))
          (spy-on 'quit-window)
          (with-current-buffer buf
            (eca-chat-compose-mode)
            (insert "  hello eca  ")
            (setq eca-chat-compose--target-buffer target-buffer)
            (eca-chat-compose-send))
          (expect sent-prompt :to-equal "hello eca")
          (expect (eq sent-session session) :to-be-truthy)
          (expect (eq (eca--session-last-chat-buffer session) target-buffer) :to-be-truthy)
          (expect 'quit-window :to-have-been-called)
          (when (buffer-live-p buf) (kill-buffer buf)))))

    (describe "eca-chat-compose-cancel"
      (it "kills the compose buffer without sending anything"
        (spy-on 'eca-chat--send-prompt)
        (let ((buf (generate-new-buffer " *compose-cancel*")))
          (spy-on 'quit-window)
          (with-current-buffer buf
            (eca-chat-compose-mode)
            (insert "discard me")
            (eca-chat-compose-cancel))
          (expect 'quit-window :to-have-been-called)
          (expect 'eca-chat--send-prompt :not :to-have-been-called)
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;; eca-chat-compose-test.el ends here
