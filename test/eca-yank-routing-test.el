;;; eca-yank-routing-test.el --- Image yank routing tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression coverage for current-chat image routing.

;;; Code:
(require 'buttercup)
(require 'eca)
(require 'eca-chat)
(describe "image yank routing"
  (it "adds the image to the current registered chat, not the last chat"
    (let* ((eca--sessions '())
           (eca--session-ids 0)
           (root (make-temp-file "eca-yank-test" t))
           (default-directory (file-name-as-directory root))
           (session (eca-create-session (list root)))
           (image (make-temp-file "eca-image" nil ".png"))
           (a (generate-new-buffer " *eca-yank-a*"))
           (b (generate-new-buffer " *eca-yank-b*"))
           (eca-chat-yank-image-context-location 'system))
      (unwind-protect
          (progn
            (setf (eca--session-status session) 'started
                  (eca--session-chats session) (list (cons "a" a) (cons "b" b))
                  (eca--session-last-chat-buffer session) b)
            (dolist (buf (list a b))
              (with-current-buffer buf
                (let ((eca--chat-init-session session)
                      (eca--chat-init-skip-welcome t))
                  (eca-chat-mode))))
            (spy-on 'eca-chat-media--save-clipboard-image :and-return-value image)
            (spy-on 'eca-chat--select-window)
            (spy-on 'eca-info)
            (with-current-buffer a
              (eca-chat--yank-image-handler "image/png" "data"))
            (expect (length (buffer-local-value 'eca-chat--context a)) :to-equal 1)
            (expect (buffer-local-value 'eca-chat--context b) :to-be nil))
        (mapc #'kill-buffer (list a b))
        (delete-file image)
        (delete-directory root)))))

;;; eca-yank-routing-test.el ends here
