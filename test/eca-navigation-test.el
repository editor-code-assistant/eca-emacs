;;; eca-navigation-test.el --- Tests for chat navigation -*- lexical-binding: t; -*-
;;; Commentary:
;; Verify that `eca-chat--go-to-overlay' navigates within
;; the current buffer rather than switching to the session's
;; last-chat-buffer (multi-session bug).
;;; Code:
(require 'buttercup)
(require 'eca-chat)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun eca-nav-test--make-buffer (name overlays)
  "Create buffer NAME with overlay specs OVERLAYS.
Each element of OVERLAYS is (OV-KEY ID TEXT).  The buffer is
populated sequentially: filler line, then each overlay block
separated by a filler line.  Returns the buffer."
  (let ((buf (generate-new-buffer name)))
    (with-current-buffer buf
      (insert "header\n")
      (dolist (spec overlays)
        (let ((ov-key (nth 0 spec))
              (id     (nth 1 spec))
              (text   (nth 2 spec))
              (start  (point)))
          (insert text)
          (let ((ov (make-overlay start (point))))
            (overlay-put ov ov-key id)))
        (insert "\nfiller\n")))
    buf))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(describe "eca-chat--go-to-overlay"

  (describe "forward navigation (first? = t)"

    (it "moves point to the first matching overlay"
      (let ((buf (eca-nav-test--make-buffer
                  "*nav-fwd*"
                  '((eca-chat--expandable-content-id "b1" "[block-1]")
                    (eca-chat--expandable-content-id "b2" "[block-2]")))))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              (let ((result
                     (eca-chat--go-to-overlay
                      'eca-chat--expandable-content-id
                      (point-min) (point-max) t)))
                (expect result :not :to-be nil)
                ;; Point should be at overlay "b1"
                (let ((ov (car (overlays-at (point)))))
                  (expect ov :not :to-be nil)
                  (expect (overlay-get
                           ov
                           'eca-chat--expandable-content-id)
                          :to-equal "b1"))))
          (kill-buffer buf))))

    (it "finds the second overlay when range starts past the first"
      (let ((buf (eca-nav-test--make-buffer
                  "*nav-fwd-2*"
                  '((eca-chat--expandable-content-id "b1" "[block-1]")
                    (eca-chat--expandable-content-id "b2" "[block-2]")))))
        (unwind-protect
            (with-current-buffer buf
              ;; First nav lands on b1
              (goto-char (point-min))
              (eca-chat--go-to-overlay
               'eca-chat--expandable-content-id
               (point-min) (point-max) t)
              (let ((b1-end
                     (overlay-end
                      (car (overlays-at (point))))))
                ;; Start range past the entire b1 overlay
                (eca-chat--go-to-overlay
                 'eca-chat--expandable-content-id
                 (1+ b1-end) (point-max) t)
                (let ((ov (car (overlays-at (point)))))
                  (expect (overlay-get
                           ov
                           'eca-chat--expandable-content-id)
                          :to-equal "b2"))))
          (kill-buffer buf)))))

  (describe "backward navigation (first? = nil)"

    (it "moves point to the last matching overlay before point"
      (let ((buf (eca-nav-test--make-buffer
                  "*nav-bwd*"
                  '((eca-chat--expandable-content-id "b1" "[block-1]")
                    (eca-chat--expandable-content-id "b2" "[block-2]")))))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-max))
              (let ((result
                     (eca-chat--go-to-overlay
                      'eca-chat--expandable-content-id
                      (point-min) (point) nil)))
                (expect result :not :to-be nil)
                (let ((ov (car (overlays-at (point)))))
                  (expect (overlay-get
                           ov
                           'eca-chat--expandable-content-id)
                          :to-equal "b2"))))
          (kill-buffer buf)))))

  (describe "no match"

    (it "returns nil and does not move point"
      (let ((buf (generate-new-buffer "*nav-empty*")))
        (unwind-protect
            (with-current-buffer buf
              (insert "no overlays here\n")
              (goto-char (point-min))
              (let ((orig (point)))
                (expect (eca-chat--go-to-overlay
                         'eca-chat--expandable-content-id
                         (point-min) (point-max) t)
                        :to-be nil)
                (expect (point) :to-equal orig)))
          (kill-buffer buf))))

    (it "returns nil when overlays have a different key"
      (let ((buf (eca-nav-test--make-buffer
                  "*nav-wrong-key*"
                  '((eca-chat--user-message-id "m1" "[msg]")))))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              (expect (eca-chat--go-to-overlay
                       'eca-chat--expandable-content-id
                       (point-min) (point-max) t)
                      :to-be nil))
          (kill-buffer buf)))))

  (describe "user-message overlays"

    (it "navigates to user-message overlays the same way"
      (let ((buf (eca-nav-test--make-buffer
                  "*nav-msg*"
                  '((eca-chat--user-message-id "m1" "[msg-1]")
                    (eca-chat--user-message-id "m2" "[msg-2]")))))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              (eca-chat--go-to-overlay
               'eca-chat--user-message-id
               (point-min) (point-max) t)
              (let ((ov (car (overlays-at (point)))))
                (expect (overlay-get
                         ov 'eca-chat--user-message-id)
                        :to-equal "m1")))
          (kill-buffer buf)))))

  (describe "multi-buffer isolation"

    (it "operates only in the current buffer"
      (let ((buf-a (eca-nav-test--make-buffer
                    "*nav-iso-a*"
                    '((eca-chat--expandable-content-id
                       "a1" "[block-a]"))))
            (buf-b (eca-nav-test--make-buffer
                    "*nav-iso-b*"
                    '((eca-chat--expandable-content-id
                       "b1" "[block-b]")))))
        (unwind-protect
            (progn
              ;; Navigate in buf-a
              (with-current-buffer buf-a
                (goto-char (point-min))
                (eca-chat--go-to-overlay
                 'eca-chat--expandable-content-id
                 (point-min) (point-max) t)
                (let ((ov (car (overlays-at (point)))))
                  (expect (overlay-get
                           ov
                           'eca-chat--expandable-content-id)
                          :to-equal "a1")))
              ;; Navigate in buf-b independently
              (with-current-buffer buf-b
                (goto-char (point-min))
                (eca-chat--go-to-overlay
                 'eca-chat--expandable-content-id
                 (point-min) (point-max) t)
                (let ((ov (car (overlays-at (point)))))
                  (expect (overlay-get
                           ov
                           'eca-chat--expandable-content-id)
                          :to-equal "b1"))))
          (kill-buffer buf-a)
          (kill-buffer buf-b))))))

(provide 'eca-navigation-test)
;;; eca-navigation-test.el ends here
