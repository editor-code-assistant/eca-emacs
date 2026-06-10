;;; eca-attention-test.el --- Tests for go-to-attention commands -*- lexical-binding: t; -*-
;;; Commentary:
;; Verify `eca-chat--needs-attention-p' and the cycling behavior of
;; `eca-chat-go-to-next-attention' and `eca-chat-go-to-next-attention-in-project'.
;;; Code:
(require 'buttercup)
(require 'eca-chat)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun eca-attention-test--make-chat-buffer (name &optional state)
  "Create a chat-like buffer NAME with attention STATE.
STATE is one of `approval', `question', `loading' or nil.  The
buffer only mimics `eca-chat-mode' by setting `major-mode' so
`derived-mode-p' matches without running the full mode setup."
  (let ((buf (generate-new-buffer name)))
    (with-current-buffer buf
      (setq-local major-mode 'eca-chat-mode)
      (pcase state
        ('approval (insert (propertize
                            "[accept]"
                            'eca-tool-call-pending-approval-accept t)))
        ('question (setq-local eca-chat--pending-question '(:question "q")))
        ('loading (setq-local eca-chat--chat-loading t))))
    buf))

(defun eca-attention-test--make-session (id buffers)
  "Create a session with ID owning chat BUFFERS (oldest-first).
Chats are stored newest-first like `eca-create-session' does."
  (make-eca--session
   :id id
   :chats (nreverse
           (cl-loop for buf in buffers
                    for i from 0
                    collect (cons (format "chat-%s-%s" id i) buf)))))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(describe "eca-chat--needs-attention-p"

  (it "is non-nil for a chat with a pending approval"
    (let ((buf (eca-attention-test--make-chat-buffer "*attn-appr*" 'approval)))
      (unwind-protect
          (expect (eca-chat--needs-attention-p buf) :to-be-truthy)
        (kill-buffer buf))))

  (it "is non-nil for a chat with a pending question"
    (let ((buf (eca-attention-test--make-chat-buffer "*attn-q*" 'question)))
      (unwind-protect
          (expect (eca-chat--needs-attention-p buf) :to-be-truthy)
        (kill-buffer buf))))

  (it "is nil for a loading chat"
    (let ((buf (eca-attention-test--make-chat-buffer "*attn-load*" 'loading)))
      (unwind-protect
          (expect (eca-chat--needs-attention-p buf) :to-be nil)
        (kill-buffer buf))))

  (it "is nil for an idle chat"
    (let ((buf (eca-attention-test--make-chat-buffer "*attn-idle*")))
      (unwind-protect
          (expect (eca-chat--needs-attention-p buf) :to-be nil)
        (kill-buffer buf))))

  (it "is nil for a non-chat buffer"
    (let ((buf (generate-new-buffer "*attn-other*")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert (propertize
                       "[accept]"
                       'eca-tool-call-pending-approval-accept t)))
            (expect (eca-chat--needs-attention-p buf) :to-be nil))
        (kill-buffer buf))))

  (it "is nil for a killed buffer"
    (let ((buf (eca-attention-test--make-chat-buffer "*attn-dead*" 'approval)))
      (kill-buffer buf)
      (expect (eca-chat--needs-attention-p buf) :to-be nil))))

(describe "eca-chat--rotate-after"

  (it "returns ITEMS unchanged when POS is nil"
    (expect (eca-chat--rotate-after '(a b c d) nil) :to-equal '(a b c d)))

  (it "starts right after POS, moving it to the end"
    (expect (eca-chat--rotate-after '(a b c d) 1) :to-equal '(c d a b)))

  (it "moves the first element to the end for POS 0"
    (expect (eca-chat--rotate-after '(a b c d) 0) :to-equal '(b c d a)))

  (it "keeps the order when POS is the last position"
    (expect (eca-chat--rotate-after '(a b c d) 3) :to-equal '(a b c d))))

(describe "eca-chat-go-to-next-attention-in-project"

  (it "goes to the next chat needing attention after the current one"
    (let* ((b1 (eca-attention-test--make-chat-buffer "*p-b1*"))
           (b2 (eca-attention-test--make-chat-buffer "*p-b2*"))
           (b3 (eca-attention-test--make-chat-buffer "*p-b3*" 'approval))
           (session (eca-attention-test--make-session 1 (list b1 b2 b3))))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-chat--switch-to-buffer)
      (unwind-protect
          (with-current-buffer b1
            (eca-chat-go-to-next-attention-in-project)
            (expect 'eca-chat--switch-to-buffer
                    :to-have-been-called-with b3 session))
        (mapc #'kill-buffer (list b1 b2 b3)))))

  (it "wraps around to an earlier chat needing attention"
    (let* ((b1 (eca-attention-test--make-chat-buffer "*w-b1*" 'question))
           (b2 (eca-attention-test--make-chat-buffer "*w-b2*"))
           (b3 (eca-attention-test--make-chat-buffer "*w-b3*"))
           (session (eca-attention-test--make-session 1 (list b1 b2 b3))))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-chat--switch-to-buffer)
      (unwind-protect
          (with-current-buffer b2
            (eca-chat-go-to-next-attention-in-project)
            (expect 'eca-chat--switch-to-buffer
                    :to-have-been-called-with b1 session))
        (mapc #'kill-buffer (list b1 b2 b3)))))

  (it "stays on the current chat when it is the only one waiting"
    (let* ((b1 (eca-attention-test--make-chat-buffer "*s-b1*"))
           (b2 (eca-attention-test--make-chat-buffer "*s-b2*" 'approval))
           (session (eca-attention-test--make-session 1 (list b1 b2))))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-chat--switch-to-buffer)
      (unwind-protect
          (with-current-buffer b2
            (eca-chat-go-to-next-attention-in-project)
            (expect 'eca-chat--switch-to-buffer
                    :to-have-been-called-with b2 session))
        (mapc #'kill-buffer (list b1 b2)))))

  (it "searches all chats when the current buffer is not a chat"
    (let* ((b1 (eca-attention-test--make-chat-buffer "*n-b1*"))
           (b2 (eca-attention-test--make-chat-buffer "*n-b2*" 'approval))
           (other (generate-new-buffer "*n-other*"))
           (session (eca-attention-test--make-session 1 (list b1 b2))))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-chat--switch-to-buffer)
      (unwind-protect
          (with-current-buffer other
            (eca-chat-go-to-next-attention-in-project)
            (expect 'eca-chat--switch-to-buffer
                    :to-have-been-called-with b2 session))
        (mapc #'kill-buffer (list b1 b2 other)))))

  (it "reports when no chat needs attention"
    (let* ((b1 (eca-attention-test--make-chat-buffer "*i-b1*"))
           (session (eca-attention-test--make-session 1 (list b1))))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-chat--switch-to-buffer)
      (spy-on 'eca-info)
      (unwind-protect
          (with-current-buffer b1
            (eca-chat-go-to-next-attention-in-project)
            (expect 'eca-chat--switch-to-buffer :not :to-have-been-called)
            (expect 'eca-info :to-have-been-called))
        (kill-buffer b1)))))

(describe "eca-chat-go-to-next-attention"

  (it "moves to the next project's chat needing attention"
    (let* ((a1 (eca-attention-test--make-chat-buffer "*x-a1*"))
           (a2 (eca-attention-test--make-chat-buffer "*x-a2*"))
           (b1 (eca-attention-test--make-chat-buffer "*x-b1*" 'approval))
           (s1 (eca-attention-test--make-session 1 (list a1 a2)))
           (s2 (eca-attention-test--make-session 2 (list b1)))
           (eca--sessions (list (cons 2 s2) (cons 1 s1))))
      (spy-on 'eca-chat--switch-to-buffer)
      (unwind-protect
          (with-current-buffer a1
            (eca-chat-go-to-next-attention)
            (expect 'eca-chat--switch-to-buffer
                    :to-have-been-called-with b1 s2))
        (mapc #'kill-buffer (list a1 a2 b1)))))

  (it "exhausts the current project before moving on"
    (let* ((a1 (eca-attention-test--make-chat-buffer "*e-a1*"))
           (a2 (eca-attention-test--make-chat-buffer "*e-a2*" 'question))
           (b1 (eca-attention-test--make-chat-buffer "*e-b1*" 'approval))
           (s1 (eca-attention-test--make-session 1 (list a1 a2)))
           (s2 (eca-attention-test--make-session 2 (list b1)))
           (eca--sessions (list (cons 2 s2) (cons 1 s1))))
      (spy-on 'eca-chat--switch-to-buffer)
      (unwind-protect
          (with-current-buffer a1
            (eca-chat-go-to-next-attention)
            (expect 'eca-chat--switch-to-buffer
                    :to-have-been-called-with a2 s1))
        (mapc #'kill-buffer (list a1 a2 b1)))))

  (it "wraps around across sessions"
    (let* ((a1 (eca-attention-test--make-chat-buffer "*r-a1*" 'approval))
           (b1 (eca-attention-test--make-chat-buffer "*r-b1*"))
           (s1 (eca-attention-test--make-session 1 (list a1)))
           (s2 (eca-attention-test--make-session 2 (list b1)))
           (eca--sessions (list (cons 2 s2) (cons 1 s1))))
      (spy-on 'eca-chat--switch-to-buffer)
      (unwind-protect
          (with-current-buffer b1
            (eca-chat-go-to-next-attention)
            (expect 'eca-chat--switch-to-buffer
                    :to-have-been-called-with a1 s1))
        (mapc #'kill-buffer (list a1 b1)))))

  (it "anchors at the current session when not in a chat buffer"
    (let* ((a1 (eca-attention-test--make-chat-buffer "*c-a1*"))
           (a2 (eca-attention-test--make-chat-buffer "*c-a2*" 'question))
           (b1 (eca-attention-test--make-chat-buffer "*c-b1*" 'approval))
           (other (generate-new-buffer "*c-other*"))
           (s1 (eca-attention-test--make-session 1 (list a1 a2)))
           (s2 (eca-attention-test--make-session 2 (list b1)))
           (eca--sessions (list (cons 2 s2) (cons 1 s1))))
      (spy-on 'eca-session :and-return-value s2)
      (spy-on 'eca-chat--switch-to-buffer)
      (unwind-protect
          (with-current-buffer other
            (eca-chat-go-to-next-attention)
            (expect 'eca-chat--switch-to-buffer
                    :to-have-been-called-with b1 s2))
        (mapc #'kill-buffer (list a1 a2 b1 other)))))

  (it "errors when no session is running"
    (let ((eca--sessions '()))
      (expect (eca-chat-go-to-next-attention) :to-throw 'user-error)))

  (it "reports when no chat needs attention in any project"
    (let* ((a1 (eca-attention-test--make-chat-buffer "*z-a1*"))
           (s1 (eca-attention-test--make-session 1 (list a1)))
           (eca--sessions (list (cons 1 s1))))
      (spy-on 'eca-chat--switch-to-buffer)
      (spy-on 'eca-info)
      (unwind-protect
          (with-current-buffer a1
            (eca-chat-go-to-next-attention)
            (expect 'eca-chat--switch-to-buffer :not :to-have-been-called)
            (expect 'eca-info :to-have-been-called))
        (kill-buffer a1)))))

(provide 'eca-attention-test)
;;; eca-attention-test.el ends here
