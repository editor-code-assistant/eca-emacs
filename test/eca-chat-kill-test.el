;;; eca-chat-kill-test.el --- Tests for closing/killing chats -*- lexical-binding: t; -*-
;;; Commentary:
;; Verify that closing a chat (kill-buffer, C-c C-k reset, tab
;; close) moves focus to a sibling chat (the previous one, or the
;; only one left) instead of falling back to an unrelated buffer
;; such as the settings buffer, and that the dead chat is dropped
;; from the session registry.
;;; Code:
(require 'buttercup)
(require 'eca-chat)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun eca-kill-test--make-chat (session id)
  "Create and register an eca chat buffer with chat-id ID for SESSION.
Returns the buffer.  Caller must kill it."
  (let ((buf (generate-new-buffer (format " *eca-chat-test:%s*" id))))
    (with-current-buffer buf
      (setq major-mode 'eca-chat-mode)
      (setq-local eca-chat--id id)
      (setq-local eca-chat--closed nil))
    (setf (eca--session-chats session)
          (eca-assoc (eca--session-chats session) id buf))
    buf))

(defun eca-kill-test--install-kill-hooks (buffer)
  "Install eca's kill hook on BUFFER.
Mirrors what `eca-chat-mode' registers, so tests exercise the real
cleanup flow in `kill-buffer-hook'."
  (with-current-buffer buffer
    (add-hook 'kill-buffer-hook #'eca-chat--on-buffer-killed nil t)))

(defun eca-kill-test--kill-all (&rest buffers)
  "Kill every live buffer in BUFFERS, ignoring the eca kill hook.
The hook is removed buffer-locally rather than let-bound: binding
`kill-buffer-hook' only shadows its global value and would still
run buffer-local entries."
  (dolist (buf buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (remove-hook 'kill-buffer-hook #'eca-chat--on-buffer-killed t))
      (let ((kill-buffer-hook nil)
            (kill-buffer-query-functions nil))
        (kill-buffer buf)))))

;; ---------------------------------------------------------------------------
;; eca-chat--sibling-chat-buffer
;; ---------------------------------------------------------------------------

(describe "eca-chat--sibling-chat-buffer"

  (it "returns the previous chat (the tab to the left)"
    (let ((session (make-eca--session)) a b c)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A")
                  b (eca-kill-test--make-chat session "B")
                  c (eca-kill-test--make-chat session "C"))
            (expect (eca-chat--sibling-chat-buffer session b) :to-be a)
            (expect (eca-chat--sibling-chat-buffer session c) :to-be b))
        (eca-kill-test--kill-all a b c))))

  (it "returns the next chat when closing the leftmost one"
    (let ((session (make-eca--session)) a b)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A")
                  b (eca-kill-test--make-chat session "B"))
            (expect (eca-chat--sibling-chat-buffer session a) :to-be b))
        (eca-kill-test--kill-all a b))))

  (it "returns nil when it is the only chat"
    (let ((session (make-eca--session)) a)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A"))
            (expect (eca-chat--sibling-chat-buffer session a) :to-be nil))
        (eca-kill-test--kill-all a))))

  (it "skips dead buffers lingering in the registry"
    (let ((session (make-eca--session)) a b dead)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A")
                  dead (eca-kill-test--make-chat session "DEAD")
                  b (eca-kill-test--make-chat session "B"))
            ;; Drop DEAD's buffer without cleaning the registry entry.
            (eca-kill-test--kill-all dead)
            ;; B's previous live tab is A (DEAD is filtered out).
            (expect (eca-chat--sibling-chat-buffer session b) :to-be a))
        (eca-kill-test--kill-all a b))))

  (it "returns any other live chat when BUFFER is not registered"
    (let ((session (make-eca--session)) a stray)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A")
                  stray (generate-new-buffer " *eca-chat-test:stray*"))
            (expect (eca-chat--sibling-chat-buffer session stray) :to-be a))
        (eca-kill-test--kill-all a stray)))))

;; ---------------------------------------------------------------------------
;; eca-chat--switch-windows-to-sibling
;; ---------------------------------------------------------------------------

(describe "eca-chat--switch-windows-to-sibling"

  (it "replaces the buffer in its window with the sibling"
    (let ((session (make-eca--session)) a b)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A")
                  b (eca-kill-test--make-chat session "B"))
            (set-window-buffer (selected-window) b)
            (expect (eca-chat--switch-windows-to-sibling session b) :to-be a)
            (expect (window-buffer (selected-window)) :to-be a)
            (expect (eca--session-last-chat-buffer session) :to-be a))
        (eca-kill-test--kill-all a b))))

  (it "preserves the window dedication flag"
    (let ((session (make-eca--session)) a b)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A")
                  b (eca-kill-test--make-chat session "B"))
            (set-window-buffer (selected-window) b)
            (set-window-dedicated-p (selected-window) t)
            (eca-chat--switch-windows-to-sibling session b)
            (expect (window-buffer (selected-window)) :to-be a)
            (expect (window-dedicated-p (selected-window)) :to-be-truthy)
            (set-window-dedicated-p (selected-window) nil))
        (eca-kill-test--kill-all a b))))

  (it "returns nil and leaves the window alone with no sibling"
    (let ((session (make-eca--session)) a)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A"))
            (set-window-buffer (selected-window) a)
            (expect (eca-chat--switch-windows-to-sibling session a) :to-be nil)
            (expect (window-buffer (selected-window)) :to-be a))
        (eca-kill-test--kill-all a)))))

;; ---------------------------------------------------------------------------
;; Killing a chat buffer (kill-buffer-hook integration)
;; ---------------------------------------------------------------------------

(describe "killing a chat buffer"

  (it "switches the window to the previous chat and drops it from the registry"
    (let ((session (make-eca--session)) a b)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-chat--force-tab-line-update)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A")
                  b (eca-kill-test--make-chat session "B"))
            (eca-kill-test--install-kill-hooks b)
            (set-window-buffer (selected-window) b)
            (let ((this-command #'kill-buffer))
              (kill-buffer b))
            (expect (window-buffer (selected-window)) :to-be a)
            (expect (eca-get (eca--session-chats session) "B") :to-be nil)
            (expect (eca--session-last-chat-buffer session) :to-be a))
        (eca-kill-test--kill-all a b))))

  (it "never deletes the chat server-side"
    ;; Killing only closes the buffer: the chat stays resumable and the
    ;; server retention cleanup removes old chats.  Explicit deletion
    ;; goes through `eca-chat-delete' or the /delete-chat command.
    (let ((session (make-eca--session)) a b)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-api-request-sync)
      (spy-on 'eca-chat--force-tab-line-update)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A")
                  b (eca-kill-test--make-chat session "B"))
            (eca-kill-test--install-kill-hooks b)
            (let ((this-command #'kill-buffer))
              (kill-buffer b))
            (expect (buffer-live-p b) :to-be nil)
            (expect 'eca-api-request-sync :not :to-have-been-called))
        (eca-kill-test--kill-all a b))))

  (it "skips cleanup for an already-closed chat"
    (let ((session (make-eca--session)) a)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-chat--force-tab-line-update)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A"))
            (with-current-buffer a
              (setq-local eca-chat--closed t))
            (eca-kill-test--install-kill-hooks a)
            (let ((this-command #'kill-buffer))
              (kill-buffer a))
            ;; The registry entry is left for `eca-chat-deleted' to clean.
            (expect (eca-get (eca--session-chats session) "A") :to-be a)
            (expect 'eca-chat--force-tab-line-update :not :to-have-been-called))
        (eca-kill-test--kill-all a)))))

;; ---------------------------------------------------------------------------
;; eca-chat-deleted (server-side deletion notification)
;; ---------------------------------------------------------------------------

(describe "eca-chat-deleted"

  (it "switches the window to a sibling and removes the deleted chat"
    (let ((session (make-eca--session)) a b)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A")
                  b (eca-kill-test--make-chat session "B"))
            (set-window-buffer (selected-window) b)
            (eca-chat-deleted session (list :chatId "B"))
            (expect (buffer-live-p b) :to-be nil)
            (expect (eca-get (eca--session-chats session) "B") :to-be nil)
            (expect (window-buffer (selected-window)) :to-be a))
        (eca-kill-test--kill-all a b)))))

;; ---------------------------------------------------------------------------
;; eca-chat-reset (C-c C-k)
;; ---------------------------------------------------------------------------

(describe "eca-chat-reset"

  (it "kills the chat and lands on the previous one without starting a new chat"
    (let ((session (make-eca--session)) a b)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-assert-session-running)
      (spy-on 'eca-chat--new-chat)
      (spy-on 'eca-chat--force-tab-line-update)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A")
                  b (eca-kill-test--make-chat session "B"))
            (eca-kill-test--install-kill-hooks b)
            (setf (eca--session-last-chat-buffer session) b)
            (set-window-buffer (selected-window) b)
            (let ((this-command #'eca-chat-reset))
              (eca-chat-reset))
            (expect (buffer-live-p b) :to-be nil)
            (expect 'eca-chat--new-chat :not :to-have-been-called)
            (expect (window-buffer (selected-window)) :to-be a))
        (eca-kill-test--kill-all a b))))

  (it "starts a fresh chat when it was the only one"
    (let ((session (make-eca--session)) a)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-assert-session-running)
      (spy-on 'eca-chat--new-chat)
      (spy-on 'eca-chat--force-tab-line-update)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A"))
            (eca-kill-test--install-kill-hooks a)
            (setf (eca--session-last-chat-buffer session) a)
            (let ((this-command #'eca-chat-reset))
              (eca-chat-reset))
            (expect (buffer-live-p a) :to-be nil)
            (expect 'eca-chat--new-chat :to-have-been-called-with session))
        (eca-kill-test--kill-all a))))

  (it "does not start a new chat when the kill is declined"
    ;; Some other `kill-buffer-query-functions' entry (e.g. a
    ;; third-party package) can still decline the kill; the reset must
    ;; not replace a chat that survived.
    (let ((session (make-eca--session)) a)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-assert-session-running)
      (spy-on 'eca-chat--new-chat)
      (spy-on 'eca-chat--force-tab-line-update)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A"))
            (eca-kill-test--install-kill-hooks a)
            (with-current-buffer a
              (add-hook 'kill-buffer-query-functions #'ignore nil t))
            (setf (eca--session-last-chat-buffer session) a)
            (let ((this-command #'eca-chat-reset))
              (eca-chat-reset))
            (expect (buffer-live-p a) :to-be t)
            (expect (eca-get (eca--session-chats session) "A") :to-be a)
            (expect 'eca-chat--new-chat :not :to-have-been-called))
        (when (buffer-live-p a)
          (with-current-buffer a
            (remove-hook 'kill-buffer-query-functions #'ignore t)))
        (eca-kill-test--kill-all a)))))

(provide 'eca-chat-kill-test)
;;; eca-chat-kill-test.el ends here
