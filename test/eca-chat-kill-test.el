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

(defun eca-kill-test--quit (&rest _)
  "Signal `quit', simulating the user quitting the prompt.
The real prompt may raise `minibuffer-quit' instead, which derives
from `quit' and is handled by the same `condition-case'."
  (signal 'quit nil))

(defun eca-kill-test--install-kill-hooks (buffer)
  "Install eca's kill query function and kill hook on BUFFER.
Mirrors what `eca-chat-mode' registers, so tests exercise the real
two-phase flow: confirmation in `kill-buffer-query-functions' and
cleanup in `kill-buffer-hook'."
  (with-current-buffer buffer
    (add-hook 'kill-buffer-query-functions #'eca-chat--kill-buffer-query nil t)
    (add-hook 'kill-buffer-hook #'eca-chat--delete-chat nil t)))

(defun eca-kill-test--kill-all (&rest buffers)
  "Kill every live buffer in BUFFERS, ignoring the eca kill hooks.
The hooks are removed buffer-locally rather than let-bound: binding
`kill-buffer-hook' / `kill-buffer-query-functions' only shadows
their global value and would still run buffer-local entries, which
here would call the real `yes-or-no-p' and block the test run."
  (dolist (buf buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (remove-hook 'kill-buffer-query-functions
                     #'eca-chat--kill-buffer-query t)
        (remove-hook 'kill-buffer-hook #'eca-chat--delete-chat t))
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
      (spy-on 'yes-or-no-p :and-return-value nil)
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

  (it "does not prompt or touch the registry for an already-closed chat"
    (let ((session (make-eca--session)) a)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'yes-or-no-p :and-return-value nil)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A"))
            (with-current-buffer a
              (setq-local eca-chat--closed t))
            (eca-kill-test--install-kill-hooks a)
            (let ((this-command #'kill-buffer))
              (kill-buffer a))
            (expect 'yes-or-no-p :not :to-have-been-called))
        (eca-kill-test--kill-all a))))

  (it "deletes the chat server side when the prompt is answered yes"
    (let ((session (make-eca--session)) a b)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'yes-or-no-p :and-return-value t)
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
            (expect 'eca-api-request-sync :to-have-been-called))
        (eca-kill-test--kill-all a b))))

  (it "only kills the buffer when the prompt is answered no"
    (let ((session (make-eca--session)) a b)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'yes-or-no-p :and-return-value nil)
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

  (it "clears the echo area so the answered prompt does not linger"
    ;; `yes-or-no-p' echoes the prompt back together with the answer,
    ;; and once the chat buffer is gone nothing redisplays over it, so
    ;; the stale prompt stayed on screen after a plain buffer kill.
    (let ((session (make-eca--session)) a b)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'yes-or-no-p :and-return-value nil)
      (spy-on 'eca-api-request-sync)
      (spy-on 'eca-chat--force-tab-line-update)
      (spy-on 'message)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A")
                  b (eca-kill-test--make-chat session "B"))
            (eca-kill-test--install-kill-hooks b)
            (let ((this-command #'kill-buffer))
              (kill-buffer b))
            (expect 'message :to-have-been-called-with nil))
        (eca-kill-test--kill-all a b)))))

;; ---------------------------------------------------------------------------
;; Cancelling the kill by quitting the prompt
;; ---------------------------------------------------------------------------

(describe "cancelling a chat kill by quitting the prompt"

  ;; The confirmation used to run from `kill-buffer-hook', after the
  ;; window had already been switched away and the chat dropped from the
  ;; registry.  Quitting then left the buffer alive but orphaned, which
  ;; looked exactly like a successful kill.  Quitting must now be a
  ;; complete no-op.

  (it "keeps the buffer alive, registered and displayed"
    (let ((session (make-eca--session)) a b)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'yes-or-no-p :and-call-fake #'eca-kill-test--quit)
      (spy-on 'eca-api-request-sync)
      (spy-on 'eca-chat--force-tab-line-update)
      (spy-on 'message)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A")
                  b (eca-kill-test--make-chat session "B"))
            (eca-kill-test--install-kill-hooks b)
            (set-window-buffer (selected-window) b)
            (let ((this-command #'kill-buffer))
              (kill-buffer b))
            ;; Nothing at all happened, and the echo area was cleaned up.
            (expect (buffer-live-p b) :to-be t)
            (expect 'message :to-have-been-called-with nil)
            (expect (eca-get (eca--session-chats session) "B") :to-be b)
            (expect (window-buffer (selected-window)) :to-be b)
            (expect 'eca-api-request-sync :not :to-have-been-called)
            (expect 'eca-chat--force-tab-line-update :not :to-have-been-called))
        (eca-kill-test--kill-all a b))))

  (it "leaves the chat killable again afterwards"
    (let ((session (make-eca--session)) a b)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-api-request-sync)
      (spy-on 'eca-chat--force-tab-line-update)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A")
                  b (eca-kill-test--make-chat session "B"))
            (eca-kill-test--install-kill-hooks b)
            (set-window-buffer (selected-window) b)
            ;; First attempt: cancelled.
            (spy-on 'yes-or-no-p :and-call-fake #'eca-kill-test--quit)
            (let ((this-command #'kill-buffer))
              (kill-buffer b))
            (expect (buffer-live-p b) :to-be t)
            ;; Second attempt: confirmed, and the stale "delete server
            ;; side" answer from the cancelled attempt is not reused.
            (spy-on 'yes-or-no-p :and-return-value nil)
            (let ((this-command #'kill-buffer))
              (kill-buffer b))
            (expect (buffer-live-p b) :to-be nil)
            (expect (eca-get (eca--session-chats session) "B") :to-be nil)
            (expect (window-buffer (selected-window)) :to-be a)
            (expect 'eca-api-request-sync :not :to-have-been-called))
        (eca-kill-test--kill-all a b)))))

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
      (spy-on 'yes-or-no-p :and-return-value nil)
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
      (spy-on 'yes-or-no-p :and-return-value nil)
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

  (it "does not start a new chat when the kill is cancelled"
    ;; Cancelling used to abort `eca-chat-reset' by signalling out of
    ;; `kill-buffer-hook'.  Now the kill is declined normally, so the
    ;; command keeps running and must not replace a chat that is still
    ;; alive.
    (let ((session (make-eca--session)) a)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-assert-session-running)
      (spy-on 'eca-chat--new-chat)
      (spy-on 'yes-or-no-p :and-call-fake #'eca-kill-test--quit)
      (spy-on 'eca-chat--force-tab-line-update)
      (unwind-protect
          (progn
            (setq a (eca-kill-test--make-chat session "A"))
            (eca-kill-test--install-kill-hooks a)
            (setf (eca--session-last-chat-buffer session) a)
            (let ((this-command #'eca-chat-reset))
              (eca-chat-reset))
            (expect (buffer-live-p a) :to-be t)
            (expect (eca-get (eca--session-chats session) "A") :to-be a)
            (expect 'eca-chat--new-chat :not :to-have-been-called))
        (eca-kill-test--kill-all a)))))

(provide 'eca-chat-kill-test)
;;; eca-chat-kill-test.el ends here
