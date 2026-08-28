;;; eca-workspaces-test.el --- Tests for eca-workspaces -*- lexical-binding: t; -*-
;;; Commentary:
;; Tests for the `eca-workspaces' dashboard: rendering (sorting, chat
;; rows, status glyphs, hints, costs), folding, point preservation,
;; navigation, actions (delete confirmation) and live-refresh wiring.
;;; Code:
(require 'buttercup)
(require 'cl-lib)
(require 'eca)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defvar eca-workspaces-test--buffers '()
  "Buffers created by the fakes, killed on cleanup.")

(defvar eca-workspaces-test--next-id 0)

(cl-defun eca-workspaces-test--make-chat (&key title loading cost approval
                                               question model variant
                                               duration-secs)
  "Create a fake chat buffer with the given buffer-local state."
  (let ((buffer (generate-new-buffer " *test-eca-chat*")))
    (push buffer eca-workspaces-test--buffers)
    (with-current-buffer buffer
      (setq-local eca-chat--id (format "chat-%d"
                                       (cl-incf eca-workspaces-test--next-id)))
      (setq-local eca-chat--title title)
      (setq-local eca-chat--chat-loading loading)
      (setq-local eca-chat--session-cost cost)
      (setq-local eca-chat--selected-model (or model "test-model"))
      (setq-local eca-chat--selected-agent "code")
      (setq-local eca-chat--selected-variant variant)
      (setq-local eca-chat--turn-duration-secs duration-secs)
      (when loading
        (setq-local eca-chat--prompt-start-time (current-time)))
      (when question
        (setq-local eca-chat--pending-question '(:question "q")))
      (when approval
        (setq-local eca-chat--pending-approval-tool-calls
                    (make-hash-table :test 'equal))
        (puthash (cons eca-chat--id "tool-1") t
                 eca-chat--pending-approval-tool-calls)))
    buffer))

(defun eca-workspaces-test--make-session (id folder chats)
  "Create and register a fake session ID at FOLDER with CHATS.
CHATS is a list of chat buffers ordered oldest-first."
  (let ((session (make-eca--session)))
    (setf (eca--session-id session) id)
    (setf (eca--session-workspace-folders session) (list folder))
    (dolist (buffer chats)
      (setf (eca--session-chats session)
            (eca-assoc (eca--session-chats session)
                       (buffer-local-value 'eca-chat--id buffer)
                       buffer)))
    (setq eca--sessions (eca-assoc eca--sessions id session))
    session))

(defun eca-workspaces-test--render ()
  "Render the dashboard buffer and return it."
  (let ((buffer (get-buffer-create eca-workspaces-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'eca-workspaces-mode)
        (eca-workspaces-mode))
      (eca-workspaces--render))
    buffer))

(defun eca-workspaces-test--content ()
  "Return the rendered dashboard content as a plain string."
  (with-current-buffer (eca-workspaces-test--render)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun eca-workspaces-test--goto (text)
  "Move point to the start of the first occurrence of TEXT."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0)))

(defun eca-workspaces-test--cleanup ()
  "Reset global state touched by the tests."
  (setq eca--sessions '())
  (when-let* ((buffer (get-buffer eca-workspaces-buffer-name)))
    (kill-buffer buffer))
  (dolist (buffer eca-workspaces-test--buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (setq eca-workspaces-test--buffers '()))

;; ---------------------------------------------------------------------------
;; eca-workspaces-mode
;; ---------------------------------------------------------------------------

(describe "eca-workspaces-mode"

  (it "derives from special-mode"
    (with-temp-buffer
      (eca-workspaces-mode)
      (expect (derived-mode-p 'special-mode) :to-be-truthy)))

  (it "inherits q to close the window from special-mode"
    (with-temp-buffer
      (eca-workspaces-mode)
      (expect (key-binding (kbd "q")) :to-be 'quit-window)))

  (it "binds navigation and action keys"
    (expect (lookup-key eca-workspaces-mode-map (kbd "RET"))
            :to-be 'eca-workspaces-visit-or-toggle)
    (expect (lookup-key eca-workspaces-mode-map (kbd "TAB"))
            :to-be 'eca-workspaces-toggle-line)
    (expect (lookup-key eca-workspaces-mode-map (kbd "n"))
            :to-be 'eca-workspaces-next-entry)
    (expect (lookup-key eca-workspaces-mode-map (kbd "p"))
            :to-be 'eca-workspaces-previous-entry)
    (expect (lookup-key eca-workspaces-mode-map (kbd "N"))
            :to-be 'eca-workspaces-next-workspace)
    (expect (lookup-key eca-workspaces-mode-map (kbd "P"))
            :to-be 'eca-workspaces-previous-workspace)
    (expect (lookup-key eca-workspaces-mode-map (kbd "+"))
            :to-be 'eca-workspaces-new-chat)
    (expect (lookup-key eca-workspaces-mode-map (kbd "d"))
            :to-be 'eca-workspaces-delete)
    (expect (lookup-key eca-workspaces-mode-map (kbd "DEL"))
            :to-be 'eca-workspaces-delete)
    (expect (lookup-key eca-workspaces-mode-map (kbd "r"))
            :to-be 'eca-workspaces-rename-chat)
    (expect (lookup-key eca-workspaces-mode-map (kbd "f"))
            :to-be 'eca-workspaces-fork-chat)
    (expect (lookup-key eca-workspaces-mode-map (kbd "m"))
            :to-be 'eca-workspaces-select-model)
    (expect (lookup-key eca-workspaces-mode-map (kbd "v"))
            :to-be 'eca-workspaces-select-variant)
    (expect (lookup-key eca-workspaces-mode-map (kbd "C"))
            :to-be 'eca-workspaces-compact-chat)
    (expect (lookup-key eca-workspaces-mode-map (kbd "a"))
            :to-be 'eca-workspaces-tool-call-accept-next)
    (expect (lookup-key eca-workspaces-mode-map (kbd "A"))
            :to-be 'eca-workspaces-tool-call-accept-all)
    (expect (lookup-key eca-workspaces-mode-map (kbd "x"))
            :to-be 'eca-workspaces-tool-call-reject-next)
    (expect (lookup-key eca-workspaces-mode-map (kbd "s"))
            :to-be 'eca-workspaces-stop-prompt)
    (expect (lookup-key eca-workspaces-mode-map (kbd "R"))
            :to-be 'eca-workspaces-resume)
    (expect (lookup-key eca-workspaces-mode-map (kbd "?"))
            :to-be 'eca-workspaces-menu)))

;; ---------------------------------------------------------------------------
;; rendering
;; ---------------------------------------------------------------------------

(describe "eca-workspaces rendering"

  (after-each (eca-workspaces-test--cleanup))

  (it "shows an empty state when no session is running"
    (expect (eca-workspaces-test--content)
            :to-match "No ECA session running"))

  (it "sorts workspaces alphabetically by project name"
    (eca-workspaces-test--make-session 1 "/tmp/zeta" '())
    (eca-workspaces-test--make-session 2 "/tmp/alpha" '())
    (let ((content (eca-workspaces-test--content)))
      (expect (string-match-p "alpha" content) :to-be-truthy)
      (expect (< (string-match "alpha" content)
                 (string-match "zeta" content))
              :to-be-truthy)))

  (it "renders chats oldest-first under their workspace"
    (eca-workspaces-test--make-session
     1 "/tmp/proj"
     (list (eca-workspaces-test--make-chat :title "First chat")
           (eca-workspaces-test--make-chat :title "Second chat")))
    (let ((content (eca-workspaces-test--content)))
      (expect (< (string-match "First chat" content)
                 (string-match "Second chat" content))
              :to-be-truthy)))

  (it "renders the chat count and total session cost"
    (eca-workspaces-test--make-session
     1 "/tmp/proj"
     (list (eca-workspaces-test--make-chat :title "A" :cost "0.05")
           (eca-workspaces-test--make-chat :title "B" :cost "0.05")))
    (let ((content (eca-workspaces-test--content)))
      (expect content :to-match "2 chats")
      (expect content :to-match (regexp-quote "$0.10 session"))))

  (it "renders status glyphs per chat state"
    (eca-workspaces-test--make-session
     1 "/tmp/proj"
     (list (eca-workspaces-test--make-chat :title "Running" :loading t)
           (eca-workspaces-test--make-chat :title "Approval" :approval t)
           (eca-workspaces-test--make-chat :title "Question" :question t)
           (eca-workspaces-test--make-chat :title "Idle")))
    (let ((content (eca-workspaces-test--content)))
      (expect content :to-match "⏳ *Running")
      (expect content :to-match "🚧 *Approval")
      (expect content :to-match "❓ *Question")))

  (it "renders attention hints"
    (eca-workspaces-test--make-session
     1 "/tmp/proj"
     (list (eca-workspaces-test--make-chat :title "Approval" :approval t)
           (eca-workspaces-test--make-chat :title "Question" :question t)))
    (let ((content (eca-workspaces-test--content)))
      (expect content :to-match "approval pending")
      (expect content :to-match "waiting your answer")))

  (it "tints the whole row of a chat pending approval"
    (eca-workspaces-test--make-session
     1 "/tmp/proj"
     (list (eca-workspaces-test--make-chat :title "Approval" :approval t)
           (eca-workspaces-test--make-chat :title "Question" :question t)))
    (with-current-buffer (eca-workspaces-test--render)
      (eca-workspaces-test--goto "Approval")
      (let ((face (get-text-property (line-beginning-position) 'face)))
        (expect (memq 'eca-chat-approval-modeline-face
                      (if (listp face) face (list face)))
                :to-be-truthy))
      (eca-workspaces-test--goto "Question")
      (let ((face (get-text-property (line-beginning-position) 'face)))
        (expect (memq 'eca-chat-approval-modeline-face
                      (if (listp face) face (list face)))
                :to-be nil))))

  (it "renders elapsed time, cost and model of a chat"
    (eca-workspaces-test--make-session
     1 "/tmp/proj"
     (list (eca-workspaces-test--make-chat :title "Idle"
                                           :cost "0.05"
                                           :duration-secs 83
                                           :variant "high")))
    (let ((content (eca-workspaces-test--content)))
      (expect content :to-match "⏱ 1m 23s")
      (expect content :to-match (regexp-quote "$0.05"))
      (expect content :to-match (regexp-quote "test-model[high] · code"))))

  (it "never truncates the model, only the title"
    (eca-workspaces-test--make-session
     1 "/tmp/proj"
     (list (eca-workspaces-test--make-chat
            :title "A very long chat title that goes beyond the column"
            :model "a-very-long-model-name-that-exceeds-the-cell-width")))
    (let ((content (eca-workspaces-test--content)))
      (expect content
              :to-match
              (regexp-quote "a-very-long-model-name-that-exceeds-the-cell-width"))
      (expect content :to-match "A very long chat title.*…")))

  (it "skips killed chat buffers"
    (let ((chat (eca-workspaces-test--make-chat :title "Doomed chat")))
      (eca-workspaces-test--make-session 1 "/tmp/proj" (list chat))
      (kill-buffer chat)
      (let ((content (eca-workspaces-test--content)))
        (expect (string-match-p "Doomed chat" content) :to-be nil)
        (expect content :to-match "0 chats"))))

  (it "renders the actions footer hint"
    (expect (eca-workspaces-test--content)
            :to-match "type \\? for actions")))

;; ---------------------------------------------------------------------------
;; folding
;; ---------------------------------------------------------------------------

(describe "eca-workspaces-toggle-line"

  (after-each (eca-workspaces-test--cleanup))

  (it "folds and unfolds a workspace, persisting across renders"
    (eca-workspaces-test--make-session
     1 "/tmp/proj"
     (list (eca-workspaces-test--make-chat :title "Chat A")))
    (with-current-buffer (eca-workspaces-test--render)
      (expect (buffer-string) :to-match "Chat A")
      (eca-workspaces-test--goto "proj")
      (eca-workspaces-toggle-line)
      (expect (buffer-string) :not :to-match "Chat A")
      (expect (buffer-string) :to-match "▶")
      ;; Persists across a full re-render.
      (eca-workspaces--render)
      (expect (buffer-string) :not :to-match "Chat A")
      (eca-workspaces-test--goto "proj")
      (eca-workspaces-toggle-line)
      (expect (buffer-string) :to-match "Chat A")))

  (it "does nothing on a chat line"
    (eca-workspaces-test--make-session
     1 "/tmp/proj"
     (list (eca-workspaces-test--make-chat :title "Chat A")))
    (with-current-buffer (eca-workspaces-test--render)
      (eca-workspaces-test--goto "Chat A")
      (let ((before (buffer-string)))
        (expect (eca-workspaces-toggle-line) :to-be nil)
        (expect (buffer-string) :to-equal before)))))

;; ---------------------------------------------------------------------------
;; point preservation
;; ---------------------------------------------------------------------------

(describe "eca-workspaces point preservation"

  (after-each (eca-workspaces-test--cleanup))

  (it "keeps point on the same chat and column across re-renders"
    (let ((chat-b (eca-workspaces-test--make-chat :title "Chat B")))
      (eca-workspaces-test--make-session
       1 "/tmp/proj"
       (list (eca-workspaces-test--make-chat :title "Chat A") chat-b))
      (with-current-buffer (eca-workspaces-test--render)
        (eca-workspaces-test--goto "Chat B")
        (forward-char 4)
        (let ((chat-id (buffer-local-value 'eca-chat--id chat-b))
              (column (current-column)))
          (expect (eca-workspaces--chat-id-at-point) :to-equal chat-id)
          (eca-workspaces--render)
          (expect (eca-workspaces--chat-id-at-point) :to-equal chat-id)
          (expect (current-column) :to-equal column)))))

  (it "falls back to buffer start when the entity is gone"
    (let ((chat (eca-workspaces-test--make-chat :title "Chat A")))
      (eca-workspaces-test--make-session 1 "/tmp/proj" (list chat))
      (with-current-buffer (eca-workspaces-test--render)
        (eca-workspaces-test--goto "Chat A")
        (kill-buffer chat)
        (eca-workspaces--render)
        (expect (point) :to-equal (point-min))))))

;; ---------------------------------------------------------------------------
;; navigation
;; ---------------------------------------------------------------------------

(describe "eca-workspaces navigation"

  (after-each (eca-workspaces-test--cleanup))

  (it "moves across entries with n/p and workspaces with N/P"
    (eca-workspaces-test--make-session
     1 "/tmp/aaa"
     (list (eca-workspaces-test--make-chat :title "Chat A")))
    (eca-workspaces-test--make-session
     2 "/tmp/bbb"
     (list (eca-workspaces-test--make-chat :title "Chat B")))
    (with-current-buffer (eca-workspaces-test--render)
      (goto-char (point-min))
      (expect (eca-workspaces--session-line-p) :to-be-truthy)
      ;; n: workspace aaa -> chat A -> workspace bbb -> chat B -> stay
      (eca-workspaces-next-entry)
      (expect (eca-workspaces--chat-id-at-point) :to-be-truthy)
      (eca-workspaces-next-entry)
      (expect (eca-workspaces--session-line-p) :to-be-truthy)
      (eca-workspaces-next-entry)
      (expect (eca-workspaces--chat-id-at-point) :to-be-truthy)
      (eca-workspaces-next-entry)
      (expect (eca-workspaces--chat-id-at-point) :to-be-truthy)
      ;; N/P: jump between workspace lines only.
      (goto-char (point-min))
      (eca-workspaces-next-workspace)
      (expect (eca-workspaces--session-id-at-point) :to-equal 2)
      (eca-workspaces-previous-workspace)
      (expect (eca-workspaces--session-id-at-point) :to-equal 1)
      (eca-workspaces-previous-workspace)
      (expect (eca-workspaces--session-id-at-point) :to-equal 1))))

;; ---------------------------------------------------------------------------
;; actions
;; ---------------------------------------------------------------------------

(describe "eca-workspaces-visit-or-toggle"

  (after-each (eca-workspaces-test--cleanup))

  (it "opens the chat at point"
    (let ((chat (eca-workspaces-test--make-chat :title "Chat A"))
          (opened nil))
      (eca-workspaces-test--make-session 1 "/tmp/proj" (list chat))
      (with-current-buffer (eca-workspaces-test--render)
        (cl-letf (((symbol-function 'eca-workspaces--open-chat)
                   (lambda (buffer) (setq opened buffer))))
          (eca-workspaces-test--goto "Chat A")
          (eca-workspaces-visit-or-toggle)
          (expect opened :to-be chat)))))

  (it "folds the workspace on a workspace line"
    (eca-workspaces-test--make-session
     1 "/tmp/proj"
     (list (eca-workspaces-test--make-chat :title "Chat A")))
    (with-current-buffer (eca-workspaces-test--render)
      (eca-workspaces-test--goto "proj")
      (eca-workspaces-visit-or-toggle)
      (expect (buffer-string) :not :to-match "Chat A"))))

(describe "eca-workspaces-delete"

  (after-each (eca-workspaces-test--cleanup))

  (it "deletes the chat at point after confirmation"
    (let ((chat (eca-workspaces-test--make-chat :title "Chat A"))
          (deleted-in nil))
      (eca-workspaces-test--make-session 1 "/tmp/proj" (list chat))
      (with-current-buffer (eca-workspaces-test--render)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                  ((symbol-function 'eca-chat-delete)
                   (lambda () (setq deleted-in (current-buffer)))))
          (eca-workspaces-test--goto "Chat A")
          (eca-workspaces-delete)
          (expect deleted-in :to-be chat)))))

  (it "does nothing when the confirmation is denied"
    (let ((chat (eca-workspaces-test--make-chat :title "Chat A"))
          (deleted-in nil))
      (eca-workspaces-test--make-session 1 "/tmp/proj" (list chat))
      (with-current-buffer (eca-workspaces-test--render)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil))
                  ((symbol-function 'eca-chat-delete)
                   (lambda () (setq deleted-in (current-buffer)))))
          (eca-workspaces-test--goto "Chat A")
          (eca-workspaces-delete)
          (expect deleted-in :to-be nil)))))

  (it "stops the workspace session at point after confirmation"
    (let ((stopped nil))
      (eca-workspaces-test--make-session 1 "/tmp/proj" '())
      (with-current-buffer (eca-workspaces-test--render)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                  ((symbol-function 'eca-stop-session)
                   (lambda (session) (setq stopped session))))
          (eca-workspaces-test--goto "proj")
          (eca-workspaces-delete)
          (expect (eca--session-id stopped) :to-equal 1))))))

;; ---------------------------------------------------------------------------
;; live updates
;; ---------------------------------------------------------------------------

(describe "eca-workspaces live updates"

  (after-each
    (when (timerp eca-workspaces--refresh-timer)
      (cancel-timer eca-workspaces--refresh-timer))
    (setq eca-workspaces--refresh-timer nil)
    (eca-workspaces-test--cleanup))

  (it "subscribes to chat status and sessions updated hooks"
    (expect (memq #'eca-workspaces--schedule-refresh
                  eca-chat-session-status-changed-functions)
            :to-be-truthy)
    (expect (memq #'eca-workspaces--schedule-refresh
                  eca-sessions-updated-hook)
            :to-be-truthy))

  (it "schedules a debounced refresh when the buffer exists"
    (eca-workspaces-test--render)
    (eca-workspaces--schedule-refresh)
    (expect (timerp eca-workspaces--refresh-timer) :to-be-truthy))

  (it "does not schedule a refresh without a dashboard buffer"
    (eca-workspaces--schedule-refresh)
    (expect eca-workspaces--refresh-timer :to-be nil)))

;;; eca-workspaces-test.el ends here
