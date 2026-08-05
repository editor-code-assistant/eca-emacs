;;; eca-workspaces.el --- ECA workspaces dashboard -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Live dashboard to manage all running ECA sessions and their chats:
;;  see each chat status, model and cost, and act on them (open, create,
;;  rename, fork, compact, approve tool calls, etc).
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'transient nil t)

(require 'eca-util)
(require 'eca-chat)

(declare-function eca-stop-session "eca")

(defface eca-workspaces-tree-chat-idle-face
  '((t :underline t))
  "Face for idle chat entries in eca-workspaces buffer."
  :group 'eca)

(defface eca-workspaces-tree-chat-loading-face
  '((t :inherit warning :underline t))
  "Face for loading chat entries in eca-workspaces buffer."
  :group 'eca)

(defface eca-workspaces-tree-chat-details-face
  '((t :inherit shadow :height 0.8))
  "Face for chat details in entries in eca-workspaces buffer."
  :group 'eca)

(defcustom eca-workspaces-chat-segments
  '((status . 3)
    (title . 36)
    (elapsed . 11)
    (cost . 9)
    (model . 24)
    (hint . nil))
  "Segments rendered for each chat row and their padded widths.
Each entry is (SEGMENT . WIDTH).  SEGMENT is one of the built-in
symbols `status', `title', `elapsed', `cost', `model', `hint', or
a function called with the chat buffer that returns a string.
WIDTH nil means render unpadded."
  :type '(alist :key-type (choice symbol function)
                :value-type (choice integer (const nil)))
  :group 'eca)

(defcustom eca-workspaces-workspace-width 58
  "Width of the workspace label part of a workspace line."
  :type 'integer
  :group 'eca)

;; Internal

(defvar eca-workspaces-buffer-name "*eca-workspaces*")

(defvar eca-workspaces--refresh-timer nil
  "Debounce timer for live dashboard refreshes.")

(defvar eca-workspaces--ticker-timer nil
  "Repeating timer updating elapsed times while chats are running.")

(defvar-local eca-workspaces--folded-sessions '()
  "List of session ids currently folded in the dashboard.")

(defun eca-workspaces--cell (text width &optional truncate)
  "Return TEXT padded to WIDTH columns.
When TRUNCATE is non-nil, TEXT wider than WIDTH is truncated with
an ellipsis.  When WIDTH is nil return TEXT unchanged.  Padding
spaces carry no face so underlined cells do not bleed into the
whitespace."
  (if (null width)
      text
    (let ((text (if (and truncate (> (string-width text) (1- width)))
                    (truncate-string-to-width text (- width 2) nil nil "…")
                  text)))
      (concat text
              (make-string (max 1 (- width (string-width text))) ?\s)))))

(defun eca-workspaces--chat-status-glyph (status)
  "Return the glyph shown for a chat with STATUS."
  (pcase status
    ('waiting-approval "🚧")
    ('waiting-answer "❓")
    ('running "⏳")
    (_ "")))

(defun eca-workspaces--chat-hint (status)
  "Return the attention hint string for STATUS, or nil."
  (pcase status
    ('waiting-approval (propertize "approval pending" 'face 'warning))
    ('waiting-answer (propertize "waiting your answer"
                                 'face 'eca-chat-question-face))))

(defun eca-workspaces--chat-model-str ()
  "Return the model[variant] · agent string for the current chat."
  (let ((model (eca-chat--model))
        (variant (eca-chat--variant))
        (agent (eca-chat--agent)))
    (concat (or model "")
            (when variant (format "[%s]" variant))
            (when agent (concat (when model " · ") agent)))))

(defun eca-workspaces--chat-segment (segment chat-buffer status)
  "Return the string of SEGMENT for CHAT-BUFFER with STATUS."
  (with-current-buffer chat-buffer
    (pcase segment
      ('status (eca-workspaces--chat-status-glyph status))
      ('title
       (eca-buttonize
        nil
        (propertize (substring-no-properties (eca-chat-title))
                    'face (if (eq status 'running)
                              'eca-workspaces-tree-chat-loading-face
                            'eca-workspaces-tree-chat-idle-face))
        (lambda () (eca-workspaces--open-chat chat-buffer))))
      ('elapsed
       (if-let* ((elapsed (eca-chat-elapsed-str chat-buffer)))
           (propertize (concat "⏱ " elapsed)
                       'face 'eca-chat-elapsed-time-face)
         ""))
      ('cost
       (if eca-chat--session-cost
           (propertize (concat "$" eca-chat--session-cost)
                       'face 'eca-chat-usage-string-face)
         ""))
      ('model
       (propertize (eca-workspaces--chat-model-str)
                   'face 'eca-workspaces-tree-chat-details-face))
      ('hint (or (eca-workspaces--chat-hint status) ""))
      (_ (if (functionp segment)
             (or (funcall segment chat-buffer) "")
           "")))))

(defun eca-workspaces--chat-line (session chat-buffer)
  "Return the rendered dashboard line for CHAT-BUFFER of SESSION."
  (let* ((status (eca-chat-status chat-buffer))
         (line (concat
                "   "
                (mapconcat
                 (lambda (segment-and-width)
                   (eca-workspaces--cell
                    (eca-workspaces--chat-segment (car segment-and-width)
                                                  chat-buffer
                                                  status)
                    (cdr segment-and-width)
                    ;; Only titles truncate; other cells show in full.
                    (eq (car segment-and-width) 'title)))
                 eca-workspaces-chat-segments
                 ""))))
    (add-text-properties
     0 (length line)
     (list 'eca-workspaces-session-id (eca--session-id session)
           'eca-workspaces-chat-id (buffer-local-value 'eca-chat--id
                                                       chat-buffer))
     line)
    (when (eq status 'waiting-approval)
      (add-face-text-property 0 (length line)
                              'eca-chat-approval-modeline-face
                              t line))
    (concat line "\n")))

(defun eca-workspaces--session-cost (session)
  "Return the total cost of all SESSION chats, or nil."
  (let ((total (-sum (-map (lambda (buffer)
                             (string-to-number
                              (or (buffer-local-value 'eca-chat--session-cost
                                                      buffer)
                                  "0")))
                           (eca-chat-buffers session)))))
    (when (> total 0) total)))

(defun eca-workspaces--workspace-line (session)
  "Return the rendered dashboard line for SESSION."
  (let* ((session-id (eca--session-id session))
         (folded (memq session-id eca-workspaces--folded-sessions))
         (chats-count (length (eca-chat-buffers session)))
         (label (eca-buttonize
                 nil
                 (concat
                  (propertize (if folded "▶" "▼") 'face 'shadow)
                  " "
                  (propertize (eca--session-project-name session)
                              'face 'bold)
                  (propertize (concat "  "
                                      (string-join
                                       (-map #'abbreviate-file-name
                                             (eca--session-workspace-folders session))
                                       ", "))
                              'face 'eca-workspaces-tree-chat-details-face))
                 (lambda () (eca-workspaces--toggle-session session-id))))
         (summary (propertize
                   (concat
                    (format "%d chat%s" chats-count
                            (if (= 1 chats-count) "" "s"))
                    (when-let* ((cost (eca-workspaces--session-cost session)))
                      (format " · $%.2f session" cost)))
                   'face 'eca-workspaces-tree-chat-details-face))
         (line (concat (eca-workspaces--cell
                        label eca-workspaces-workspace-width t)
                       summary)))
    (add-text-properties 0 (length line)
                         (list 'eca-workspaces-session-id session-id
                               'eca-workspaces-session t)
                         line)
    (concat line "\n")))

(defun eca-workspaces--sorted-sessions ()
  "Return all sessions sorted alphabetically by project name."
  (sort (copy-sequence (eca-vals eca--sessions))
        (lambda (a b)
          (string-lessp (downcase (eca--session-project-name a))
                        (downcase (eca--session-project-name b))))))

(defun eca-workspaces--entity-at (pos)
  "Return the (SESSION-ID . CHAT-ID) entity of the line at POS, or nil."
  (when (and pos (>= pos (point-min)) (<= pos (point-max)))
    (save-excursion
      (goto-char pos)
      (let ((session-id (get-text-property (line-beginning-position)
                                           'eca-workspaces-session-id)))
        (when session-id
          (cons session-id
                (get-text-property (line-beginning-position)
                                   'eca-workspaces-chat-id)))))))

(defun eca-workspaces--find-entity (entity)
  "Return the position of the line showing ENTITY, or nil."
  (when entity
    (save-excursion
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (if (and (equal (car entity)
                          (get-text-property (point) 'eca-workspaces-session-id))
                   (equal (cdr entity)
                          (get-text-property (point) 'eca-workspaces-chat-id)))
              (setq found (point))
            (forward-line 1)))
        found))))

(defun eca-workspaces--restore-point (entity column)
  "Move point to ENTITY line at COLUMN, falling back to buffer start."
  (if-let* ((position (eca-workspaces--find-entity entity)))
      (progn
        (goto-char position)
        (move-to-column (or column 0)))
    (goto-char (point-min))))

(defun eca-workspaces--footer ()
  "Return the dashboard footer hint line."
  (concat "\n "
          (propertize "type " 'face 'eca-workspaces-tree-chat-details-face)
          (propertize "?" 'face (if (facep 'help-key-binding)
                                    'help-key-binding
                                  'bold))
          (propertize " for actions"
                      'face 'eca-workspaces-tree-chat-details-face)
          "\n"))

(defun eca-workspaces--window-state (window)
  "Return (WINDOW ENTITY COLUMN) for WINDOW's current point."
  (let ((position (window-point window)))
    (list window
          (eca-workspaces--entity-at position)
          (save-excursion
            (goto-char position)
            (current-column)))))

(defun eca-workspaces--render ()
  "Render the dashboard into the current buffer.
Preserves point, per-window points and fold state across renders."
  (let* ((inhibit-read-only t)
         (point-entity (eca-workspaces--entity-at (point)))
         (point-column (current-column))
         (window-states (-map #'eca-workspaces--window-state
                              (get-buffer-window-list nil nil t))))
    (erase-buffer)
    (let ((sessions (eca-workspaces--sorted-sessions)))
      (if (null sessions)
          (insert (propertize "No ECA session running, start one with M-x eca"
                              'face 'eca-workspaces-tree-chat-details-face)
                  "\n")
        (dolist (session sessions)
          (insert (eca-workspaces--workspace-line session))
          (unless (memq (eca--session-id session)
                        eca-workspaces--folded-sessions)
            (dolist (chat-buffer (eca-chat-buffers session))
              (insert (eca-workspaces--chat-line session chat-buffer)))))))
    (insert (eca-workspaces--footer))
    (eca-workspaces--restore-point point-entity point-column)
    (dolist (state window-states)
      (-let [(window entity column) state]
        (when-let* ((position (eca-workspaces--find-entity entity)))
          (set-window-point window
                            (save-excursion
                              (goto-char position)
                              (move-to-column (or column 0))
                              (point))))))))

(defun eca-workspaces--any-chat-running-p ()
  "Return non-nil when any chat of any session is loading."
  (-any-p (lambda (session)
            (-any-p (lambda (buffer)
                      (eq (buffer-local-value 'eca-chat--chat-loading buffer)
                          t))
                    (eca-chat-buffers session)))
          (eca-vals eca--sessions)))

(defun eca-workspaces--stop-ticker ()
  "Cancel the elapsed-time ticker timer."
  (when (timerp eca-workspaces--ticker-timer)
    (cancel-timer eca-workspaces--ticker-timer))
  (setq eca-workspaces--ticker-timer nil))

(defun eca-workspaces--ensure-ticker ()
  "Start or stop the 1s ticker based on visibility and running chats."
  (let ((buffer (get-buffer eca-workspaces-buffer-name)))
    (if (and buffer
             (buffer-live-p buffer)
             (get-buffer-window buffer t)
             (eca-workspaces--any-chat-running-p))
        (unless (timerp eca-workspaces--ticker-timer)
          (setq eca-workspaces--ticker-timer
                (run-with-timer 1 1 #'eca-workspaces--tick)))
      (eca-workspaces--stop-ticker))))

(defun eca-workspaces--tick ()
  "Refresh the dashboard from the elapsed-time ticker."
  (let ((buffer (get-buffer eca-workspaces-buffer-name)))
    (if (and buffer (buffer-live-p buffer) (get-buffer-window buffer t))
        (eca-workspaces-refresh)
      (eca-workspaces--stop-ticker))))

(defun eca-workspaces--schedule-refresh (&rest _)
  "Schedule a debounced refresh of the dashboard buffer."
  (when (get-buffer eca-workspaces-buffer-name)
    (unless (timerp eca-workspaces--refresh-timer)
      (setq eca-workspaces--refresh-timer
            (run-with-timer 0.2 nil
                            (lambda ()
                              (setq eca-workspaces--refresh-timer nil)
                              (eca-workspaces-refresh)))))))

(defun eca-workspaces--on-kill ()
  "Clean up dashboard timers when the buffer is killed."
  (when (timerp eca-workspaces--refresh-timer)
    (cancel-timer eca-workspaces--refresh-timer))
  (setq eca-workspaces--refresh-timer nil)
  (eca-workspaces--stop-ticker))

;; Entities at point

(defun eca-workspaces--session-id-at-point ()
  "Return the session id of the line at point, or nil."
  (get-text-property (line-beginning-position) 'eca-workspaces-session-id))

(defun eca-workspaces--chat-id-at-point ()
  "Return the chat id of the line at point, or nil."
  (get-text-property (line-beginning-position) 'eca-workspaces-chat-id))

(defun eca-workspaces--session-line-p ()
  "Return non-nil if the current line is a workspace (session) entry."
  (and (eca-workspaces--session-id-at-point)
       (not (eca-workspaces--chat-id-at-point))))

(defun eca-workspaces--session-at-point ()
  "Return the session of the line at point, or nil."
  (when-let* ((session-id (eca-workspaces--session-id-at-point)))
    (eca-get eca--sessions session-id)))

(defun eca-workspaces--chat-buffer-at-point ()
  "Return the live chat buffer of the line at point, or nil."
  (when-let* ((session (eca-workspaces--session-at-point))
              (chat-id (eca-workspaces--chat-id-at-point))
              (buffer (eca-get (eca--session-chats session) chat-id)))
    (when (buffer-live-p buffer)
      buffer)))

(defun eca-workspaces--chat-buffer-at-point-or-error ()
  "Return the chat buffer at point or signal a `user-error'."
  (or (eca-workspaces--chat-buffer-at-point)
      (user-error "No chat at point")))

(defun eca-workspaces--open-chat (chat-buffer)
  "Open CHAT-BUFFER in the chat window, making it the last chat."
  (with-current-buffer chat-buffer
    (setf (eca--session-last-chat-buffer (eca-session)) chat-buffer)
    (eca-chat-open (eca-session))))

(defun eca-workspaces--toggle-session (session-id)
  "Fold or unfold the workspace of SESSION-ID."
  (setq eca-workspaces--folded-sessions
        (if (memq session-id eca-workspaces--folded-sessions)
            (delq session-id eca-workspaces--folded-sessions)
          (cons session-id eca-workspaces--folded-sessions)))
  (eca-workspaces-refresh))

;; Public

(defun eca-workspaces-refresh ()
  "Refresh the ECA workspaces buffer if it exists."
  (interactive)
  (when-let* ((buffer (get-buffer eca-workspaces-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless (derived-mode-p 'eca-workspaces-mode)
          (eca-workspaces-mode))
        (eca-workspaces--render))
      (eca-workspaces--ensure-ticker))))

(defun eca-workspaces-toggle-line (&optional event)
  "Fold or unfold the workspace on the current line.
Does nothing on a chat line, which has nothing to fold.
EVENT, when non-nil, is the mouse event that triggered the command."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (when (eca-workspaces--session-line-p)
    (eca-workspaces--toggle-session (eca-workspaces--session-id-at-point))
    t))

(defun eca-workspaces-visit-or-toggle (&optional event)
  "Switch to the chat on the current line, or toggle the workspace.
EVENT, when non-nil, is the mouse event that triggered the command."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (if-let* ((chat-buffer (eca-workspaces--chat-buffer-at-point)))
      (eca-workspaces--open-chat chat-buffer)
    (eca-workspaces-toggle-line)))

(defun eca-workspaces--next-line-matching (predicate)
  "Move to the next line matching PREDICATE, or stay put."
  (let ((origin (point))
        (found nil))
    (forward-line 1)
    (while (and (not (eobp)) (not found))
      (if (funcall predicate)
          (setq found t)
        (forward-line 1)))
    (unless found
      (goto-char origin))
    found))

(defun eca-workspaces--previous-line-matching (predicate)
  "Move to the previous line matching PREDICATE, or stay put."
  (let ((origin (point))
        (found nil))
    (beginning-of-line)
    (while (and (not (bobp)) (not found))
      (forward-line -1)
      (when (funcall predicate)
        (setq found t)))
    (unless found
      (goto-char origin))
    found))

(defun eca-workspaces-next-entry (&optional count)
  "Move point to the next workspace or chat entry.
With prefix COUNT, repeat that many times."
  (interactive "p")
  (dotimes (_ (or count 1))
    (eca-workspaces--next-line-matching
     #'eca-workspaces--session-id-at-point)))

(defun eca-workspaces-previous-entry (&optional count)
  "Move point to the previous workspace or chat entry.
With prefix COUNT, repeat that many times."
  (interactive "p")
  (dotimes (_ (or count 1))
    (eca-workspaces--previous-line-matching
     #'eca-workspaces--session-id-at-point)))

(defun eca-workspaces-next-workspace (&optional count)
  "Move point to the next workspace (session) entry.
With prefix COUNT, repeat that many times."
  (interactive "p")
  (dotimes (_ (or count 1))
    (eca-workspaces--next-line-matching
     #'eca-workspaces--session-line-p)))

(defun eca-workspaces-previous-workspace (&optional count)
  "Move point to the previous workspace (session) entry.
With prefix COUNT, repeat that many times."
  (interactive "p")
  (dotimes (_ (or count 1))
    (eca-workspaces--previous-line-matching
     #'eca-workspaces--session-line-p)))

(defun eca-workspaces--read-session ()
  "Return the session at point or ask the user to pick one."
  (or (eca-workspaces--session-at-point)
      (let ((sessions (eca-vals eca--sessions)))
        (cond
         ((null sessions) (user-error "No ECA session running"))
         ((null (cdr sessions)) (car sessions))
         (t (let* ((candidates
                    (-map (lambda (session)
                            (cons (eca--session-project-name session)
                                  session))
                          sessions))
                   (choice (completing-read "Workspace: "
                                            (-map #'car candidates)
                                            nil t)))
              (cdr (assoc choice candidates))))))))

(defun eca-workspaces-new-chat ()
  "Start a new chat in the workspace at point.
Asks for an optional initial prompt which is sent right away."
  (interactive)
  (let* ((session (eca-workspaces--read-session))
         (prompt (string-trim (read-string "Initial prompt (optional): "))))
    (eca-chat--new-chat session)
    (let ((chat-buffer (eca--session-last-chat-buffer session)))
      (when (and (buffer-live-p chat-buffer)
                 (not (string-empty-p prompt)))
        (with-current-buffer chat-buffer
          (eca-chat--send-prompt session prompt))))))

(defun eca-workspaces-delete ()
  "Delete the chat or stop the workspace at point, with confirmation."
  (interactive)
  (let ((chat-buffer (eca-workspaces--chat-buffer-at-point))
        (session (eca-workspaces--session-at-point)))
    (cond
     (chat-buffer
      (when (y-or-n-p (format "Delete chat '%s' from server? "
                              (with-current-buffer chat-buffer
                                (substring-no-properties (eca-chat-title)))))
        (with-current-buffer chat-buffer
          (eca-chat-delete))
        (eca-workspaces--schedule-refresh)))
     (session
      (when (y-or-n-p (format "Stop ECA session '%s'? "
                              (eca--session-project-name session)))
        (eca-stop-session session)
        (eca-workspaces--schedule-refresh)))
     (t (user-error "No chat or workspace at point")))))

(defun eca-workspaces-rename-chat ()
  "Rename the chat at point."
  (interactive)
  (with-current-buffer (eca-workspaces--chat-buffer-at-point-or-error)
    (eca-chat-rename)))

(defun eca-workspaces-fork-chat ()
  "Fork the chat at point into a new chat."
  (interactive)
  (with-current-buffer (eca-workspaces--chat-buffer-at-point-or-error)
    (eca-chat-fork)))

(defun eca-workspaces-select-model ()
  "Select the model of the chat at point."
  (interactive)
  (with-current-buffer (eca-workspaces--chat-buffer-at-point-or-error)
    (eca-chat-select-model)))

(defun eca-workspaces-select-variant ()
  "Select the model variant of the chat at point."
  (interactive)
  (with-current-buffer (eca-workspaces--chat-buffer-at-point-or-error)
    (eca-chat-select-variant)))

(defun eca-workspaces-compact-chat ()
  "Compact the chat at point sending the /compact command.
Asks for an optional extra prompt to guide the compaction."
  (interactive)
  (let ((chat-buffer (eca-workspaces--chat-buffer-at-point-or-error))
        (extra (string-trim (read-string "Compact prompt (optional): "))))
    (with-current-buffer chat-buffer
      (let ((session (eca-session)))
        (eca-assert-session-running session)
        (when eca-chat--chat-loading
          (user-error "Chat is busy, wait or stop the running prompt"))
        (eca-chat--send-prompt session
                               (string-trim (concat "/compact " extra)))))))

(defun eca-workspaces-stop-prompt ()
  "Stop the running prompt of the chat at point."
  (interactive)
  (with-current-buffer (eca-workspaces--chat-buffer-at-point-or-error)
    (eca-chat-stop-prompt)))

(defun eca-workspaces-tool-call-accept-next ()
  "Accept the next pending tool call of the chat at point."
  (interactive)
  (with-current-buffer (eca-workspaces--chat-buffer-at-point-or-error)
    (eca-chat-tool-call-accept-next)))

(defun eca-workspaces-tool-call-accept-all ()
  "Accept all pending tool calls of the chat at point."
  (interactive)
  (with-current-buffer (eca-workspaces--chat-buffer-at-point-or-error)
    (eca-chat-tool-call-accept-all)))

(defun eca-workspaces-tool-call-reject-next ()
  "Reject the next pending tool call of the chat at point."
  (interactive)
  (with-current-buffer (eca-workspaces--chat-buffer-at-point-or-error)
    (eca-chat-tool-call-reject-next)))

(defun eca-workspaces-resume ()
  "Resume a server-side chat into the workspace at point."
  (interactive)
  (let* ((session (eca-workspaces--read-session))
         (chat-buffer (car (eca-chat-buffers session))))
    (unless chat-buffer
      (user-error "This workspace has no chat buffer to resume from"))
    (with-current-buffer chat-buffer
      (eca-chat-resume))))

(with-eval-after-load 'transient
  (transient-define-prefix eca-workspaces--menu-prefix ()
    "ECA workspaces actions menu."
    [["Open"
      ("RET" "Open chat at point" eca-workspaces-visit-or-toggle)
      ("TAB" "Fold/unfold workspace" eca-workspaces-toggle-line)
      ("+" "New chat" eca-workspaces-new-chat)
      ("R" "Resume closed chat" eca-workspaces-resume)
      ("g" "Refresh" eca-workspaces-refresh)]
     ["Chat"
      ("r" "Rename" eca-workspaces-rename-chat)
      ("f" "Fork" eca-workspaces-fork-chat)
      ("C" "Compact" eca-workspaces-compact-chat)
      ("m" "Select model" eca-workspaces-select-model)
      ("v" "Select variant" eca-workspaces-select-variant)
      ("s" "Stop prompt" eca-workspaces-stop-prompt)
      ("d" "Delete chat/workspace" eca-workspaces-delete)]
     ["Tool calls"
      ("a" "Accept next" eca-workspaces-tool-call-accept-next)
      ("A" "Accept all" eca-workspaces-tool-call-accept-all)
      ("x" "Reject next" eca-workspaces-tool-call-reject-next)]]))

(defun eca-workspaces-menu ()
  "Open the ECA workspaces actions menu.
Requires the `transient' package."
  (interactive)
  (unless (featurep 'transient)
    (user-error "Install the `transient' package to use this menu"))
  (condition-case err
      (transient-setup 'eca-workspaces--menu-prefix)
    (error
     (user-error
      "ECA workspaces menu failed: %s. Try: M-x package-reinstall RET transient"
      (error-message-string err)))))

(defvar eca-workspaces-mode-map (make-sparse-keymap)
  "Keymap for `eca-workspaces-mode'.")

;; Bindings are applied at load time (not inside the defvar) so
;; reloading the package in a running session picks up new keys even
;; when the keymap variable survived from an older version.
;; `q', `g' (revert), scrolling, etc. are inherited from `special-mode'.
(let ((map eca-workspaces-mode-map))
  (define-key map (kbd "RET") #'eca-workspaces-visit-or-toggle)
  (define-key map (kbd "TAB") #'eca-workspaces-toggle-line)
  (define-key map (kbd "<tab>") #'eca-workspaces-toggle-line)
  (define-key map (kbd "n") #'eca-workspaces-next-entry)
  (define-key map (kbd "p") #'eca-workspaces-previous-entry)
  (define-key map (kbd "N") #'eca-workspaces-next-workspace)
  (define-key map (kbd "P") #'eca-workspaces-previous-workspace)
  (define-key map (kbd "+") #'eca-workspaces-new-chat)
  (define-key map (kbd "d") #'eca-workspaces-delete)
  (define-key map (kbd "DEL") #'eca-workspaces-delete)
  (define-key map (kbd "<deletechar>") #'eca-workspaces-delete)
  (define-key map (kbd "r") #'eca-workspaces-rename-chat)
  (define-key map (kbd "f") #'eca-workspaces-fork-chat)
  (define-key map (kbd "m") #'eca-workspaces-select-model)
  (define-key map (kbd "v") #'eca-workspaces-select-variant)
  (define-key map (kbd "C") #'eca-workspaces-compact-chat)
  (define-key map (kbd "a") #'eca-workspaces-tool-call-accept-next)
  (define-key map (kbd "A") #'eca-workspaces-tool-call-accept-all)
  (define-key map (kbd "x") #'eca-workspaces-tool-call-reject-next)
  (define-key map (kbd "s") #'eca-workspaces-stop-prompt)
  (define-key map (kbd "R") #'eca-workspaces-resume)
  (define-key map (kbd "?") #'eca-workspaces-menu))

;; Under evil (e.g. Doom), state maps beat major-mode maps, so keys
;; like `?', `d' or `r' would trigger evil commands instead.  Mark the
;; dashboard map as overriding: its bound keys win over evil state
;; bindings while unbound keys (`j'/`k', ...) stay with evil.
(with-eval-after-load 'evil
  (when (fboundp 'evil-make-overriding-map)
    (evil-make-overriding-map eca-workspaces-mode-map nil)))

(define-derived-mode eca-workspaces-mode special-mode "ECA-Workspaces"
  "Major mode for the ECA workspaces dashboard buffer.

\\{eca-workspaces-mode-map}"
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (eca-workspaces-refresh)))
  (setq-local header-line-format
              (concat " "
                      (propertize "ECA workspaces" 'face 'bold)
                      "  "
                      (propertize "live"
                                  'face 'eca-workspaces-tree-chat-details-face)))
  (add-hook 'kill-buffer-hook #'eca-workspaces--on-kill nil t))

;;;###autoload
(defun eca-workspaces ()
  "Display all running ECA sessions and their chats in a live dashboard.

The dashboard refreshes automatically as chats change state and
shows for each chat its status (⏳ running, 🚧 pending approval,
❓ waiting answer), elapsed time, cost and model.

In the buffer, press \\<eca-workspaces-mode-map>\\[eca-workspaces-menu] \
to list all available actions,
\\[eca-workspaces-toggle-line] to fold/unfold a workspace, and
\\[eca-workspaces-visit-or-toggle] to switch to the chat under point."
  (interactive)
  (let ((buffer (get-buffer-create eca-workspaces-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'eca-workspaces-mode)
        (eca-workspaces-mode))
      (eca-workspaces--render))
    (select-window
     (display-buffer
      buffer
      '((display-buffer-in-side-window)
        (side . bottom)
        (slot . 0)
        (dedicated . t)
        (window-parameters . ((no-delete-other-windows . t))))))
    (eca-workspaces--ensure-ticker)))

;; Live updates

(add-hook 'eca-chat-session-status-changed-functions
          #'eca-workspaces--schedule-refresh)
(add-hook 'eca-sessions-updated-hook
          #'eca-workspaces--schedule-refresh)

(provide 'eca-workspaces)
;;; eca-workspaces.el ends here
