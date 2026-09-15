;;; eca-doom.el --- ECA (Editor Code Assistant) Doom Emacs integration -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Doom Emacs integration for ECA.  Marks chat buffers as Doom "real"
;;  buffers so they join the workspace buffer list and are never
;;  swapped for the fallback buffer.  With the `:ui workspaces' module
;;  it also decorates the workspaces tabline, coloring each workspace
;;  tab according to the status of its related ECA session: orange
;;  when waiting for an approval/question, dim yellow while a chat is
;;  running (disable with `eca-doom-workspace-tabs'), and stops the
;;  ECA session of a killed workspace (disable with
;;  `eca-doom-stop-session-on-workspace-kill').  Enabled automatically
;;  on Doom.
;;
;;; Code:

(require 'dash)
(require 'f)

(require 'eca-util)
(require 'eca-chat)

(declare-function +workspace-list-names "ext:workspaces" ())
(declare-function +workspace-get "ext:workspaces" (name &optional noerror))
(declare-function +workspace/display "ext:workspaces" ())
(declare-function persp-buffers "ext:persp-mode" (persp))
(declare-function persp-name "ext:persp-mode" (persp))
(declare-function eca-stop-session "eca" (session))

(defcustom eca-doom-workspace-tabs t
  "Whether to decorate the Doom workspace tabline with ECA status.
When non-nil, workspaces related to an ECA session that is running
or waiting for user approval are colored in the tabline shown by
`+workspace/display' and after workspace switches.  Only used in
Doom Emacs with the `:ui workspaces' module enabled."
  :type 'boolean
  :group 'eca)

(defcustom eca-doom-stop-session-on-workspace-kill t
  "Whether killing a Doom workspace also stops its ECA session.
When non-nil, killing a workspace (e.g. `+workspace/kill') stops the
ECA session related to it, unless another workspace still refers to
that session.  Its chats stay resumable server-side.  Only used in
Doom Emacs with the `:ui workspaces' module enabled."
  :type 'boolean
  :group 'eca)

(defface eca-doom-workspace-tab-attention-face
  '((((background dark))  :foreground "#ff9e64")
    (((background light)) :foreground "#cc5500"))
  "Face for Doom workspace tabs waiting on the user.
Used when any chat of the workspace ECA session has a pending
tool call approval or question."
  :group 'eca)

(defface eca-doom-workspace-tab-running-face
  '((t :inherit eca-chat-tab-inactive-active-face))
  "Face for Doom workspace tabs with a running ECA chat."
  :group 'eca)

(defun eca-doom-real-buffer-p (buffer)
  "Return non-nil when BUFFER is a live ECA chat buffer.
Meant for `doom-real-buffer-functions' so Doom treats chats as
real buffers: they join the workspace buffer list and commands like
`+workspace/kill' do not swap them for the fallback buffer.  Chats
closed by `eca-chat-exit' are not real."
  (with-current-buffer buffer
    (and (derived-mode-p 'eca-chat-mode)
         (not eca-chat--closed))))

(defun eca-doom--buffer-in-folders-p (buffer folders)
  "Return non-nil when BUFFER's directory is under one of FOLDERS."
  (when-let* ((dir (buffer-local-value 'default-directory buffer)))
    (-first (lambda (folder)
              (or (f-same? folder dir)
                  (f-ancestor-of? folder dir)))
            folders)))

(defun eca-doom--session-for-persp (persp)
  "Return the ECA session related to the perspective PERSP, or nil.
Resolves through the buffers of PERSP: first via their cached
session id, then by matching their directory against the workspace
folders of each session."
  (let ((buffers (-filter #'buffer-live-p (persp-buffers persp))))
    (or (-some (lambda (buffer)
                 (eca-get eca--sessions
                          (buffer-local-value 'eca--session-id-cache buffer)))
               buffers)
        (-first (lambda (session)
                  (-first (lambda (buffer)
                            (eca-doom--buffer-in-folders-p
                             buffer
                             (eca--session-workspace-folders session)))
                          buffers))
                (eca-vals eca--sessions)))))

(defun eca-doom--session-for-workspace (name)
  "Return the ECA session related to the Doom workspace NAME, or nil.
See `eca-doom--session-for-persp'."
  (when-let* ((persp (+workspace-get name t)))
    (eca-doom--session-for-persp persp)))

(defun eca-doom--session-used-elsewhere-p (session name)
  "Return non-nil when a workspace other than NAME relates to SESSION."
  (-some (lambda (other)
           (and (not (equal other name))
                (eq session (eca-doom--session-for-workspace other))))
         (+workspace-list-names)))

(defun eca-doom--on-workspace-kill (persp)
  "Stop the ECA session of the workspace PERSP about to be killed.
Bound to `persp-before-kill-functions'.  Does nothing when
`eca-doom-stop-session-on-workspace-kill' is nil, when PERSP has no
related session or when another workspace still refers to it.
Errors are reported without aborting the workspace kill."
  (when (and eca-doom-stop-session-on-workspace-kill persp)
    (when-let* ((session (eca-doom--session-for-persp persp))
                (name (persp-name persp)))
      (unless (eca-doom--session-used-elsewhere-p session name)
        (condition-case err
            (eca-stop-session session)
          (error
           (eca-warn "Could not stop the ECA session of workspace %s: %s"
                     name (error-message-string err))))))))

(defun eca-doom--status-face (status)
  "Return the face to apply for STATUS, or nil when idle."
  (pcase status
    ('waiting-approval 'eca-doom-workspace-tab-attention-face)
    ('running 'eca-doom-workspace-tab-running-face)
    (_ nil)))

(defun eca-doom--decorate-segment (tabline index name face)
  "Apply FACE to the workspace NAME segment at INDEX in TABLINE.
Return the decorated tabline, or TABLINE unchanged when the
segment is not found."
  (let ((regexp (format " \\[%d\\] \\(%s\\) " index (regexp-quote name))))
    (if (string-match regexp tabline)
        (let ((result (copy-sequence tabline)))
          (add-face-text-property (match-beginning 1) (match-end 1)
                                  face nil result)
          result)
      tabline)))

(defun eca-doom--tabline-decorate (tabline names)
  "Return TABLINE with NAMES segments decorated by ECA session status.
Return nil when `eca-doom-workspace-tabs' is nil or TABLINE is not
a string, so callers can fall back to the original value."
  (when (and eca-doom-workspace-tabs (stringp tabline))
    (let ((result tabline))
      (-each-indexed names
        (lambda (index name)
          (when-let* ((session (eca-doom--session-for-workspace name))
                      (face (eca-doom--status-face
                             (eca-chat-session-status session))))
            (setq result (eca-doom--decorate-segment result (1+ index)
                                                     name face)))))
      result)))

(defvar eca-doom--last-tabline nil
  "The last tabline string produced by `eca-doom--tabline-advice'.
Used to detect whether the workspace tabline is the message currently
shown, so `eca-doom--do-refresh' only redraws an already-visible
tabline.")

(defvar eca-doom--refresh-timer nil
  "Idle timer coalescing workspace tabline refreshes, or nil.")

(defun eca-doom--tabline-advice (orig-fn &optional names)
  "Around advice for `+workspace--tabline' adding ECA status decoration.
Call ORIG-FN with NAMES and decorate its result.  Any error falls
back to the undecorated tabline so the Doom UI never breaks."
  (let* ((tabline (funcall orig-fn names))
         (result (or (ignore-errors
                       (eca-doom--tabline-decorate tabline
                                                   (or names (+workspace-list-names))))
                     tabline)))
    (when (stringp result)
      (setq eca-doom--last-tabline result))
    result))

(defun eca-doom--tabline-visible-p ()
  "Return non-nil when the workspace tabline is the message shown now.
Compares the echo-area message text with the last produced tabline.
`string-equal' ignores text properties, so a status-only face change
still matches the same workspace labels."
  (when-let* ((shown (current-message)))
    (and eca-doom--last-tabline
         (string-equal shown eca-doom--last-tabline))))

(defun eca-doom--do-refresh ()
  "Re-display the workspace tabline with fresh ECA status.
Only acts when the tabline is the message currently shown, so an
unrelated or dismissed echo-area message is never clobbered nor
resurrected."
  (setq eca-doom--refresh-timer nil)
  (when (and eca-doom-workspace-tabs
             (fboundp '+workspace/display)
             (eca-doom--tabline-visible-p))
    (ignore-errors (+workspace/display))))

(defun eca-doom--schedule-refresh ()
  "Schedule a single idle workspace tabline refresh.
Throttles bursts of status changes into one redraw: subsequent calls
while a refresh is pending are ignored."
  (unless (timerp eca-doom--refresh-timer)
    (setq eca-doom--refresh-timer
          (run-with-idle-timer 0.1 nil #'eca-doom--do-refresh))))

(defun eca-doom--on-session-status-changed (&optional _session)
  "Refresh the workspace tabline after an ECA session status change.
Bound to `eca-chat-session-status-changed-functions'."
  (when (and eca-doom-workspace-tabs
             (fboundp '+workspace/display))
    (eca-doom--schedule-refresh)))

(defun eca-doom-setup ()
  "Enable the ECA Doom integration.
Marks chat buffers as Doom real buffers and, when the `:ui
workspaces' module is enabled, decorates the workspaces tabline and
stops the ECA session of killed workspaces."
  (add-hook 'doom-real-buffer-functions #'eca-doom-real-buffer-p)
  (when (fboundp '+workspace--tabline)
    (advice-add '+workspace--tabline :around #'eca-doom--tabline-advice)
    (add-hook 'eca-chat-session-status-changed-functions
              #'eca-doom--on-session-status-changed)
    (add-hook 'persp-before-kill-functions #'eca-doom--on-workspace-kill)))

(when (featurep 'doom)
  (eca-doom-setup))

(provide 'eca-doom)
;;; eca-doom.el ends here
