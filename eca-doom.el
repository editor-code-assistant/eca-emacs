;;; eca-doom.el --- ECA (Editor Code Assistant) Doom Emacs integration -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Doom Emacs integration for ECA.  Decorates the Doom workspaces
;;  tabline (`:ui workspaces' module) coloring each workspace tab
;;  according to the status of its related ECA session: orange when
;;  waiting for an approval/question, dim yellow while a chat is
;;  running.  Enabled automatically on Doom, disable with
;;  `eca-doom-workspace-tabs'.
;;
;;; Code:

(require 'dash)
(require 'f)

(require 'eca-util)
(require 'eca-chat)

(declare-function +workspace-list-names "ext:workspaces" ())
(declare-function +workspace-get "ext:workspaces" (name &optional noerror))
(declare-function persp-buffers "ext:persp-mode" (persp))

(defcustom eca-doom-workspace-tabs t
  "Whether to decorate the Doom workspace tabline with ECA status.
When non-nil, workspaces related to an ECA session that is running
or waiting for user approval are colored in the tabline shown by
`+workspace/display' and after workspace switches.  Only used in
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

(defun eca-doom--buffer-in-folders-p (buffer folders)
  "Return non-nil when BUFFER's directory is under one of FOLDERS."
  (when-let* ((dir (buffer-local-value 'default-directory buffer)))
    (-first (lambda (folder)
              (or (f-same? folder dir)
                  (f-ancestor-of? folder dir)))
            folders)))

(defun eca-doom--session-for-workspace (name)
  "Return the ECA session related to the Doom workspace NAME, or nil.
Resolves through the buffers of the workspace perspective: first
via their cached session id, then by matching their directory
against the workspace folders of each session."
  (when-let* ((persp (+workspace-get name t)))
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
                  (eca-vals eca--sessions))))))

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

(defun eca-doom--tabline-advice (orig-fn &optional names)
  "Around advice for `+workspace--tabline' adding ECA status decoration.
Call ORIG-FN with NAMES and decorate its result.  Any error falls
back to the undecorated tabline so the Doom UI never breaks."
  (let ((tabline (funcall orig-fn names)))
    (or (ignore-errors
          (eca-doom--tabline-decorate tabline
                                      (or names (+workspace-list-names))))
        tabline)))

(defun eca-doom-setup ()
  "Enable the ECA Doom workspaces integration."
  (advice-add '+workspace--tabline :around #'eca-doom--tabline-advice))

(when (and (featurep 'doom)
           (fboundp '+workspace--tabline))
  (eca-doom-setup))

(provide 'eca-doom)
;;; eca-doom.el ends here
