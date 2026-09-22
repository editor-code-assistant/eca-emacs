;;; eca-config.el --- ECA config file editing tab -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Settings tab for viewing and editing the ECA global
;;  configuration file as a JSON buffer.
;;
;;; Code:

(require 'dash)
(require 'f)

(require 'eca-util)
(require 'eca-settings)

(declare-function eca "eca" (&optional arg))

;; Paths

(defun eca-config--home-directory ()
  "Return the home directory the ECA server resolves.
The server uses the JVM `user.home', which on Windows is the user
profile directory even when HOME is unset, while Emacs defaults
HOME to %APPDATA% in that case."
  (or (and (eq system-type 'windows-nt)
           (getenv "USERPROFILE"))
      (f-expand "~")))

(defun eca-config--local-global-path ()
  "Resolve the global ECA config path the same way the server does."
  (if-let* ((xdg (getenv "XDG_CONFIG_HOME")))
      (f-join xdg "eca" "config.json")
    (f-join (eca-config--home-directory) ".config" "eca" "config.json")))

(defun eca-config--server-path-to-local (path session)
  "Translate PATH reported by the server of SESSION to a local path.
Applies the session path mappings; when the workspace folders are
remote and no mapping applied, reuse their TRAMP prefix so the file
is opened on the host running the server."
  (let* ((eca--path-session session)
         (local (eca--path-remote-to-local path)))
    (if-let* ((remote (and (string= local path)
                           (-some #'file-remote-p
                                  (eca--session-workspace-folders session)))))
        (concat remote path)
      local)))

(defun eca-config--global-path (&optional session)
  "Return the path to the global ECA config file.
Prefer the path the server of SESSION reported in the `initialize'
response, so the buffer edits the file the server actually reads.
Fall back to resolving it locally for servers without support."
  (if-let* ((path (and session (eca--session-global-config-path session))))
      (eca-config--server-path-to-local path session)
    (eca-config--local-global-path)))

;; Helpers

(defun eca-config--ensure-directory ()
  "Create parent directory for the current buffer file before saving."
  (when buffer-file-name
    (make-directory (file-name-directory buffer-file-name) t)))

(defun eca-config--json-mode ()
  "Enable the best available JSON major mode.
Temporarily clears buffer during mode
activation so file-based hooks (e.g. LSP) do not
trigger — config buffers are not project files."
  (let ((file-name buffer-file-name)
        (true-name buffer-file-truename))
    (setq buffer-file-name nil
          buffer-file-truename nil)
    (unwind-protect
        (cond
         ((and (fboundp 'json-ts-mode)
               (fboundp 'treesit-ready-p)
               (treesit-ready-p 'json t))
          (json-ts-mode))
         ((fboundp 'json-mode) (json-mode))
         (t (js-mode)))
      (setq buffer-file-name file-name
            buffer-file-truename true-name))))

(defun eca-config--create-buffer (tab-key session path)
  "Create a file-visiting settings buffer for TAB-KEY.
SESSION is the current ECA session.  PATH is the config file."
  (let ((buf (eca-settings--create-buffer tab-key session))
        (abs-path (expand-file-name path)))
    (with-current-buffer buf
      (if (file-exists-p abs-path)
          (insert-file-contents abs-path nil nil nil t)
        (insert "{}\n"))
      (setq buffer-file-name abs-path)
      (setq buffer-file-truename (file-truename abs-path))
      (add-hook 'before-save-hook #'eca-config--ensure-directory nil t)
      (eca-config--json-mode)
      (local-set-key (kbd "C-c C-,") (lambda () (interactive) (eca)))
      (local-set-key (kbd "C-c .") #'eca-transient-menu)
      (set-buffer-modified-p nil)
      (setq-local mode-line-buffer-identification
                  (list (propertize (abbreviate-file-name abs-path)
                                    'face 'mode-line-buffer-id)))
      (eca-settings--setup-tab-line tab-key session))
    buf))

;; Create functions

(defun eca-config--create-global-buffer (session)
  "Create the Global Config settings tab buffer for SESSION."
  (eca-config--create-buffer "global-config" session
                             (eca-config--global-path session)))

;; Refresh

(defun eca-config--refresh (session buffer)
  "Refresh config BUFFER for SESSION by reverting from disk."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((tab-key eca-settings--tab-key))
        (revert-buffer t t t)
        (eca-config--json-mode)
        (local-set-key (kbd "C-c C-,")
                       (lambda () (interactive) (eca)))
        (local-set-key (kbd "C-c .") #'eca-transient-menu)
        (eca-settings--setup-tab-line tab-key session)))))

;; Registration

(eca-settings-register-tab
 "global-config" "⚙ Global Config"
 #'eca-config--create-global-buffer
 #'eca-config--refresh)

(provide 'eca-config)
;;; eca-config.el ends here
