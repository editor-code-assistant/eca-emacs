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

(require 'f)

(require 'eca-util)
(require 'eca-settings)

(declare-function eca "eca" (&optional arg))

;; Paths

(defun eca-config--global-path ()
  "Return the path to the global ECA config file."
  (if-let (xdg (getenv "XDG_CONFIG_HOME"))
      (f-join xdg "eca" "config.json")
    (f-join (f-expand "~") ".config" "eca" "config.json")))

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
      (eca-settings--setup-tab-line tab-key))
    buf))

;; Create functions

(defun eca-config--create-global-buffer (session)
  "Create the Global Config settings tab buffer for SESSION."
  (eca-config--create-buffer "global-config" session
                             (eca-config--global-path)))

;; Refresh

(defun eca-config--refresh (_session buffer)
  "Refresh config BUFFER by reverting from disk."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((tab-key eca-settings--tab-key))
        (revert-buffer t t t)
        (eca-config--json-mode)
        (local-set-key (kbd "C-c C-,")
                       (lambda () (interactive) (eca)))
        (local-set-key (kbd "C-c .") #'eca-transient-menu)
        (eca-settings--setup-tab-line tab-key)))))

;; Registration

(eca-settings-register-tab
 "global-config" "⚙ Global Config"
 #'eca-config--create-global-buffer
 #'eca-config--refresh)

(provide 'eca-config)
;;; eca-config.el ends here
