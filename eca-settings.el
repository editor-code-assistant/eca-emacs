;;; eca-settings.el --- ECA centralized settings panel -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Centralized settings panel for ECA with tab-line
;;  navigation across management features (MCP servers,
;;  jobs, login, etc).
;;
;;; Code:

(require 'compat)

(require 'eca-util)

(declare-function eca "eca" (&optional arg))

;; Faces

(defface eca-settings-heading
  '((t :inherit bold :height 1.2))
  "Face for headings in ECA settings buffers."
  :group 'eca)

;; Customization

(defcustom eca-settings-tab-line t
  "Whether to show a tab line in settings buffers.
When non-nil, enables `tab-line-mode' in settings
buffers with tabs for each registered settings panel."
  :type 'boolean
  :group 'eca)

(defcustom eca-settings-display-params
  '((display-buffer-in-side-window)
    (side . right)
    (window-width . 0.35))
  "Display parameters for the settings panel."
  :type 'alist
  :group 'eca)

;; Tab registry

(defvar eca-settings--tabs '()
  "Registered settings tabs.
Ordered list of plists with :key :label :create-fn :refresh-fn.")

(defun eca-settings-register-tab (key label create-fn refresh-fn)
  "Register a settings tab identified by KEY.
LABEL is the display name shown in the tab-line.
CREATE-FN is called with (session) to create the buffer.
REFRESH-FN is called with (session buffer) to re-render.
Re-registering an existing KEY updates it in place,
preserving tab order."
  (let ((new-tab (list :key key
                       :label label
                       :create-fn create-fn
                       :refresh-fn refresh-fn))
        (pos (cl-position key eca-settings--tabs
                          :test #'string=
                          :key (lambda (tab)
                                 (plist-get tab :key)))))
    (if pos
        (setf (nth pos eca-settings--tabs) new-tab)
      (setq eca-settings--tabs
            (append eca-settings--tabs
                    (list new-tab))))))

(defun eca-settings--find-tab (key)
  "Find the registered tab plist for KEY, or nil."
  (cl-find-if (lambda (tab)
                (string= (plist-get tab :key) key))
              eca-settings--tabs))

;; Buffer management

(defvar-local eca-settings--tab-key nil
  "The tab key for this settings buffer.")

(defun eca-settings--buffer-name (tab-key session)
  "Return buffer name for TAB-KEY in SESSION."
  (format "<eca-settings:%s:%s>"
          tab-key (eca--session-id session)))

(defun eca-settings--get-buffer (tab-key session)
  "Get existing settings buffer for TAB-KEY in SESSION."
  (get-buffer (eca-settings--buffer-name tab-key session)))

(defun eca-settings--create-buffer (tab-key session)
  "Create a new settings buffer for TAB-KEY in SESSION."
  (get-buffer-create
   (generate-new-buffer-name
    (eca-settings--buffer-name tab-key session))))

(defun eca-settings--get-or-create-tab-buffer (tab-key session)
  "Get or create settings buffer for TAB-KEY in SESSION.
When creating, calls the tab's registered :create-fn."
  (or (when-let* ((buf (eca-settings--get-buffer tab-key session)))
        (when (buffer-live-p buf) buf))
      (when-let* ((tab (eca-settings--find-tab tab-key)))
        (funcall (plist-get tab :create-fn) session))))

(defun eca-settings--ensure-all-buffers (session)
  "Ensure all registered tab buffers exist for SESSION."
  (dolist (tab eca-settings--tabs)
    (let ((key (plist-get tab :key)))
      (eca-settings--get-or-create-tab-buffer key session))))

;; Tab-line

(defun eca-settings--tab-line-tabs ()
  "Return tab descriptors for all registered settings tabs."
  (when-let* ((session (ignore-errors (eca-session))))
    (let ((current-key eca-settings--tab-key))
      (-keep
       (lambda (tab-def)
         (let* ((key (plist-get tab-def :key))
                (label (plist-get tab-def :label))
                (buf (eca-settings--get-buffer key session)))
           (when (and buf (buffer-live-p buf))
             `(tab
               (name . ,(format " %s " label))
               (tab-key . ,key)
               (buffer . ,buf)
               (selected . ,(string= key current-key))))))
       eca-settings--tabs))))

(defun eca-settings--tab-line-face (tab _tabs face _selected-p _buffer)
  "Dim non-selected settings tabs.
Applies `eca-tab-inactive-face' to non-selected TAB,
preserving the base FACE."
  (if (cdr (assq 'selected tab))
      face
    `(:inherit (eca-tab-inactive-face ,face))))

(defun eca-settings--setup-tab-line (tab-key)
  "Setup tab-line in current buffer for TAB-KEY."
  (setq-local eca-settings--tab-key tab-key)
  (when eca-settings-tab-line
    (require 'tab-line)
    (setq-local tab-line-tabs-function
                #'eca-settings--tab-line-tabs)
    (setq-local tab-line-tab-face-functions
                '(eca-settings--tab-line-face))
    (setq-local tab-line-new-button-show nil)
    (setq-local tab-line-close-button-show nil)
    (setq-local tab-line-separator "")
    (face-remap-add-relative 'tab-line :height 0.9)
    (tab-line-mode 1)))

(defun eca-settings--force-tab-line-update ()
  "Force tab-line redraw in all settings windows."
  (walk-windows
   (lambda (win)
     (when (buffer-local-value 'eca-settings--tab-key
                               (window-buffer win))
       (set-window-parameter win 'tab-line-cache nil)))
   nil t)
  (force-mode-line-update t))

;; Refresh

(defun eca-settings-refresh-tab (tab-key session)
  "Refresh the settings buffer for TAB-KEY in SESSION."
  (when-let* ((tab (eca-settings--find-tab tab-key))
              (buf (eca-settings--get-buffer tab-key session))
              (refresh-fn (plist-get tab :refresh-fn)))
    (when (buffer-live-p buf)
      (funcall refresh-fn session buf)
      (eca-settings--force-tab-line-update))))

(defun eca-settings--refresh-current ()
  "Refresh the currently displayed settings tab."
  (interactive)
  (when-let* ((session (ignore-errors (eca-session)))
              (key eca-settings--tab-key))
    (eca-settings-refresh-tab key session)))

;; Mode

(defvar eca-settings-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")
                #'eca-settings--refresh-current)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "C-c C-,")
                (lambda () (interactive) (eca)))
    (define-key map (kbd "C-c .") #'eca-transient-menu)
    map)
  "Keymap for `eca-settings-mode'.")

(define-derived-mode eca-settings-mode special-mode
  "eca-settings"
  "Major mode for ECA settings panels.
\\{eca-settings-mode-map}"
  :group 'eca)

;; Entry point

(defun eca-settings--find-window ()
  "Find a window displaying a settings buffer, or nil."
  (let ((found nil))
    (walk-windows
     (lambda (win)
       (when (buffer-local-value 'eca-settings--tab-key
                                 (window-buffer win))
         (setq found win)))
     nil t)
    found))

;;;###autoload
(defun eca-settings (&optional tab-key)
  "Open the ECA settings panel.
Optional TAB-KEY focuses a specific tab."
  (interactive)
  (let ((session (eca-session)))
    (eca-assert-session-running session)
    (unless eca-settings--tabs
      (user-error "No settings tabs registered"))
    (eca-settings--ensure-all-buffers session)
    (let* ((key (or tab-key
                    (plist-get (car eca-settings--tabs) :key)))
           (buf (eca-settings--get-or-create-tab-buffer
                 key session))
           (existing-win (eca-settings--find-window)))
      (cond
       ;; Target tab already visible
       ((window-live-p (get-buffer-window buf))
        (select-window (get-buffer-window buf)))
       ;; Another settings tab visible, switch it
       (existing-win
        (select-window existing-win)
        (switch-to-buffer buf))
       ;; No settings window, open new
       (t
        (display-buffer buf eca-settings-display-params)
        (select-window (get-buffer-window buf))))
      (eca-settings--force-tab-line-update))))

(defun eca-settings-exit (session)
  "Clean up all settings buffers for SESSION."
  (dolist (tab eca-settings--tabs)
    (let* ((key (plist-get tab :key))
           (buf (eca-settings--get-buffer key session)))
      (when (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (rename-buffer (concat (buffer-name) ":closed") t)
          (setq-local mode-line-format
                      '("*Closed session*"))
          (when-let* ((win (get-buffer-window buf)))
            (quit-window nil win)))))))

(provide 'eca-settings)
;;; eca-settings.el ends here
