;;; eca-mcp.el --- ECA (Editor Code Assistant) mcp -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) mcp.
;;
;;; Code:

(require 'compat)

(require 'eca-util)
(require 'eca-process)
(require 'eca-settings)

(defcustom eca-mcp-details-position-params `((display-buffer-in-side-window)
                                             (side . right)
                                             (window-width . 0.35))
  "Position params for mcp details display."
  :type 'alist
  :group 'eca)

(defface eca-mcp-details-tool-face
  '((t (:inherit hl-line :slant italic)))
  "Face for tools showed in mcp details buffer."
  :group 'eca)

(defface eca-mcp-details-tool-disabled-face
  '((t (:inherit hl-line :slant italic :strike-through t)))
  "Face for tools showed in mcp details buffer."
  :group 'eca)

(defface eca-mcp-details-requires-auth-face
  '((t (:inherit warning :weight bold)))
  "Face for requires-auth status in mcp details."
  :group 'eca)

(defface eca-mcp-details-button-face
  '((t (:inherit button)))
  "Face for buttons in mcp details buffer."
  :group 'eca)

(defface eca-mcp-details-button-stop-face
  '((t (:inherit error :underline t)))
  "Face for stop button in mcp details buffer."
  :group 'eca)

(defface eca-mcp-details-button-logout-face
  '((t (:inherit warning :underline t)))
  "Face for logout button in mcp details buffer."
  :group 'eca)

(defface eca-mcp-details-button-disable-face
  '((t (:foreground "orange" :underline t)))
  "Face for disable button in mcp details buffer."
  :group 'eca)

(defface eca-mcp-details-command-value-face
  '((t (:inherit font-lock-doc-face :height 0.9)))
  "Face for command value in mcp details."
  :group 'eca)

;; Internal

(declare-function eca "eca.el" args)

(defvar eca-mcp-details-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-,") (lambda () (interactive) (eca-settings)))
    (define-key map (kbd "C-c .") #'eca-transient-menu)
    map)
  "Keymap used by `eca-mcp-details-mode'.")

(defun eca-mcp-details-buffer-name (session)
  "Return the chat buffer name for SESSION."
  (format  "<eca-mcp-details:%s>" (eca--session-id session)))

(defun eca-mcp--get-details-buffer (session)
  "Get the eca mcp buffer for SESSION."
  (get-buffer (eca-mcp-details-buffer-name session)))

(defun eca-mcp--create-details-buffer (session)
  "Create the eca mcp details buffer for SESSION."
  (get-buffer-create (generate-new-buffer-name (eca-mcp-details-buffer-name session))))

(defun eca-mcp--status-emoji (status)
  "Return a colored emoji circle for STATUS."
  (pcase status
    ("running" "🟢")
    ("starting" "🟡")
    ("failed" "🔴")
    ("stopped" "⚪")
    ("stopping" "⚪")
    ("disabled" "⚫")
    ("requires-auth" "🟠")
    (_ "⚪")))

(defun eca-mcp--render-server-details (session buffer)
  "Render MCP server details for SESSION into BUFFER.
Works with both standalone and settings panel buffers."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (keymap (or (current-local-map)
                        eca-mcp-details-mode-map)))
        (erase-buffer)
        (insert "\n")
        (insert (propertize "All MCP servers configured in ECA" 'font-lock-face 'helpful-heading))
        (insert "\n\n")
        (seq-doseq (server (-sort  (lambda (a b)
                                     (string-lessp (plist-get a :name)
                                                   (plist-get b :name)))
                                   (eca-vals (eca--session-tool-servers session))))
          (-let (((&plist :name name :command command :args args
                          :status status :tools tools) server))
            (insert (propertize (eca-mcp--status-emoji status)
                                'eca-mcp-status status
                                'help-echo status))
            (insert " ")
            (insert (propertize name 'font-lock-face 'bold))
            (insert "   ")
            (pcase status
              ("requires-auth"
               (insert (eca-buttonize
                        keymap
                        (propertize "connect"
                                    'font-lock-face 'eca-mcp-details-button-face)
                        (lambda () (eca-api-notify session
                                                    :method "mcp/connectServer"
                                                    :params (list :name name))))))
              ((or "running" "starting")
               (insert (eca-buttonize
                        keymap
                        (propertize "stop"
                                    'font-lock-face 'eca-mcp-details-button-stop-face)
                        (lambda () (eca-api-notify session
                                                    :method "mcp/stopServer"
                                                    :params (list :name name)))))
               (insert " "
                       (eca-buttonize
                        keymap
                        (propertize "disable"
                                    'font-lock-face 'eca-mcp-details-button-disable-face)
                        (lambda () (eca-api-notify session
                                                    :method "mcp/disableServer"
                                                    :params (list :name name)))))
               (when (plist-get server :hasAuth)
                 (insert " "
                         (eca-buttonize
                          keymap
                          (propertize "logout"
                                      'font-lock-face 'eca-mcp-details-button-logout-face)
                          (lambda () (eca-api-notify session
                                                      :method "mcp/logoutServer"
                                                      :params (list :name name)))))))
              ("disabled"
               (insert (eca-buttonize
                        keymap
                        (propertize "enable"
                                    'font-lock-face 'eca-mcp-details-button-face)
                        (lambda () (eca-api-notify session
                                                    :method "mcp/enableServer"
                                                    :params (list :name name))))))
              (_
               (insert (eca-buttonize
                        keymap
                        (propertize "start"
                                    'font-lock-face 'eca-mcp-details-button-face)
                        (lambda () (eca-api-notify session
                                                    :method "mcp/startServer"
                                                    :params (list :name name)))))))
            (insert "\n")
            (if (seq-empty-p tools)
                (insert (propertize "No tools available" 'font-lock-face font-lock-doc-face))
              (progn
                (insert (propertize "Tools: " 'font-lock-face font-lock-doc-face))
                (seq-doseq (tool tools)
                  (insert (propertize (plist-get tool :name)
                                      'eca-mcp-tool tool
                                      'font-lock-face (if (plist-get tool :disabled)
                                                          'eca-mcp-details-tool-disabled-face
                                                        'eca-mcp-details-tool-face)) " "))))
            (when-let* ((prompts (plist-get server :prompts))
                        (_ (not (seq-empty-p prompts))))
              (insert "\n")
              (insert (propertize "Prompts: " 'font-lock-face font-lock-doc-face))
              (seq-doseq (prompt prompts)
                (insert (propertize (plist-get prompt :name)
                                    'font-lock-face 'eca-mcp-details-tool-face) " ")))
            (when-let* ((resources (plist-get server :resources))
                        (_ (not (seq-empty-p resources))))
              (insert "\n")
              (insert (propertize "Resources: " 'font-lock-face font-lock-doc-face))
              (seq-doseq (resource resources)
                (insert (propertize (plist-get resource :name)
                                    'font-lock-face 'eca-mcp-details-tool-face) " ")))
            (when command
              (insert "\n")
              (insert (propertize "Command: " 'font-lock-face font-lock-doc-face))
              (insert (propertize (concat command " " (string-join args " "))
                                 'font-lock-face 'eca-mcp-details-command-value-face)))
            (when-let* ((url (plist-get server :url)))
              (insert "\n")
              (insert (propertize "URL: " 'font-lock-face font-lock-doc-face))
              (insert (propertize url
                                 'font-lock-face 'eca-mcp-details-command-value-face)))
            (when (string= "failed" status)
              (insert "\n")
              (insert (propertize (format "Failed to start, check %s for details"
                                          (buttonize
                                           "eca stderr buffer"
                                           (lambda(_) (eca-process-show-stderr session))))
                                  'font-lock-face 'error))))
          (insert "\n\n"))))))

(defun eca-mcp--refresh-server-details (session)
  "Refresh the standalone MCP details buffer for SESSION."
  (when-let* ((buf (eca-mcp--get-details-buffer session)))
    (eca-mcp--render-server-details session buf)))

(defun eca-mcp--format-input-schema-args (input-schema)
  "Format INPUT-SCHEMA properties into a list of readable arg description strings."
  (when-let* ((properties (plist-get input-schema :properties)))
    (let ((required (append (plist-get input-schema :required) nil))
          (args '()))
      (cl-loop for (key val) on properties by #'cddr
               do (let* ((name (substring (symbol-name key) 1))
                         (type (plist-get val :type))
                         (description (plist-get val :description))
                         (required? (member name required)))
                    (push (concat (propertize name 'face (if required? 'bold 'italic))
                                  (when type
                                    (concat " (" (propertize type 'face 'font-lock-type-face) ")"))
                                  (unless required?
                                    (propertize " [optional]" 'face 'shadow))
                                  (when description
                                    (concat ": " description)))
                          args)))
      (nreverse args))))

(defun eca-mcp--eldoc-function (cb &rest _ignored)
  "Eldoc function for MCP details buffer.
When point is on a tool or status emoji, call CB with docs."
  (cond
   ((when-let* ((status (get-text-property (point) 'eca-mcp-status)))
      (funcall cb (concat (propertize "Status: " 'face 'bold)
                          status))
      t))
   ((when-let* ((tool (get-text-property (point) 'eca-mcp-tool)))
      (let* ((name (plist-get tool :name))
             (description (plist-get tool :description))
             (input-schema (plist-get tool :inputSchema))
             (args (eca-mcp--format-input-schema-args input-schema))
             (doc (concat (propertize name 'face 'bold)
                          (when description
                            (concat ": " description))
                          (when args
                            (concat "\n"
                                    (propertize "Args:" 'face 'font-lock-keyword-face)
                                    "\n  "
                                    (string-join args "\n  "))))))
        (funcall cb doc)
        t)))))

;; Public

(define-derived-mode eca-mcp-details-mode fundamental-mode "eca-mcp-details"
  "Major mode for ECA mcp details.
\\{eca-mcp-details-mode-map}"
  :group 'eca
  (visual-line-mode)
  (add-hook 'eldoc-documentation-functions #'eca-mcp--eldoc-function nil t)
  (eldoc-mode 1)
  (eca-mcp--render-server-details (eca-session) (current-buffer)))

(defun eca-mcp-servers (session)
  "Return all servers that are not from eca server SESSION, the MCP servers."
  (eca-vals (eca-dissoc (eca--session-tool-servers session) "ECA")))

(defun eca-mcp--handle-mcp-server-updated (session _server)
  "Handle mcp SERVER updated for SESSION."
  (eca-mcp--refresh-server-details session))

(defun eca-mcp-details-exit (session)
  "Exit the ECA mcp details for SESSION."
  (when (buffer-live-p (get-buffer (eca-mcp-details-buffer-name session)))
    (with-current-buffer (eca-mcp--get-details-buffer session)
      (goto-char (point-max))
      (setq-local mode-line-format '("*Closed session*"))
      (rename-buffer (concat (buffer-name) ":closed") t)
      (when-let* ((window (get-buffer-window (eca-mcp--get-details-buffer session))))
        (quit-window nil window)))))

;;;###autoload
(defun eca-mcp-details ()
  "List MCP servers with their status and options.
Opens the settings panel focused on the MCPs tab."
  (interactive)
  (eca-settings "mcps"))

;; Settings tab

(defun eca-mcp--create-settings-buffer (session)
  "Create the MCP settings tab buffer for SESSION."
  (let ((buf (eca-settings--create-buffer "mcps" session)))
    (with-current-buffer buf
      (eca-settings-mode)
      (visual-line-mode)
      (add-hook 'eldoc-documentation-functions
                #'eca-mcp--eldoc-function nil t)
      (eldoc-mode 1)
      (eca-settings--setup-tab-line "mcps")
      (eca-mcp--render-server-details session buf))
    buf))

(eca-settings-register-tab
 "mcps" "🔌 MCPs"
 #'eca-mcp--create-settings-buffer
 #'eca-mcp--render-server-details)

(provide 'eca-mcp)
;;; eca-mcp.el ends here
