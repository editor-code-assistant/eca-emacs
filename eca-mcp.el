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

(require 'eca-api)
(require 'eca-util)
(require 'eca-process)
(require 'eca-settings)

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
  '((t (:inherit success :underline t)))
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
  '((t (:inherit button)))
  "Face for disable button in mcp details buffer."
  :group 'eca)

(defface eca-mcp-details-button-enable-face
  '((t (:inherit button)))
  "Face for enable button in mcp details buffer."
  :group 'eca)

(defface eca-mcp-details-command-value-face
  '((t (:inherit font-lock-doc-face :height 0.9)))
  "Face for command value in mcp details."
  :group 'eca)

;; Internal

(declare-function eca "eca.el" args)
(declare-function eca-open-global-config "eca" ())
(declare-function eca-providers "eca-providers" ())

(defconst eca-mcp-menu-items
  '(("mcps" . mcps)
    ("providers" . providers)
    ("global config" . global-config))
  "Entries shown in the MCP header popup menu.")

(defun eca-mcp--open-menu-choice (choice)
  "Open the settings view selected by CHOICE."
  (pcase choice
    ('mcps (eca-mcp-details))
    ('providers (eca-providers))
    ('global-config (eca-open-global-config))))

(defun eca-mcp-open-menu (&optional event)
  "Open a popup menu for MCP-related settings.
With EVENT, show a mouse popup.  Without it, prompt in minibuffer."
  (interactive (list (when (mouse-event-p last-input-event)
                       last-input-event)))
  (eca-assert-session-running (eca-session))
  (let ((choice
         (if (and event (display-popup-menus-p))
             (x-popup-menu
              event
              (list "Config"
                    (cons "MCPs" eca-mcp-menu-items)))
           (let* ((labels (mapcar #'car eca-mcp-menu-items))
                  (label (completing-read "Open MCP settings: "
                                          labels nil t)))
             (cdr (assoc label eca-mcp-menu-items))))))
    (when choice
      (eca-mcp--open-menu-choice choice))))

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

;; Buffer-local UI state for the MCPs settings tab.

(defvar-local eca-mcp--add-form-state nil
  "Buffer-local plist describing the in-progress add-server form.
nil when the form is closed.")

(defvar-local eca-mcp--confirming-remove nil
  "Buffer-local name of the MCP server currently in remove-confirmation, or nil.")

(defun eca-mcp--add-form-default ()
  "Return the initial plist for a fresh add-server form."
  (list :name ""
        :transport "stdio"
        :command ""
        :args ""
        :env nil
        :url ""
        :headers nil
        :scope "global"))

(defun eca-mcp--parse-args-string (s)
  "Split args string S by whitespace into a list of args."
  (when (and s (not (string-empty-p (string-trim s))))
    (split-string s "[ \t\n]+" t)))

(defun eca-mcp--parse-env-lines (lines)
  "Parse LINES (each \"KEY=VALUE\") into a plist for JSON encoding."
  (let (out)
    (dolist (l lines)
      (when-let* ((pos (string-match "=" l)))
        (let ((k (substring l 0 pos))
              (v (substring l (1+ pos))))
          (push v out)
          (push (intern (concat ":" k)) out))))
    (nreverse out)))

(defun eca-mcp--parse-header-lines (lines)
  "Parse LINES (each \"Header: value\") into a plist for JSON encoding."
  (let (out)
    (dolist (l lines)
      (when-let* ((pos (string-match ":" l)))
        (let ((k (string-trim (substring l 0 pos)))
              (v (string-trim (substring l (1+ pos)))))
          (push v out)
          (push (intern (concat ":" k)) out))))
    (nreverse out)))

(defun eca-mcp--add-form-set (field value session)
  "Set FIELD to VALUE in the add-server form state and refresh SESSION's tab."
  (setq eca-mcp--add-form-state
        (plist-put eca-mcp--add-form-state field value))
  (eca-settings-refresh-tab "mcps" session))

(defun eca-mcp--add-form-edit-field (field prompt session)
  "Read a single-line value for FIELD from minibuffer using PROMPT."
  (let ((cur (or (plist-get eca-mcp--add-form-state field) "")))
    (eca-mcp--add-form-set field (read-string prompt cur) session)))

(defun eca-mcp--add-form-edit-list-field (field prompt session)
  "Read multiple lines for list FIELD (env or headers) until empty."
  (let (acc next)
    (while (not (string-empty-p (setq next (read-string prompt ""))))
      (push next acc))
    (eca-mcp--add-form-set field (nreverse acc) session)))

(defun eca-mcp--open-add-form (session)
  "Open the inline add-server form."
  (setq eca-mcp--add-form-state (eca-mcp--add-form-default))
  (eca-settings-refresh-tab "mcps" session))

(defun eca-mcp--cancel-add-form (session)
  "Close the add-server form without submitting."
  (setq eca-mcp--add-form-state nil)
  (eca-settings-refresh-tab "mcps" session))

(defun eca-mcp--submit-add-server (session)
  "Validate current form state and send `mcp/addServer' to the server."
  (let* ((st eca-mcp--add-form-state)
         (name (string-trim (or (plist-get st :name) "")))
         (transport (plist-get st :transport))
         (scope (plist-get st :scope)))
    (cond
     ((string-empty-p name)
      (eca-error "MCP server name is required"))
     ((and (string= transport "stdio")
           (string-empty-p (string-trim (or (plist-get st :command) ""))))
      (eca-error "Command is required for stdio transport"))
     ((and (string= transport "remote")
           (string-empty-p (string-trim (or (plist-get st :url) ""))))
      (eca-error "URL is required for remote transport"))
     ((and (string= scope "workspace")
           (null (eca--session-workspace-folders session)))
      (eca-error "No workspace folder available for workspace scope"))
     (t
      (let ((params (list :name name :scope scope))
            (buf (current-buffer)))
        (if (string= transport "stdio")
            (let ((cmd (string-trim (plist-get st :command)))
                  (args (eca-mcp--parse-args-string (plist-get st :args)))
                  (env (eca-mcp--parse-env-lines (plist-get st :env))))
              (setq params (plist-put params :command cmd))
              (when args (setq params (plist-put params :args (vconcat args))))
              (when env (setq params (plist-put params :env env))))
          (let ((url (string-trim (plist-get st :url)))
                (headers (eca-mcp--parse-header-lines (plist-get st :headers))))
            (setq params (plist-put params :url url))
            (when headers (setq params (plist-put params :headers headers)))))
        (when (string= scope "workspace")
          (when-let ((folder (car (eca--session-workspace-folders session))))
            (setq params (plist-put params :workspaceUri (eca--path-to-uri folder)))))
        (eca-api-request-async
         session
         :method "mcp/addServer"
         :params params
         :success-callback
         (lambda (resp)
           (let ((err (plist-get resp :error)))
             (if err
                 (eca-error "Add MCP failed: %s"
                            (or (plist-get err :message) (format "%S" err)))
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq eca-mcp--add-form-state nil)))
               (eca-settings-refresh-tab "mcps" session))))
         :error-callback
         (lambda (err)
           (eca-error "Add MCP failed: %s"
                      (or (plist-get err :message) (format "%S" err))))))))))

(defun eca-mcp--request-confirm-remove (session name)
  "Start two-step remove-confirmation for server NAME."
  (setq eca-mcp--confirming-remove name)
  (eca-settings-refresh-tab "mcps" session))

(defun eca-mcp--cancel-confirm-remove (session)
  "Cancel remove confirmation in SESSION's MCPs tab."
  (setq eca-mcp--confirming-remove nil)
  (eca-settings-refresh-tab "mcps" session))

(defun eca-mcp--submit-remove-server (session name)
  "Send `mcp/removeServer' for NAME."
  (let ((buf (current-buffer)))
    (eca-api-request-async
     session
     :method "mcp/removeServer"
     :params (list :name name)
     :success-callback
     (lambda (resp)
       (let ((err (plist-get resp :error)))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (setq eca-mcp--confirming-remove nil)))
         (when err
           (eca-error "Remove MCP %s failed: %s" name
                      (or (plist-get err :message) (format "%S" err)))
           (eca-settings-refresh-tab "mcps" session))))
     :error-callback
     (lambda (err)
       (eca-error "Remove MCP %s failed: %s" name
                  (or (plist-get err :message) (format "%S" err)))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (setq eca-mcp--confirming-remove nil)))
       (eca-settings-refresh-tab "mcps" session)))))

(defun eca-mcp--render-add-form (session keymap)
  "Render the inline add-server form into the current buffer."
  (let* ((st eca-mcp--add-form-state)
         (transport (plist-get st :transport))
         (scope (plist-get st :scope)))
    (insert (propertize "Add MCP server" 'font-lock-face 'bold) "\n")
    (insert "Name: ")
    (insert (eca-buttonize
             keymap
             (propertize (if (string-empty-p (plist-get st :name))
                             "<click to set>"
                           (plist-get st :name))
                         'font-lock-face 'eca-mcp-details-command-value-face)
             (lambda () (eca-mcp--add-form-edit-field :name "Server name: " session))))
    (insert "\n")
    (insert "Transport: ")
    (insert (eca-buttonize
             keymap
             (propertize (concat (if (string= transport "stdio") "[x]" "[ ]") " stdio")
                         'font-lock-face (if (string= transport "stdio")
                                             'eca-mcp-details-button-face
                                           'eca-mcp-details-button-disable-face))
             (lambda () (eca-mcp--add-form-set :transport "stdio" session))))
    (insert "  ")
    (insert (eca-buttonize
             keymap
             (propertize (concat (if (string= transport "remote") "[x]" "[ ]") " remote")
                         'font-lock-face (if (string= transport "remote")
                                             'eca-mcp-details-button-face
                                           'eca-mcp-details-button-disable-face))
             (lambda () (eca-mcp--add-form-set :transport "remote" session))))
    (insert "\n")
    (cond
     ((string= transport "stdio")
      (insert "Command: ")
      (insert (eca-buttonize
               keymap
               (propertize (if (string-empty-p (plist-get st :command))
                               "<click to set>"
                             (plist-get st :command))
                           'font-lock-face 'eca-mcp-details-command-value-face)
               (lambda () (eca-mcp--add-form-edit-field :command "Command: " session))))
      (insert "\n")
      (insert "Args: ")
      (insert (eca-buttonize
               keymap
               (propertize (if (string-empty-p (plist-get st :args))
                               "<click to set>"
                             (plist-get st :args))
                           'font-lock-face 'eca-mcp-details-command-value-face)
               (lambda () (eca-mcp--add-form-edit-field :args "Args (space-separated): " session))))
      (insert "\n")
      (insert "Env: ")
      (let ((env (plist-get st :env)))
        (if env
            (insert (propertize (mapconcat #'identity env ", ")
                                'font-lock-face 'eca-mcp-details-command-value-face))
          (insert (propertize "(none)" 'font-lock-face 'shadow))))
      (insert " ")
      (insert (eca-buttonize
               keymap
               (propertize "[edit]" 'font-lock-face 'eca-mcp-details-button-face)
               (lambda () (eca-mcp--add-form-edit-list-field
                           :env "Env KEY=VALUE (empty to finish): " session))))
      (insert "\n"))
     (t
      (insert "URL: ")
      (insert (eca-buttonize
               keymap
               (propertize (if (string-empty-p (plist-get st :url))
                               "<click to set>"
                             (plist-get st :url))
                           'font-lock-face 'eca-mcp-details-command-value-face)
               (lambda () (eca-mcp--add-form-edit-field :url "URL: " session))))
      (insert "\n")
      (insert "Headers: ")
      (let ((hs (plist-get st :headers)))
        (if hs
            (insert (propertize (mapconcat #'identity hs ", ")
                                'font-lock-face 'eca-mcp-details-command-value-face))
          (insert (propertize "(none)" 'font-lock-face 'shadow))))
      (insert " ")
      (insert (eca-buttonize
               keymap
               (propertize "[edit]" 'font-lock-face 'eca-mcp-details-button-face)
               (lambda () (eca-mcp--add-form-edit-list-field
                           :headers "Header KEY: VALUE (empty to finish): " session))))
      (insert "\n")))
    (insert "Scope: ")
    (insert (eca-buttonize
             keymap
             (propertize (concat (if (string= scope "global") "[x]" "[ ]") " global")
                         'font-lock-face (if (string= scope "global")
                                             'eca-mcp-details-button-face
                                           'eca-mcp-details-button-disable-face))
             (lambda () (eca-mcp--add-form-set :scope "global" session))))
    (insert "  ")
    (insert (eca-buttonize
             keymap
             (propertize (concat (if (string= scope "workspace") "[x]" "[ ]") " workspace")
                         'font-lock-face (if (string= scope "workspace")
                                             'eca-mcp-details-button-face
                                           'eca-mcp-details-button-disable-face))
             (lambda () (eca-mcp--add-form-set :scope "workspace" session))))
    (insert "\n")
    (insert (eca-buttonize
             keymap
             (propertize "[Submit]" 'font-lock-face 'eca-mcp-details-button-face)
             (lambda () (eca-mcp--submit-add-server session))))
    (insert "  ")
    (insert (eca-buttonize
             keymap
             (propertize "[Cancel]" 'font-lock-face 'eca-mcp-details-button-stop-face)
             (lambda () (eca-mcp--cancel-add-form session))))
    (insert "\n\n")))

(defun eca-mcp--render-server-details (session buffer)
  "Render MCP server details for SESSION into BUFFER.
Works with both standalone and settings panel buffers."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (keymap (or (current-local-map)
                        eca-settings-mode-map))
            (saved-pos (point)))
        (erase-buffer)
        (insert "\n")
        (insert (propertize "All MCP servers configured in ECA" 'font-lock-face 'eca-settings-heading))
        (insert "\n\n")
        (if eca-mcp--add-form-state
            (eca-mcp--render-add-form session keymap)
          (insert (eca-buttonize
                   keymap
                   (propertize "[+ Add MCP server]"
                               'font-lock-face 'eca-mcp-details-button-face)
                   (lambda () (eca-mcp--open-add-form session)))
                  "\n\n"))
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
            (cond
             ((and eca-mcp--confirming-remove
                   (string= name eca-mcp--confirming-remove))
              (insert (propertize "Remove? " 'font-lock-face 'warning))
              (insert (eca-buttonize
                       keymap
                       (propertize "[Yes]"
                                   'font-lock-face 'eca-mcp-details-button-stop-face)
                       (lambda () (eca-mcp--submit-remove-server session name))))
              (insert " ")
              (insert (eca-buttonize
                       keymap
                       (propertize "[Cancel]"
                                   'font-lock-face 'eca-mcp-details-button-face)
                       (lambda () (eca-mcp--cancel-confirm-remove session)))))
             (t
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
               (if (plist-get server :disabled)
                   (insert " "
                           (eca-buttonize
                            keymap
                            (propertize "enable"
                                        'font-lock-face 'eca-mcp-details-button-enable-face)
                            (lambda () (eca-api-notify session
                                                        :method "mcp/enableServer"
                                                        :params (list :name name)))))
                 (insert " "
                         (eca-buttonize
                          keymap
                          (propertize "disable"
                                      'font-lock-face 'eca-mcp-details-button-disable-face)
                          (lambda () (eca-api-notify session
                                                      :method "mcp/disableServer"
                                                      :params (list :name name))))))
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
                        (propertize "start"
                                    'font-lock-face 'eca-mcp-details-button-face)
                        (lambda () (eca-api-notify session
                                                    :method "mcp/startServer"
                                                    :params (list :name name)))))
               (insert " "
                       (eca-buttonize
                        keymap
                        (propertize "enable"
                                    'font-lock-face 'eca-mcp-details-button-enable-face)
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
                                                    :params (list :name name)))))
               (when (plist-get server :disabled)
                 (insert " "
                         (eca-buttonize
                          keymap
                          (propertize "enable"
                                      'font-lock-face 'eca-mcp-details-button-enable-face)
                          (lambda () (eca-api-notify session
                                                      :method "mcp/enableServer"
                                                      :params (list :name name))))))))
              (unless (string= name "ECA")
                (insert " "
                        (eca-buttonize
                         keymap
                         (propertize "remove"
                                     'font-lock-face 'eca-mcp-details-button-stop-face)
                         (lambda () (eca-mcp--request-confirm-remove session name)))))))
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
          (insert "\n\n"))
        (goto-char (min saved-pos (point-max)))))))

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

(defun eca-mcp-servers (session)
  "Return all servers that are not from eca server SESSION, the MCP servers."
  (eca-vals (eca-dissoc (eca--session-tool-servers session) "ECA")))

(defun eca-mcp--handle-mcp-server-updated (session _server)
  "Handle mcp SERVER updated for SESSION."
  (eca-settings-refresh-tab "mcps" session))

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
      (eca-settings--setup-tab-line "mcps" session)
      (eca-mcp--render-server-details session buf))
    buf))

(eca-settings-register-tab
 "mcps" "🔌 MCPs"
 #'eca-mcp--create-settings-buffer
 #'eca-mcp--render-server-details)

(provide 'eca-mcp)
;;; eca-mcp.el ends here
