;;; eca.el --- AI pair programming via ECA (Editor Code Assistant) -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;; Author: Eric Dallo <ercdll1337@gmail.com>
;; Maintainer: Eric Dallo <ercdll1337@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (dash "2.18.0") (s "1.12.0") (f "0.20.0") (markdown-mode "2.3") (compat "30.1"))
;; Keywords: tools
;; Homepage: https://github.com/editor-code-assistant/eca-emacs
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) client for Emacs to
;;  add AI code assistant tools.  Heavily insipired on
;;  lsp-mode for parsing and handling jsonrpc messages.
;;
;;; Code:

(when (version< emacs-version "28.1")
  (error "ECA requires Emacs 28.1 or later, but you are running %s"
         emacs-version))

(require 'cl-lib)
(require 'backtrace)
(require 'hierarchy)
(require 'tree-widget)
(require 'smerge-mode)

(require 'eca-util)
(require 'eca-process)
(require 'eca-api)
(require 'eca-settings)
(require 'eca-chat)
(require 'eca-mcp)
(require 'eca-providers)
(require 'eca-config)
(require 'eca-jobs)
(require 'eca-editor)
(require 'eca-completion)
(require 'eca-rewrite)
(require 'eca-doom)

(declare-function package-desc-version "package" (pkg-desc))
(declare-function package-version-join "package" (vlist))
(declare-function eca-chat--doctor-section "eca-chat")

(defun eca--client-version ()
  "Return the eca-emacs client version string.
Tries git info first, then package.el version, then file modification date."
  (let ((lib-file (or load-file-name (locate-library "eca"))))
    (when lib-file
      (let ((lib-dir (file-name-directory lib-file)))
        (or
         ;; Try git: SHA + date
         (when-let* ((git-dir (locate-dominating-file lib-dir ".git"))
                     (default-directory git-dir)
                     (sha (ignore-errors
                            (string-trim
                             (shell-command-to-string "git rev-parse --short HEAD 2>/dev/null"))))
                     (date (ignore-errors
                             (string-trim
                              (shell-command-to-string "git log -1 --format=%cd --date=short 2>/dev/null")))))
           (when (and (not (string-empty-p sha))
                      (not (string-empty-p date))
                      (not (string-match-p "fatal" sha)))
             (format "%s (%s)" sha date)))
         ;; Try package.el version
         (when (bound-and-true-p package-alist)
           (when-let* ((pkg-desc (cadr (assq 'eca package-alist))))
             (package-version-join (package-desc-version pkg-desc))))
         ;; Fallback: file modification date
         (when-let* ((attrs (file-attributes lib-file))
                     (mtime (file-attribute-modification-time attrs)))
           (format "unknown (modified %s)" (format-time-string "%Y-%m-%d" mtime))))))))

(defgroup eca nil
  "ECA group."
  :group 'eca)

;; Variables

(defcustom eca-before-initialize-hook nil
  "List of functions to be called before ECA has been initialized."
  :type 'hook
  :group 'eca)

(defcustom eca-after-initialize-hook nil
  "List of functions to be called after ECA has been initialized."
  :type 'hook
  :group 'eca)

(defcustom eca-send-process-id t
  "Whether to send the Emacs process ID to the ECA server.
When non-nil, the server uses it to detect when Emacs exits and
shut down automatically.  Set to nil to omit it, e.g. when running
the server independently of Emacs.

Set to nil when running the server inside a sandbox that hides or
remaps the host PID (firejail, bubblewrap, jai, containers); in
those setups the server's parent-process watchdog would otherwise
see an invalid PID and shut down right after startup.  Pair this
with `eca-process-wrapper-function' for a fully sandboxed setup."
  :type 'boolean
  :group 'eca)

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

;; Internal

(defvar eca-workspaces-buffer-name "*eca-workspaces*")

(defun eca--emacs-errors-buffer-name (session)
  "Return the Emacs errors buffer name for SESSION."
  (format "<eca:emacs-errors[%s]:%s>"
          (eca--session-project-name session)
          (eca--session-id session)))

(defun eca--log-error (session err &optional context backtrace)
  "Log error ERR to the Emacs errors buffer for SESSION.
Optional CONTEXT is a string describing what was happening
when the error occurred.  Optional BACKTRACE is a list of
frames captured via `backtrace-get-frames'."
  (let ((buffer (get-buffer-create (eca--emacs-errors-buffer-name session))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format "[%s] %s%s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      (if context (format "%s: " context) "")
                      (error-message-string err)))
      (when backtrace
        (dolist (frame backtrace)
          (let ((fun (backtrace-frame-fun frame)))
            (when fun
              (insert (format "  at %s\n" fun)))))
        (insert "\n")))))

(defun eca-show-emacs-errors (session)
  "Open the Emacs errors buffer for SESSION."
  (let ((buffer (get-buffer (eca--emacs-errors-buffer-name session))))
    (if buffer
        (with-current-buffer buffer
          (if (window-live-p (get-buffer-window (buffer-name)))
              (select-window (get-buffer-window (buffer-name)))
            (display-buffer (current-buffer))))
      (eca-info "No emacs errors logged for this session"))))

;;;###autoload
(defun eca-show-errors ()
  "Open the eca Emacs errors buffer if running."
  (interactive)
  (eca-show-emacs-errors (eca-session)))

(defun eca--emacs-errors-exit (session)
  "Clean up the Emacs errors buffer for SESSION on stop."
  (let ((buffer (get-buffer (eca--emacs-errors-buffer-name session))))
    (when buffer
      (with-current-buffer buffer
        (rename-buffer (concat (buffer-name) ":closed") t)
        (setq-local mode-line-format '("*Closed session*"))
        (when-let ((win (get-buffer-window (current-buffer))))
          (quit-window nil win))
        ;; Keep only the most recently closed errors buffer; kill older ones.
        (let ((current (current-buffer)))
          (dolist (b (buffer-list))
            (when (and (not (eq b current))
                       (or
                        (string-match-p "^<eca:emacs-errors:.*>:closed$" (buffer-name b))
                        (string-match-p "^<eca:emacs-errors:.*>$" (buffer-name b))))
              (kill-buffer b))))))))

(defun eca--get-message-type (json-data)
  "Get the message type from JSON-DATA."
  (if (plist-member json-data :id)
      (if (plist-member json-data :error)
          'response-error
        (if (plist-member json-data :method)
            'request
          'response))
    'notification))

(defun eca--handle-show-message (params)
  "Handle the show-message notification with PARAMS."
  (let ((type (plist-get params :type))
        (msg (plist-get params :message)))
    (pcase type
      ("error" (eca-error msg))
      ("warning" (eca-warn msg))
      ("info" (eca-info msg)))))

(defun eca-config-updated (session config)
  "Handle CONFIG updated notification for SESSION."
  (when-let ((chat (plist-get config :chat)))
    (eca-chat-config-updated session chat (plist-get config :chatId))))

(defun eca--tool-server-updated (session server)
  "Handle tool server updated message with SERVER for SESSION."
  (setf (eca--session-tool-servers session)
        (eca-assoc (eca--session-tool-servers session)
                   (plist-get server :name)
                   server))
  (eca-chat--handle-mcp-server-updated session server)
  (eca-mcp--handle-mcp-server-updated session server)
  (eca-settings-refresh-tab "mcps" session))

(defun eca--tool-server-removed (session params)
  "Handle tool server removed notification PARAMS for SESSION."
  (let ((name (plist-get params :name)))
    (setf (eca--session-tool-servers session)
          (eca-dissoc (eca--session-tool-servers session) name))
    (eca-chat--handle-mcp-server-updated session params)
    (eca-mcp--handle-mcp-server-updated session params)
    (eca-settings-refresh-tab "mcps" session)))

(defun eca--handle-progress (session params)
  "Handle $/progress notification with PARAMS for SESSION."
  (let ((task-id (plist-get params :taskId))
        (type (plist-get params :type))
        (title (plist-get params :title)))
    (setf (eca--session-init-tasks session)
          (eca-assoc (eca--session-init-tasks session)
                     task-id
                     (list :title title :type type)))
    (eca-chat--handle-init-progress session)))

(defun eca--handle-server-notification (session notification)
  "Handle NOTIFICATION sent by server for SESSION."
  (let ((method (plist-get notification :method))
        (params (plist-get notification :params)))
    (pcase method
      ("config/updated" (eca-config-updated session params))
      ("chat/contentReceived" (eca-chat-content-received session params))
      ("chat/cleared" (eca-chat-cleared session params))
      ("chat/deleted" (eca-chat-deleted session params))
      ("chat/opened" (eca-chat-opened session params))
      ("chat/statusChanged" (eca-chat-status-changed session params))
      ("rewrite/contentReceived" (eca-rewrite-content-received session params))
      ("tool/serverUpdated" (eca--tool-server-updated session params))
      ("tool/serverRemoved" (eca--tool-server-removed session params))
      ("providers/updated" (eca-providers--handle-provider-updated session params))
      ("jobs/updated" (eca-jobs--handle-jobs-updated session params))
      ("$/showMessage" (eca--handle-show-message params))
      ("$/progress" (eca--handle-progress session params))
      (_ 'ignore))))

(defun eca--handle-server-request (session request)
  "Handle REQUEST sent by server for SESSION."
  (let ((method (plist-get request :method))
        (params (plist-get request :params)))
    (pcase method
      ("editor/getDiagnostics" (eca-editor-get-diagnostics session params))
      ("chat/askQuestion" (eca-chat-handle-ask-question session request params))
      (_ (eca-warn "Unknown server request %s" method)))))

(defmacro eca--with-backtrace (var &rest body)
  "Execute BODY, capturing backtrace into VAR on error.
On Emacs 30+ uses `handler-bind' to capture a pre-unwind
backtrace.  On older Emacs, runs BODY without capture."
  (declare (indent 1))
  (if (fboundp 'handler-bind)
      `(handler-bind
           ((error (lambda (_err)
                     (setq ,var
                           (backtrace-get-frames
                            'handler-bind)))))
         ,@body)
    `(progn ,@body)))

(defun eca--handle-message (session json-data)
  "Handle raw message JSON-DATA for SESSION."
  (let ((id (plist-get json-data :id))
        (result (plist-get json-data :result))
        (backtrace nil)
        (eca--path-session session))
    (eca--with-backtrace backtrace
      (condition-case err
          (pcase (eca--get-message-type json-data)
            ('response (-let [(success-callback) (plist-get (eca--session-response-handlers session) id)]
                         (when success-callback
                           (cl-remf (eca--session-response-handlers session) id)
                           (funcall success-callback result))))
            ('response-error (-let [(_ error-callback) (plist-get (eca--session-response-handlers session) id)]
                               (when error-callback
                                 (cl-remf (eca--session-response-handlers session) id)
                                 (funcall error-callback (plist-get json-data :error)))))
            ('notification (eca--handle-server-notification session json-data))
            ('request (let ((response (eca--handle-server-request session json-data)))
                        (unless (eq response :async)
                          (eca-api-send-request-response session json-data response)))))
        (error
         (eca--log-error session err "handle-message" backtrace)
         (signal (car err) (cdr err)))))))

(defun eca--initialize (session)
  "Send the initialize request for SESSION."
  (run-hooks 'eca-before-initialize-hook)
  (setf (eca--session-status session) 'starting)
  (eca-api-request-async
   session
   :method "initialize"
   :params (append (when-let* ((pid (and eca-send-process-id
                                        (unless (-some-> (buffer-file-name)
                                                  (file-remote-p))
                                          (emacs-pid)))))
                     (list :processId pid))
                   (list :clientInfo (list :name "emacs"
                                          :version (emacs-version))
                         :capabilities (list :codeAssistant (list :chat t
                                                                  :chatCapabilities (list :askQuestion t)
                                                                  :editor (list :diagnostics t)))
                         :initializationOptions (list :chatAgent eca-chat-custom-agent)
                         :workspaceFolders (vconcat (-map (lambda (folder)
                                                           (list :uri (eca--path-to-uri folder)
                                                                 :name (file-name-nondirectory (directory-file-name folder))))
                                                         (eca--session-workspace-folders session)))))
   :success-callback (-lambda (res)
                       (setf (eca--session-status session) 'started)
                       (setf (eca--session-chat-welcome-message session) (plist-get res :chatWelcomeMessage))
                       (eca-api-notify session :method "initialized")
                       (eca-info "Started with workspaces: %s" (string-join (eca--session-workspace-folders session) ","))
                       (eca-chat-open session)
                       (run-hooks 'eca-after-initialize-hook))
   :error-callback (lambda (e) (eca-error e))))

(defun eca--discover-workspaces ()
  "Ask user for workspaces."
  (let ((root (funcall eca-find-root-for-buffer-function))
        (add-workspace? "yes")
        (workspaces '()))
    (while (string= "yes" add-workspace?)
      (let ((new-workspace (read-directory-name "Select the workspace root:" root)))
        (push new-workspace workspaces)
        (setq add-workspace? (completing-read "Add more workspaces?"
                                              (lambda (string pred action)
                                                (if (eq action 'metadata)
                                                    `(metadata (display-sort-function . identity))
                                                  (complete-with-action action '("no" "yes") string pred)))))))
    workspaces))

(defvar eca-workspaces-widget-icon-keymap
  (let ((map (make-sparse-keymap)))
    ;; Keep the tree-widget mouse behaviour (fold on click) but route
    ;; keyboard RET/TAB to our commands so they work on every line,
    ;; including leaf (chat) lines where the icon has no useful action.
    (set-keymap-parent map tree-widget-button-keymap)
    (define-key map (kbd "RET") #'eca-workspaces-visit-or-toggle)
    (define-key map (kbd "TAB") #'eca-workspaces-toggle-line)
    (define-key map (kbd "<tab>") #'eca-workspaces-toggle-line)
    map)
  "Keymap used on tree-widget icons in the ECA workspaces buffer.")

(defun eca--tree-widget-open-all (tree-widget)
  "Recursively add :open t and custom icons to TREE-WIDGET and all its children."
  (let ((args (plist-get (cdr tree-widget) :args)))
    (when args
      (plist-put (cdr tree-widget) :args
                 (mapcar #'eca--tree-widget-open-all args))))
  (append tree-widget `(:open t
                        :open-icon (tree-widget-icon :tag ,"▼ " :keymap ,eca-workspaces-widget-icon-keymap)
                        :close-icon (tree-widget-icon :tag ,"▶ " :keymap ,eca-workspaces-widget-icon-keymap)
                        :empty-icon (tree-widget-icon :tag ,(propertize "" 'face 'shadow) :keymap ,eca-workspaces-widget-icon-keymap)
                        :leaf-icon (tree-widget-icon :tag ,(propertize "" 'face 'shadow) :keymap ,eca-workspaces-widget-icon-keymap)
                        :guide (tree-widget-icon :tag ,(propertize "│" 'face 'shadow) :keymap ,eca-workspaces-widget-icon-keymap)
                        :end-guide (tree-widget-icon :tag ,(propertize "└" 'face 'shadow) :keymap ,eca-workspaces-widget-icon-keymap)
                        :no-guide (tree-widget-icon :tag " " :keymap ,eca-workspaces-widget-icon-keymap)
                        :handle (tree-widget-icon :tag ,(propertize "─" 'face 'shadow) :keymap ,eca-workspaces-widget-icon-keymap)
                        :no-handle (tree-widget-icon :tag "  " :keymap ,eca-workspaces-widget-icon-keymap)
                        :nohandle-guide (tree-widget-icon :tag ,(propertize "│" 'face 'shadow) :keymap ,eca-workspaces-widget-icon-keymap))))

;; Public

;;;###autoload
(defun eca-debug-nrepl-connect ()
  "Connect in eca nrepl port for development."
  (interactive)
  (eca-assert-session-running (eca-session))
  (with-current-buffer (eca-process--stderr-buffer-name (eca-session))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "started on port \\([0-9]+\\)" nil t)

        (when-let ((nrepl-port (string-to-number (match-string 1))))
          (save-match-data
            (when (functionp 'cider-connect-clj)
              (cider-connect-clj `(:host "localhost"
                                   :port ,nrepl-port)))))))))

;;;###autoload
(defun eca-version ()
  "Show ECA version information for debugging.
Displays eca-emacs client version, server version, and Emacs version."
  (interactive)
  (let* ((client-version (or (eca--client-version) "unknown"))
         (server-version
          (or (eca-process--server-version)
              "not found")))
    (message "eca-emacs: %s | server: %s | emacs: %s"
             client-version server-version (emacs-version))))

;;;###autoload
(defun eca-doctor ()
  "Print ECA diagnostic info to `*eca-doctor*' for bug reports.
The report contains the same data as `eca-version' followed by a
chat section automatically sourced from the active session's
chat buffer.  See `eca-chat--doctor-section'."
  (interactive)
  (let* ((out-buf (get-buffer-create "*eca-doctor*"))
         (client-ver (or (eca--client-version) "unknown"))
         (server-ver (or (eca-process--server-version) "not found"))
         (chat-section (eca-chat--doctor-section)))
    (with-current-buffer out-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# eca-doctor\n\n")
        (insert "Paste this buffer into your bug report.\n\n")
        (insert "## Versions\n\n")
        (insert (format "- eca-emacs:   %s\n" client-ver))
        (insert (format "- eca server:  %s\n" server-ver))
        (insert (format "- emacs:       %s\n" (emacs-version)))
        (insert (format "- system-type: %s\n\n" system-type))
        (insert "## Chat\n\n")
        (insert chat-section)
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer out-buf)))

;;;###autoload
(defun eca (&optional arg)
  "Start or switch to a eca session.
When ARG is current prefix, ask for workspace roots to use."
  (interactive "P")
  (let* ((workspaces (if (equal arg '(4))
                         (eca--discover-workspaces)
                       (list (funcall eca-find-root-for-buffer-function))))
         (session (or (eca-session)
                      (eca-create-session workspaces))))
    (pcase (eca--session-status session)
      ('stopped (eca-process-start session
                                   (lambda ()
                                     (eca--initialize session))
                                   (-partial #'eca--handle-message session)))
      ('started (eca-chat-open session))
      ('starting (eca-info "eca server is already starting")))))

;;;###autoload
(defun eca-stop ()
  "Stop eca if running."
  (interactive)
  (let ((session (eca-session)))
    (when (eca-process-running-p session)
      (eca-info "Shutting down...")
      (eca-api-request-sync session :method "shutdown")
      (eca-api-notify session :method "exit")
      (eca-process-stop session)
      (eca-chat-exit session)
      (eca-settings-exit session)
      (eca--emacs-errors-exit session)
      (eca-delete-session session))))

;;;###autoload
(defun eca-restart ()
  "Restart eca, if not running just start."
  (interactive)
  (eca-stop)
  (eca))

(defvar eca-workspaces-mode-map
  (let ((map (make-sparse-keymap)))
    ;; `q', `g' (revert), scrolling, etc. are inherited from
    ;; `special-mode'.  Only the tree-specific keys are defined here.
    (define-key map (kbd "RET") #'eca-workspaces-visit-or-toggle)
    (define-key map (kbd "TAB") #'eca-workspaces-toggle-line)
    (define-key map (kbd "<tab>") #'eca-workspaces-toggle-line)
    (define-key map (kbd "n") #'eca-workspaces-next-workspace)
    (define-key map (kbd "p") #'eca-workspaces-previous-workspace)
    map)
  "Keymap for `eca-workspaces-mode'.")

(define-derived-mode eca-workspaces-mode special-mode "ECA-Workspaces"
  "Major mode for the ECA workspaces tree buffer.

\\{eca-workspaces-mode-map}"
  (setq-local tree-widget-image-enable nil)
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (eca-workspaces-refresh))))

(defun eca-workspaces--chat-action-on-line ()
  "Return the chat-open action stored on the current line, if any."
  (save-excursion
    (beginning-of-line)
    (let ((eol (line-end-position))
          (action nil))
      (while (and (not action) (< (point) eol))
        (setq action (get-text-property (point) 'eca-button-on-action))
        (unless action
          (forward-char 1)))
      action)))

(defun eca-workspaces-toggle-line (&optional event)
  "Fold or unfold the workspace tree node on the current line.
Does nothing on a leaf (chat) line, which has nothing to fold.
EVENT, when non-nil, is the mouse event that triggered the command."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (save-excursion
    (beginning-of-line)
    (let ((eol (line-end-position))
          (toggled nil))
      (while (and (not toggled) (< (point) eol))
        (let ((widget (widget-at (point))))
          ;; The expand/collapse icon is the tree-widget icon that
          ;; carries a :node and is not a leaf.  Guides have no :node
          ;; and leaf icons cannot be folded, so skip both.
          (if (and widget
                   (tree-widget-p (widget-get widget :parent))
                   (widget-get widget :node)
                   (not (widget-get widget :tree-widget--leaf-flag)))
              (progn
                (widget-apply (widget-get widget :parent) :action)
                (setq toggled t))
            (forward-char 1))))
      toggled)))

(defun eca-workspaces-visit-or-toggle (&optional event)
  "Switch to the chat on the current line, or toggle the workspace node.
EVENT, when non-nil, is the mouse event that triggered the command."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (let ((action (eca-workspaces--chat-action-on-line)))
    (if action
        (funcall action)
      (eca-workspaces-toggle-line))))

(defun eca-workspaces--session-line-p ()
  "Return non-nil if the current line is a workspace (session) entry."
  (save-excursion
    (beginning-of-line)
    (let ((eol (line-end-position))
          (found nil))
      (while (and (not found) (< (point) eol))
        (if (get-text-property (point) 'eca-workspaces-session)
            (setq found t)
          (forward-char 1)))
      found)))

(defun eca-workspaces--goto-session-label ()
  "Move point to the workspace label on the current line."
  (beginning-of-line)
  (let ((eol (line-end-position)))
    (while (and (< (point) eol)
                (not (get-text-property (point) 'eca-workspaces-session)))
      (forward-char 1))))

(defun eca-workspaces-next-workspace (&optional count)
  "Move point to the next workspace (session) entry.
With prefix COUNT, repeat that many times."
  (interactive "p")
  (dotimes (_ (or count 1))
    (let ((origin (point))
          (found nil))
      (forward-line 1)
      (while (and (not (eobp)) (not found))
        (if (eca-workspaces--session-line-p)
            (setq found t)
          (forward-line 1)))
      (if found
          (eca-workspaces--goto-session-label)
        (goto-char origin)))))

(defun eca-workspaces-previous-workspace (&optional count)
  "Move point to the previous workspace (session) entry.
With prefix COUNT, repeat that many times."
  (interactive "p")
  (dotimes (_ (or count 1))
    (let ((origin (point))
          (found nil))
      (beginning-of-line)
      (while (and (not (bobp)) (not found))
        (forward-line -1)
        (when (eca-workspaces--session-line-p)
          (setq found t)))
      (if found
          (eca-workspaces--goto-session-label)
        (goto-char origin)))))

(defun eca-workspaces--foldable-label (text)
  "Return TEXT that toggles its tree node on RET/TAB.
When `eca-buttons-allow-mouse' is non-nil, clicking TEXT also
toggles it and the label shows a clickable affordance."
  (let* ((map (make-sparse-keymap))
         (props (list 'keymap map)))
    (define-key map (kbd "RET") #'eca-workspaces-toggle-line)
    (define-key map (kbd "TAB") #'eca-workspaces-toggle-line)
    (when eca-buttons-allow-mouse
      (define-key map (kbd "<mouse-1>") #'eca-workspaces-toggle-line)
      (setq props (append props (list 'pointer 'hand
                                      'help-echo "mouse-1: fold/unfold"))))
    (add-text-properties 0 (length text) props text)
    text))

(defun eca-workspaces--refresh-buffer (buffer)
  "Rebuild the ECA workspaces tree inside BUFFER."
  (let ((h (hierarchy-new))
        (parent-fn (lambda (item)
                     (cond
                      ((eca--session-p item) 'root)
                      ((bufferp item)
                       (with-current-buffer item
                         (eca-get eca--sessions eca--session-id-cache)))
                      (t nil))))
        (label-fn (lambda (item _)
                    (insert
                     (cond
                      ((eca--session-p item)
                       (eca-workspaces--foldable-label
                        (propertize (string-join (eca--session-workspace-folders item) ", ")
                                    'face 'shadow
                                    'eca-workspaces-session t)))
                      ((bufferp item)
                       (with-current-buffer item
                         (concat
                          (eca-buttonize
                             nil
                             (propertize (eca-chat-title)
                                         'face (if (buffer-local-value 'eca-chat--chat-loading item)
                                                   'eca-workspaces-tree-chat-loading-face
                                                 'eca-workspaces-tree-chat-idle-face))
                             (lambda ()
                               (with-current-buffer item
                                 (setf (eca--session-last-chat-buffer (eca-session)) item)
                                 (eca-chat-open (eca-session)))))
                          (when eca-chat--session-cost
                            (propertize (format "  %s" (eca-chat--usage-str)) 'face 'eca-workspaces-tree-chat-details-face)))))
                      (t (propertize "ECA" 'face 'shadow)))))))
    (seq-doseq (session (eca-vals eca--sessions))
      (hierarchy-add-tree h session parent-fn)
      (seq-doseq (chat-by-id (eca--session-chats session))
        (when (buffer-live-p (cdr chat-by-id))
          (hierarchy-add-tree h (cdr chat-by-id) parent-fn))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'eca-workspaces-mode)
        (eca-workspaces-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (widget-create (eca--tree-widget-open-all
                        (hierarchy-convert-to-tree-widget h label-fn)))
        (widget-setup))
      (goto-char (point-min)))))

(defun eca-workspaces-refresh ()
  "Refresh the ECA workspaces buffer if it exists."
  (interactive)
  (when-let* ((b (get-buffer eca-workspaces-buffer-name)))
    (when (buffer-live-p b)
      (eca-workspaces--refresh-buffer b))))

;;;###autoload
(defun eca-workspaces ()
  "Display all running ECA sessions and their chats in a tree view.

In the buffer, press \\<eca-workspaces-mode-map>\\[quit-window] to \
close the window,
\\[eca-workspaces-toggle-line] to fold/unfold a workspace, and
\\[eca-workspaces-visit-or-toggle] to switch to the chat under point."
  (interactive)
  (let ((b (get-buffer-create eca-workspaces-buffer-name)))
    (eca-workspaces--refresh-buffer b)
    (select-window
     (display-buffer
      b
      '((display-buffer-in-side-window)
        (side . bottom)
        (slot . 0)
        (dedicated . t)
        (window-parameters . ((no-delete-other-windows . t))))))))

(defun eca--chat-buffer-candidates-for-session (session seen)
  "Return completion candidates for SESSION.

SEEN is a hash table tracking previously used candidate names.
Returns a list of (NAME . PLIST) entries."
  (cl-loop
   for chat-by-id in (eca--session-chats session)
   for buf = (cdr chat-by-id)
   when (buffer-live-p buf)
   collect
   (with-current-buffer buf
     (let* ((title (substring-no-properties (eca-chat-title)))
            (project (eca--session-project-name session))
            (base (format "%s: %s" project title))
            (n (gethash base seen 0))
            (name (if (zerop n)
                      base
                    (format "%s (%s)" base (buffer-name buf)))))
       (puthash base (1+ n) seen)
       (cons name
             (list :buffer buf
                   :session session))))))

(defun eca--chat-buffer-candidates ()
  "Return an alist of chat completion candidates.

Each element is (NAME . PLIST).  PLIST includes:
  :buffer  the chat buffer
  :session the owning session.

Candidate names are made unique when multiple chats share the same title."
  (let ((seen (make-hash-table :test #'equal)))
    (cl-loop
     for session in (eca-vals eca--sessions)
     append (eca--chat-buffer-candidates-for-session session seen))))

(defun eca--chat-buffer-completion-table (candidates)
  "Build a completion table for CANDIDATES."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata
          (category . eca-chat))
      (complete-with-action action (mapcar #'car candidates) string pred))))

(defun eca--switch-to-chat-candidate (prompt candidates empty-message)
  "Switch to a chat selected from CANDIDATES using PROMPT.

Signal EMPTY-MESSAGE as a `user-error' when CANDIDATES is nil."
  (unless candidates
    (user-error empty-message))
  (let* ((info (if (= 1 (length candidates))
                   (cdar candidates)
                 (let* ((table (eca--chat-buffer-completion-table candidates))
                        (choice (completing-read prompt table nil t)))
                   (cdr (assoc choice candidates)))))
         (buffer (plist-get info :buffer))
         (session (plist-get info :session)))
    (eca-chat--switch-to-buffer buffer session)))

;;;###autoload
(defun eca-switch-to-chat ()
  "Prompt for an active ECA chat buffer and switch to it."
  (interactive)
  (eca--switch-to-chat-candidate
   "Switch to ECA chat: "
   (eca--chat-buffer-candidates)
   "No active ECA chats"))

;;;###autoload
(defun eca-switch-to-project-chat ()
  "Switch to an ECA chat buffer for the current project."
  (interactive)
  (let ((session (eca-session))
        (seen (make-hash-table :test #'equal)))
    (eca--switch-to-chat-candidate
     "Switch to project ECA chat: "
     (and session
          (eca--chat-buffer-candidates-for-session session seen))
     "No active ECA chats for this project")))

;;;###autoload
(defun eca-open-global-config ()
  "Open the global config tab in eca-settings."
  (interactive)
  (eca-settings "global-config"))

(provide 'eca)
;;; eca.el ends here
