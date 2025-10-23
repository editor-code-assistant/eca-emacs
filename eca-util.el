;;; eca-util.el --- ECA (Editor Code Assistant) util -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) utils.
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'transient)

(defcustom eca-buttons-allow-mouse nil
  "Whether to allow mouse clicks on ECA buttons."
  :type 'boolean
  :group 'eca)

(defcustom eca-find-root-for-buffer-function #'eca-find-root-for-buffer
  "Function for getting the ECA's session root."
  :type 'function
  :group 'eca)

(defun eca-assoc (map key val)
  "Return a new MAP with KEY associated to flat plist VAL, replacing any existing."
  (cons (cons key val)
        (cl-remove-if (lambda (pair) (equal (car pair) key)) map)))

(defun eca-dissoc (map key)
  "Return a new MAP with KEY removed."
  (cl-remove-if (lambda (pair) (equal (car pair) key)) map))

(defun eca-get (map key)
  "Return the plist value associated with KEY in MAP, or nil."
  (let ((pair (cl-find key map :key #'car :test #'equal)))
    (when pair (cdr pair))))

(defun eca-vals (map)
  "Return the plist values from MAP."
  (-map #'cdr map))

(defun eca-plist-equal (plist1 plist2)
  "Check if PLIST1 is equal to PLIST2."
  (and (= (length plist1) (length plist2))
       (cl-loop for (key val) on plist1 by #'cddr
                always (equal val (plist-get plist2 key)))))

(defun eca-find-root-for-buffer ()
  "Get the project root using git falling back to file directory."
  (or (when (fboundp 'project-current)
        (when-let* ((project (project-current)))
          (if (fboundp 'project-root)
              (project-root project)
            (car (with-no-warnings
                   (project-roots project))))))
      (when buffer-file-name (file-name-directory buffer-file-name))
      default-directory))

(defvar-local eca--session-id-cache nil)

(defvar eca--sessions '())
(defvar eca--session-ids 0)

(cl-defstruct eca--session
  ;; id to manage multiple eca sessions
  (id nil)

  ;; The status of this session
  (status 'stopped)

  ;; The eca <process>
  (process nil)

  ;; the chat buffers
  (chats '())

  (last-chat-buffer nil)

  ;; A list of workspace folders of this session
  (workspace-folders '())

  ;; A plist of request method names (strings) -> handlers used when
  ;; receiving requests from server.
  (request-handlers '())

  ;; A plist of client request ids -> handlers for pending requests used when
  ;; receiving responses from server.
  (response-handlers '())

  ;; The suported models by the server.
  (models '())

  ;; The servers and their status.
  (tool-servers '())

  ;; Model to use in the chat after session started.
  (chat-selected-model nil)

  ;; The supported chat behaviors by the server.
  (chat-behaviors nil)

  ;; Behavior to use in the chat after session started.
  (chat-selected-behavior nil)

  ;; The welcome message for new chats.
  (chat-welcome-message ""))

(defun eca-session ()
  "Return the session related to root of current buffer otherwise nil."
  (or (eca-get eca--sessions eca--session-id-cache)
      (let* ((root (funcall eca-find-root-for-buffer-function))
             (session (-first (lambda (session)
                                (--first (string= it root)
                                         (eca--session-workspace-folders session)))
                              (eca-vals eca--sessions))))
        (when session
          (setq-local eca--session-id-cache (eca--session-id session)))
        session)))

(defun eca-create-session (workspace-roots)
  "Create a new ECA session for WORKSPACE-ROOTS."
  (let ((session (make-eca--session))
        (id (cl-incf eca--session-ids)))
    (setf (eca--session-id session) id)
    (setf (eca--session-workspace-folders session) workspace-roots)
    (setq eca--sessions (eca-assoc eca--sessions id session))
    session))

(defun eca-delete-session (session)
  "Delete SESSION from existing sessions."
  (when session
    (setq eca--sessions
          (eca-dissoc eca--sessions (eca--session-id session)))))

(defun eca-assert-session-running (session)
  "Assert that a eca SESSION is running."
  (unless session
    (user-error "ECA must be running, no session found, start with `eca` command")))

(defvar eca--uri-file-prefix (pcase system-type
                               (`windows-nt "file:///")
                               (_ "file://"))
  "Prefix for a file-uri.")

(defun eca--path-to-uri (path)
  "Convert a PATH to a uri."
  (concat eca--uri-file-prefix
          (--> path
               (expand-file-name it)
               (or (file-remote-p it 'localname t) it))))

(defun eca--uri-to-path (uri)
  "Convert a file URI to a file path."
  (cond
   ((string-prefix-p "file:///" uri)
    (url-unhex-string (substring uri 8)))

   ((string-prefix-p "file://" uri)
    (url-unhex-string (substring uri 7)))


   (t uri)))

(defun eca-info (format &rest args)
  "Display eca info message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "ECA" 'face 'success) (apply #'format format args)))

(defun eca-warn (format &rest args)
  "Display eca warn message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "ECA" 'face 'warning) (apply #'format format args)))

(defun eca-error (format &rest args)
  "Display eca error message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "ECA" 'face 'error) (apply #'format format args)))

(defun eca-buttonize (base-map text callback)
  "Create a actionable TEXT that call CALLBACK when actioned.
Inheirits BASE-MAP."
  (let ((km (make-composed-keymap (make-sparse-keymap) base-map))
        (callback-int (lambda (&rest _)
                        (interactive)
                        (funcall callback))))
    (when eca-buttons-allow-mouse
      (define-key km (kbd "<mouse-1>") callback-int))
    (define-key km (kbd "<return>") callback-int)
    (define-key km (kbd "RET") callback-int)
    (propertize text
                'eca-button-on-action callback
                'pointer 'hand
                'help-echo text
                'local-map km
                'keymap km)))

(transient-define-prefix eca-transient-menu
  ()
  "ECA transient menu"
  [["Chat"
    ("n" "New" eca-chat-new)
    ("f" "New" eca-chat-select)
    ("c" "Clear" eca-chat-clear)
    ("r" "Reset" eca-chat-reset)
    ("R" "Rename" eca-chat-rename)
    ("t" "Talk" eca-chat-talk)
    ("m" "Select model" eca-chat-select-model)
    ("b" "Change behavior" eca-chat-select-behavior)
    ("o" "Open/close chat window" eca-chat-toggle-window)
    ("a" "Accept all pending tool calls" eca-chat-tool-call-accept-all)
    ("s" "Accept all pending tool calls and remember" eca-chat-tool-call-accept-all-and-remember)
    ("A" "Accept next pending tool call" eca-chat-tool-call-accept-next)]

   ["Navigation"
    ("C" "Chat" eca)
    ("M" "MCP details" eca-mcp-details)
    ("E" "Show stderr (logs)" eca-show-stderr)]

   ["Context"
    ("s" "Add to system prompt" eca-chat-add-context-to-system-prompt)
    ("u" "Add to user prompt" eca-chat-add-context-to-user-prompt)
    ("d" "Drop from system prompt" eca-chat-drop-context-from-system-prompt)]

   ["Server"
    ("R" "Restart" eca-restart)
    ("S" "Stop" eca-stop)]])

(provide 'eca-util)
;;; eca-util.el ends here
