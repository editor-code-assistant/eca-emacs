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
(require 'vc-git)
(require 'dash)
(require 'transient)

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

(defun eca-find-root-for-buffer ()
  "Get the project root using git falling back to file directory."
  (-some-> (or (vc-git-root default-directory)
               (when buffer-file-name (file-name-directory buffer-file-name))
               default-directory)
    (file-truename)))

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

  ;; the chat buffer
  (chat nil)

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

  ;; Default model to use in the chat returned by server.
  (chat-default-model nil)

  ;; Model to use in the chat selected by user after session started.
  (chat-selected-model nil)

  ;; The supported chat behaviors by the server.
  (chat-behaviors nil)

  ;; Default behavior to use in the chat returned by server.
  (chat-default-behavior nil)

  ;; Behavior to use in the chat selected by user after session started.
  (chat-selected-behavior nil)

  ;; The welcome message for new chats.
  (chat-welcome-message ""))

(defun eca-session ()
  "Return the session related to root of current buffer otherwise nil."
  (or (eca-get eca--sessions eca--session-id-cache)
      (let* ((root (eca-find-root-for-buffer))
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

(defun eca--path-to-uri (path)
  "Convert a PATH to a uri."
  (concat "file://"
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

(defun eca-buttonize (text callback)
  "Create a actionable TEXT that call CALLBACK when actioned."
  (let ((km (make-sparse-keymap))
        (callback-int (lambda (&rest _)
                        (interactive)
                        (funcall callback))))
    (define-key km (kbd "<mouse-1>") callback-int)
    (define-key km (kbd "<tab>") callback-int)
    (define-key km (kbd "<return>") callback-int)
    (propertize text
                'eca-button-on-action callback
                'pointer 'hand
                'keymap km)))

(transient-define-prefix eca-transient-menu
  ()
  "ECA transient menu"
  [["Chat"
    ("c" "Clear" eca-chat-clear)
    ("r" "Reset" eca-chat-reset)
    ("t" "Talk" eca-chat-talk)
    ("m" "Select model" eca-chat-select-model)
    ("b" "Change behavior" eca-chat-select-behavior)]

   ["Navigation"
    ("C" "Chat" eca)
    ("M" "MCP details" eca-mcp-details)
    ("E" "Show stderr (logs)" eca-show-stderr)]

   ["Server"
    ("R" "Stop" eca-restart)
    ("S" "Stop" eca-stop)]])

(provide 'eca-util)
;;; eca-util.el ends here
