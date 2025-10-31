;;; eca-chat.el --- ECA (Editor Code Assistant) chat -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) chat.
;;
;;; Code:

(require 'f)
(require 'markdown-mode)
(require 'compat)

(require 'eca-util)
(require 'eca-api)
(require 'eca-mcp)
(require 'eca-diff)

(require 'evil nil t)

;; Variables

(defcustom eca-chat-mode-hook '()
  "Hooks to run after entering in eca chat mode hook."
  :type 'hook
  :group 'eca)

(defcustom eca-chat-window-side 'right
  "Side of the frame where the ECA chat window should appear.
Can be `'left', `'right', `'top', or `'bottom'.  This setting will only
be used when `eca-chat-use-side-window' is non-nil."
  :type '(choice (const :tag "Left" left)
          (const :tag "Right" right)
          (const :tag "Top" top)
          (const :tag "Bottom" bottom))
  :group 'eca)

(defcustom eca-chat-window-width 0.40
  "Width of the ECA chat side window when opened on left or right."
  :type 'number
  :group 'eca)

(defcustom eca-chat-window-height 0.30
  "Height of the ECA chat side window when opened on top or bottom."
  :type 'number
  :group 'eca)

(defcustom eca-chat-use-side-window t
  "Whether to display ECA chat in a side window.
When non-nil (default), ECA chat opens in a dedicated side window
controlled by `eca-chat-window-side' and related settings.  When nil,
ECA chat opens in a regular buffer that follows standard
`display-buffer' behavior."
  :type 'boolean
  :group 'eca)

(defcustom eca-chat-focus-on-open t
  "Whether to focus the ECA chat window when it opens."
  :type 'boolean
  :group 'eca)

(defcustom eca-chat-auto-add-repomap nil
  "Whether to auto include repoMap context when opening eca."
  :type 'boolean
  :group 'eca)

(defcustom eca-chat-auto-add-cursor t
  "Whether to auto track cursor opened files/position and add them to context."
  :type 'boolean
  :group 'eca)

(defcustom eca-chat-cursor-context-debounce 0.3
  "Seconds to debounce updates when tracking cursor to context."
  :type 'number
  :group 'eca)

(defcustom eca-chat-prompt-separator "\n---"
  "The separator text between chat and prompt area."
  :type 'string
  :group 'eca)

(defcustom eca-chat-prompt-prefix "> "
  "The prompt prefix string used in eca chat buffer."
  :type 'string
  :group 'eca)

(defcustom eca-chat-prompt-prefix-loading "⏳ "
  "The prompt prefix string used in eca chat buffer when loading."
  :type 'string
  :group 'eca)

(defcustom eca-chat-context-prefix "@"
  "The context prefix string used in eca chat buffer."
  :type 'string
  :group 'eca)

(defcustom eca-chat-filepath-prefix "#"
  "The filepath prefix string used in eca chat buffer."
  :type 'string
  :group 'eca)

(defcustom eca-chat-expandable-block-open-symbol "⏵ "
  "The string used in eca chat buffer for blocks in open mode like tool calls."
  :type 'string
  :group 'eca)

(defcustom eca-chat-expandable-block-close-symbol "⏷ "
  "The string used in eca chat buffer for blocks in close mode like tool calls."
  :type 'string
  :group 'eca)

(defcustom eca-chat-mcp-tool-call-loading-symbol "⏳"
  "The string used in eca chat buffer for mcp tool calls while loading."
  :type 'string
  :group 'eca)

(defcustom eca-chat-mcp-tool-call-error-symbol "❌"
  "The string used in eca chat buffer for mcp tool calls when error."
  :type 'string
  :group 'eca)

(defcustom eca-chat-mcp-tool-call-success-symbol "✅"
  "The string used in eca chat buffer for mcp tool calls when success."
  :type 'string
  :group 'eca)

(defcustom eca-chat-expand-pending-approval-tools t
  "Whether to auto expand tool calls when pending approval."
  :type 'boolean
  :group 'eca)

(defcustom eca-chat-shrink-called-tools t
  "Whether to auto shrink tool calls after called."
  :type 'boolean
  :group 'eca)

(defcustom eca-chat-custom-model nil
  "Which model to use during chat, nil means use server's default.
Must be a valid model supported by server, check `eca-chat-select-model`."
  :type 'string
  :group 'eca)

(defcustom eca-chat-custom-behavior nil
  "Which chat behavior to use, if nil use server's default."
  :type 'string
  :group 'eca)

(defcustom eca-chat-usage-string-format '(:session-tokens " / " :context-limit " (" :session-cost ")")
  "Format to show about chat usage tokens/costs."
  :type '(repeat
          (choice
           (string :tag "any string like separators")
           (const :tag "Total tokens sent + received" :session-tokens)
           (const :tag "Total session cost" :session-cost)
           (const :tag "The context limit" :context-limit)
           (const :tag "The output limit" :output-limit)
           (const :tag "Last message cost" :last-message-cost)))
  :group 'eca)

(defcustom eca-chat-diff-tool 'smerge
  "Select the method for displaying file-change diffs in ECA chat."
  :type '(choice (const :tag "Side-by-side Ediff" ediff)
          (const :tag "Merge-style Smerge" smerge))
  :group 'eca)

(defcustom eca-chat-tool-call-prepare-throttle 'smart
  "Throttle strategy for handling `toolCallPrepare` events.
Possible values: `all` or `smart` (default)."
  :type '(choice (const :tag "Process all updates" all)
          (const :tag "Smart throttle" smart))
  :group 'eca)

(defcustom eca-chat-tool-call-prepare-update-interval 5
  "When `smart`, process every Nth `toolCallPrepare` update.
Must be a positive integer."
  :type 'integer
  :group 'eca)

(defvar-local eca-chat--tool-call-prepare-counters (make-hash-table :test 'equal)
  "Hash table mapping toolCall ID to message count.")

(defvar-local eca-chat--tool-call-prepare-content-cache (make-hash-table :test 'equal)
  "Hash table mapping toolCall ID to accumulated argument text.")

(defcustom eca-chat-tool-call-approval-content-size 0.9
  "The size of font of tool call approval."
  :type 'number
  :group 'eca)

;; Faces

(defface eca-chat-prompt-prefix-face
  '((t (:foreground "lime green" :weight bold)))
  "Face for the `eca-chat-prompt-prefix`."
  :group 'eca)

(defface eca-chat-prompt-stop-face
  '((t (:inherit error :underline t :weight bold)))
  "Face for the stop action when loading."
  :group 'eca)

(defface eca-chat-tool-call-approval-content-face
  `((t :height ,eca-chat-tool-call-approval-content-size))
  "Face for the MCP tool calls approval content in chat."
  :group 'eca)

(defface eca-chat-tool-call-accept-face
  `((t (:inherit success :height ,eca-chat-tool-call-approval-content-size :underline t :weight bold)))
  "Face for the accept tool call action."
  :group 'eca)

(defface eca-chat-tool-call-accept-and-remember-face
  `((t (:inherit success :height ,eca-chat-tool-call-approval-content-size :underline t :weight bold)))
  "Face for the accept and remember tool call action."
  :group 'eca)

(defface eca-chat-tool-call-reject-face
  `((t (:inherit error :height ,eca-chat-tool-call-approval-content-size :underline t :weight bold)))
  "Face for the cancel tool call action."
  :group 'eca)

(defface eca-chat-tool-call-keybinding-face
  `((t :inherit font-lock-comment-face :height ,eca-chat-tool-call-approval-content-size))
  "Face for the tool call keybinding in chat."
  :group 'eca)

(defface eca-chat-tool-call-spacing-face
  `((t :height ,eca-chat-tool-call-approval-content-size))
  "Face for the tool call spacing in chat."
  :group 'eca)

(defface eca-chat-diff-view-face
  '((t (:foreground "dodger blue" :underline t :weight bold)))
  "Face for the diff view button."
  :group 'eca)

(defface eca-chat-context-unlinked-face
  '((t (:foreground "gold" :height 0.9)))
  "Face for contexts to be added."
  :group 'eca)

(defface eca-chat-context-file-face
  '((t (:foreground "coral" :underline t :height 0.9)))
  "Face for contexts of file type."
  :group 'eca)

(defface eca-chat-context-repo-map-face
  '((t (:foreground "turquoise" :underline t :height 0.9)))
  "Face for contexts of repoMap type."
  :group 'eca)

(defface eca-chat-context-mcp-resource-face
  '((t (:foreground "lime green" :underline t :height 0.9)))
  "Face for contexts of mcpResource type."
  :group 'eca)

(defface eca-chat-context-cursor-face
  '((t (:foreground "gainsboro" :underline t :height 0.9)))
  "Face for contexts of cursor type."
  :group 'eca)

(defface eca-chat-title-face
  '((t :height 0.9))
  "Face for the chat title."
  :group 'eca)

(defface eca-chat-user-messages-face
  '((t :inherit font-lock-doc-face))
  "Face for the user sent messages in chat."
  :group 'eca)

(defface eca-chat-system-messages-face
  '((t :inherit font-lock-builtin-face))
  "Face for the system messages in chat."
  :group 'eca)

(defface eca-chat-reason-label-face
  '((t :inherit font-lock-comment-face))
  "Face for the reason messages in chat."
  :group 'eca)

(defface eca-chat-hook-label-face
  '((t :inherit font-lock-keyword-face))
  "Face for the hook messages in chat."
  :group 'eca)

(defface eca-chat-time-face
  '((t :inherit font-lock-comment-face :slant italic :height 0.8))
  "Face for times spent in chat."
  :group 'eca)

(defface eca-chat-mcp-tool-call-label-face
  '((t :inherit font-lock-function-call-face))
  "Face for the MCP tool calls in chat."
  :group 'eca)

(defface eca-chat-file-change-label-face
  '((t :inherit diff-file-header))
  "Face for file changes labels in chat."
  :group 'eca)

(defface eca-chat-file-path-face
  '((t :inherit link))
  "Face for file paths in chat."
  :group 'eca)

(defface eca-chat--tool-call-table-key-face
  '((t :height 0.9 :inherit font-lock-comment-face))
  "Face for the MCP tool call table keys in chat."
  :group 'eca)

(defface eca-chat--tool-call-argument-key-face
  '()
  "Face for the MCP tool calls's argument key in chat."
  :group 'eca)

(defface eca-chat--tool-call-argument-value-face
  '((t :weight bold))
  "Face for the MCP tool calls's argument value in chat."
  :group 'eca)

(defface eca-chat-welcome-face
  '((t :inherit font-lock-builtin-face))
  "Face for the welcome message in chat."
  :group 'eca)

(defface eca-chat-option-key-face
  '((t :inherit font-lock-doc-face))
  "Face for the option keys in header-line of the chat."
  :group 'eca)

(defface eca-chat-option-value-face
  '((t :weight bold))
  "Face for the option values in header-line of the chat."
  :group 'eca)

(defface eca-chat-usage-string-face
  '((t :inherit font-lock-doc-face))
  "Face for the strings segments in usage string in mode-line of the chat."
  :group 'eca)

(defface eca-chat-command-description-face
  '((t :inherit font-lock-comment-face))
  "Face for the descriptions in chat command completion."
  :group 'eca)

;; Internal

(defvar-local eca-chat--closed nil)
(defvar-local eca-chat--history '())
(defvar-local eca-chat--history-index -1)
(defvar-local eca-chat--id nil)
(defvar-local eca-chat--title nil)
(defvar-local eca-chat--custom-title nil)
(defvar-local eca-chat--last-request-id 0)
(defvar-local eca-chat--context-completion-cache (make-hash-table :test 'equal))
(defvar-local eca-chat--file-completion-cache (make-hash-table :test 'equal))
(defvar-local eca-chat--command-completion-cache (make-hash-table :test 'equal))
(defvar-local eca-chat--context '())
(defvar-local eca-chat--spinner-string "")
(defvar-local eca-chat--spinner-timer nil)
(defvar-local eca-chat--progress-text "")
(defvar-local eca-chat--last-user-message-pos nil)
(defvar-local eca-chat--chat-loading nil)
(defvar-local eca-chat--session-cost nil)
(defvar-local eca-chat--message-cost nil)
(defvar-local eca-chat--message-input-tokens nil)
(defvar-local eca-chat--message-output-tokens nil)
(defvar-local eca-chat--session-tokens nil)
(defvar-local eca-chat--session-limit-context nil)
(defvar-local eca-chat--session-limit-output nil)
(defvar-local eca-chat--empty t)
(defvar-local eca-chat--cursor-context nil)

;; Timer used to debounce post-command driven context updates
(defvar eca-chat--cursor-context-timer nil)
(defvar eca-chat--new-chat-id 0)

(defun eca-chat-new-buffer-name (session)
  "Return the chat buffer name for SESSION."
  (format "<eca-chat:%s:%s>" (eca--session-id session) eca-chat--new-chat-id))

(defvar eca-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map markdown-mode-map)
    (define-key map (kbd "S-<return>") #'eca-chat--key-pressed-newline)
    (define-key map (kbd "C-<up>") #'eca-chat--key-pressed-previous-prompt-history)
    (define-key map (kbd "C-<down>") #'eca-chat--key-pressed-next-prompt-history)
    (define-key map (kbd "<return>") #'eca-chat--key-pressed-return)
    (define-key map (kbd "RET") #'eca-chat--key-pressed-return)
    (define-key map (kbd "C-c C-<return>") #'eca-chat-send-prompt-at-chat)
    (define-key map (kbd "<tab>") #'eca-chat--key-pressed-tab)
    (define-key map (kbd "C-c C-k") #'eca-chat-reset)
    (define-key map (kbd "C-c C-l") #'eca-chat-clear)
    (define-key map (kbd "C-c C-t") #'eca-chat-talk)
    (define-key map (kbd "C-c C-b") #'eca-chat-select-behavior)
    (define-key map (kbd "C-c C-m") #'eca-chat-select-model)
    (define-key map (kbd "C-c C-n") #'eca-chat-new)
    (define-key map (kbd "C-c C-f") #'eca-chat-select)
    (define-key map (kbd "C-c C-p") #'eca-chat-repeat-prompt)
    (define-key map (kbd "C-c C-d") #'eca-chat-clear-prompt)
    (define-key map (kbd "C-c C-h") #'eca-chat-timeline)
    (define-key map (kbd "C-c C-a") #'eca-chat-tool-call-accept-all)
    (define-key map (kbd "C-c C-S-a") #'eca-chat-tool-call-accept-next)
    (define-key map (kbd "C-c C-s") #'eca-chat-tool-call-accept-all-and-remember)
    (define-key map (kbd "C-c C-r") #'eca-chat-tool-call-reject-next)
    (define-key map (kbd "C-c C-S-r") #'eca-chat-rename)
    (define-key map (kbd "C-c .") #'eca-transient-menu)
    (define-key map (kbd "C-c C-,") #'eca-mcp-details)
    (define-key map (kbd "C-c C-<up>") #'eca-chat-go-to-prev-user-message)
    (define-key map (kbd "C-c C-<down>") #'eca-chat-go-to-next-user-message)
    (define-key map (kbd "C-c <up>") #'eca-chat-go-to-prev-expandable-block)
    (define-key map (kbd "C-c <down>") #'eca-chat-go-to-next-expandable-block)
    (define-key map (kbd "C-c <tab>") #'eca-chat-toggle-expandable-block)
    map)
  "Keymap used by `eca-chat-mode'.")

(defun eca-chat--get-last-buffer (session)
  "Get the eca chat buffer for SESSION."
  (or (when-let (last-buff (eca--session-last-chat-buffer session))
        (when (buffer-live-p last-buff)
          last-buff))
      (get-buffer (eca-chat-new-buffer-name session))))

(defun eca-chat--create-buffer (session)
  "Create the eca chat buffer for SESSION."
  (get-buffer-create (generate-new-buffer-name (eca-chat-new-buffer-name session))))

(defun eca-chat--get-chat-buffer (session chat-id)
  "Get chat buffer for SESSION and CHAT-ID."
  (or (eca-get (eca--session-chats session) chat-id)
      ;; new chat, we rename empty to chat-id
      (let ((empty-chat-buffer (eca-get (eca--session-chats session) 'empty)))
        (setf (eca--session-chats session)
              (-> (eca--session-chats session)
                  (eca-assoc chat-id empty-chat-buffer)
                  (eca-dissoc 'empty)))
        empty-chat-buffer)))

(defun eca-chat--insert (&rest contents)
  "Insert CONTENTS reseting undo-list to avoid buffer inconsistencies."
  (apply #'insert contents)
  (setq-local buffer-undo-list nil))

(defmacro eca-chat--allow-write (&rest body)
  "Execute BODY allowing write to buffer."
  `(let ((inhibit-read-only t))
     ,@body))

(defmacro eca-chat--with-current-buffer (buffer &rest body)
  "Eval BODY inside chat BUFFER."
  (declare (indent 1) (debug t))
  `(with-current-buffer ,buffer
     (let ((inhibit-read-only t))
       ,@body)))

(defun eca-chat--spinner-start (callback)
  "Start modeline spinner calling CALLBACK when updating."
  (eca-chat--allow-write
   (setq eca-chat--spinner-timer
         (run-with-timer
          0
          0.5
          (lambda ()
            (when eca-chat--spinner-timer
              (if (eq 3 (length eca-chat--spinner-string))
                  (setq eca-chat--spinner-string ".")
                (setq eca-chat--spinner-string (concat eca-chat--spinner-string ".")))
              (funcall callback)))))))

(defun eca-chat--spinner-stop ()
  "Stop modeline spinner."
  (when eca-chat--spinner-timer
    (cancel-timer eca-chat--spinner-timer)
    (setq eca-chat--spinner-timer nil))
  (setq eca-chat--spinner-string ""))

(defun eca-chat--time->presentable-time (ms)
  "Return a presentable time for MS."
  (let ((secs (/ (float ms) 1000)))
    (propertize (format "%.2f s" secs) 'font-lock-face 'eca-chat-time-face)))

(defun eca-chat--behavior (session)
  "The chat behavior considering what's in SESSION and user option."
  (or eca-chat-custom-behavior
      (eca--session-chat-selected-behavior session)))

(defun eca-chat--model (session)
  "The chat model considering what's in SESSION and user option."
  (or eca-chat-custom-model
      (eca--session-chat-selected-model session)))

(defun eca-chat--mcps-summary (session)
  "The summary of MCP servers for SESSION."
  (let* ((running 0) (starting 0) (failed 0)
         (propertize-fn (lambda (n face &optional add-slash?)
                          (unless (zerop n)
                            (concat
                             (propertize (number-to-string n) 'font-lock-face face)
                             (when add-slash? (propertize "/" 'font-lock-face 'font-lock-comment-face))))))
         (mcp-servers (eca-mcp-servers session)))
    (if (seq-empty-p mcp-servers)
        "0"
      (progn
        (seq-doseq (mcp-server mcp-servers)
          (pcase (plist-get mcp-server :status)
            ("running" (cl-incf running))
            ("starting" (cl-incf starting))
            ("failed" (cl-incf failed))))
        (concat (funcall propertize-fn failed 'error (or (> running 0) (> starting 0)))
                (funcall propertize-fn starting 'warning (> running 0))
                (funcall propertize-fn running 'success))))))

(defun eca-chat--build-tool-call-approval-str-content (session id spacing-line-prefix)
  "Build the tool call approval string for SESSION, ID and SPACING-LINE-PREFIX."
  (let ((keybinding-for (lambda (command)
                          (concat "("
                                  (key-description (car (where-is-internal command eca-chat-mode-map)))
                                  ")"))))
    (concat (propertize "\n" 'font-lock-face 'eca-chat-tool-call-spacing-face)
            (eca-buttonize
             eca-chat-mode-map
             (propertize "Accept"
                         'eca-tool-call-pending-approval-accept t
                         'line-prefix spacing-line-prefix
                         'font-lock-face 'eca-chat-tool-call-accept-face)
             (lambda ()
               (eca-api-notify session
                               :method "chat/toolCallApprove"
                               :params (list :chatId eca-chat--id
                                             :toolCallId id))))
            (propertize " " 'font-lock-face 'eca-chat-tool-call-approval-content-face)
            (propertize (funcall keybinding-for #'eca-chat-tool-call-accept-all)
                        'font-lock-face 'eca-chat-tool-call-keybinding-face)
            (propertize "\n" 'font-lock-face 'eca-chat-tool-call-spacing-face)
            (eca-buttonize
             eca-chat-mode-map
             (propertize "Accept and remember"
                         'eca-tool-call-pending-approval-accept-and-remember t
                         'line-prefix spacing-line-prefix
                         'font-lock-face 'eca-chat-tool-call-accept-and-remember-face)
             (lambda ()
               (eca-api-notify session
                               :method "chat/toolCallApprove"
                               :params (list :chatId eca-chat--id
                                             :save "session"
                                             :toolCallId id))))
            (propertize " for this session "
                        'font-lock-face 'eca-chat-tool-call-approval-content-face)
            (propertize (funcall keybinding-for #'eca-chat-tool-call-accept-all-and-remember)
                        'font-lock-face 'eca-chat-tool-call-keybinding-face)
            (propertize "\n" 'font-lock-face 'eca-chat-tool-call-spacing-face)
            (eca-buttonize
             eca-chat-mode-map
             (propertize "Reject"
                         'eca-tool-call-pending-approval-reject t
                         'line-prefix spacing-line-prefix
                         'font-lock-face 'eca-chat-tool-call-reject-face)
             (lambda ()
               (eca-api-notify session
                               :method "chat/toolCallReject"
                               :params (list :chatId eca-chat--id
                                             :toolCallId id))))
            (propertize " and tell ECA what to do differently "
                        'font-lock-face 'eca-chat-tool-call-approval-content-face)
            (propertize (funcall keybinding-for #'eca-chat-tool-call-reject-next)
                        'font-lock-face 'eca-chat-tool-call-keybinding-face))))

(defun eca-chat--insert-prompt-string ()
  "Insert the prompt and context string adding overlay metadatas."
  (let ((prompt-area-ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)) (current-buffer))))
    (overlay-put prompt-area-ov 'eca-chat-prompt-area t))
  (eca-chat--insert eca-chat-prompt-separator)
  (let ((progress-area-ov (make-overlay (line-beginning-position) (line-end-position) (current-buffer) nil t)))
    (overlay-put progress-area-ov 'eca-chat-progress-area t)
    (eca-chat--insert "\n")
    (move-overlay progress-area-ov (overlay-start progress-area-ov) (1- (overlay-end progress-area-ov))))
  (let ((context-area-ov (make-overlay (line-beginning-position) (line-end-position) (current-buffer) nil t)))
    (overlay-put context-area-ov 'eca-chat-context-area t)
    (eca-chat--insert (propertize eca-chat-context-prefix 'font-lock-face 'eca-chat-context-unlinked-face))
    (eca-chat--insert "\n")
    (move-overlay context-area-ov (overlay-start context-area-ov) (1- (overlay-end context-area-ov))))
  (let ((prompt-field-ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)) (current-buffer))))
    (overlay-put prompt-field-ov 'eca-chat-prompt-field t)
    (overlay-put prompt-field-ov 'before-string (propertize eca-chat-prompt-prefix 'font-lock-face 'eca-chat-prompt-prefix-face))))

(defun eca-chat--clear ()
  "Clear the chat for SESSION."
  (erase-buffer)
  (remove-overlays (point-min) (point-max))
  (eca-chat--insert "\n")
  (eca-chat--insert-prompt-string)
  (eca-chat--refresh-context))

(defun eca-chat--stop-prompt (session)
  "Stop the running chat prompt for SESSION."
  (when eca-chat--chat-loading
    (eca-api-notify session
                    :method "chat/promptStop"
                    :params (list :chatId eca-chat--id))
    (eca-chat--set-chat-loading session nil)))

(defun eca-chat--set-chat-loading (session loading)
  "Set the SESSION chat to a loading state if LOADING is non nil.
Otherwise to a not loading state."
  (unless (eq eca-chat--chat-loading loading)
    (setq-local eca-chat--chat-loading loading)
    (setq-local buffer-read-only loading)
    (let ((prompt-field-ov (eca-chat--prompt-field-ov))
          (stop-text (eca-buttonize
                      eca-chat-mode-map
                      (propertize "stop" 'font-lock-face 'eca-chat-prompt-stop-face)
                      (lambda () (eca-chat--stop-prompt session)))))
      (if eca-chat--chat-loading
          (progn
            (overlay-put prompt-field-ov 'before-string (propertize eca-chat-prompt-prefix-loading 'font-lock-face 'default))
            (save-excursion
              (goto-char (overlay-start prompt-field-ov))
              (eca-chat--insert stop-text)))
        (progn
          (overlay-put prompt-field-ov 'before-string (propertize eca-chat-prompt-prefix 'font-lock-face 'eca-chat-prompt-prefix-face))
          (save-excursion
            (goto-char (overlay-start prompt-field-ov))
            (delete-region (point) (+ (point) (length stop-text)))))))))

(defun eca-chat--set-prompt (text)
  "Set the chat prompt to be TEXT."
  (-some-> (eca-chat--prompt-field-start-point) (goto-char))
  (delete-region (point) (point-max))
  (eca-chat--insert text))

(defun eca-chat--cycle-history (n)
  "Cycle history by N."
  (when (and eca-chat--history (eca-chat--point-at-prompt-field-p))
    (when (and (>= (+ eca-chat--history-index n) 0)
               (nth (+ eca-chat--history-index n) eca-chat--history))
      (cl-incf eca-chat--history-index n)
      (eca-chat--set-prompt (nth eca-chat--history-index eca-chat--history)))))

(defun eca-chat--key-pressed-previous-prompt-history ()
  "Cycle previous the prompt history."
  (interactive)
  (eca-chat--cycle-history 1))

(defun eca-chat--key-pressed-next-prompt-history ()
  "Cycle next the prompt history."
  (interactive)
  (eca-chat--cycle-history -1))

(defun eca-chat--key-pressed-newline ()
  "Insert a newline character at point."
  (interactive)
  (when (>= (point) (eca-chat--prompt-field-start-point))
    (eca-chat--insert "\n")))

(defun eca-chat--prompt-field-ov ()
  "Return the overlay for the prompt field."
  (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-prompt-field)))
          (overlays-in (point-min) (point-max))))

(defun eca-chat--prompt-field-start-point ()
  "Return the metadata overlay for the prompt field start point."
  (overlay-start (eca-chat--prompt-field-ov)))

(defun eca-chat--prompt-progress-field-ov ()
  "Return the overlay for the progress field."
  (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-progress-area)))
          (overlays-in (point-min) (point-max))))

(defun eca-chat--prompt-context-field-ov ()
  "Return the overlay for the context field."
  (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-context-area)))
          (overlays-in (point-min) (point-max))))

(defun eca-chat--prompt-area-ov ()
  "Return the overlay for the prompt area."
  (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-prompt-area)))
          (overlays-in (point-min) (point-max))))

(defun eca-chat--prompt-area-start-point ()
  "Return the metadata overlay for the prompt area start point."
  (-some-> (eca-chat--prompt-area-ov)
    (overlay-start)))

(defun eca-chat--new-context-start-point ()
  "Return the metadata overlay for the new context area start point."
  (-some-> (eca-chat--prompt-context-field-ov)
    (overlay-start)))

(defun eca-chat--key-pressed-deletion (side-effect-fn &rest args)
  "Apply SIDE-EFFECT-FN with ARGS before point.
Unless at the prompt field boundary.
Checks if it's in a context, removing it if so.
This is similar to actions like `backward-delete-char' but protects
the prompt/context line."
  (if (derived-mode-p 'eca-chat-mode)
      (let* ((cur-ov (car (overlays-in (line-beginning-position) (line-end-position))))
             (text (thing-at-point 'symbol))
             (in-prompt? (eca-chat--point-at-prompt-field-p))
             (context-item (-some->> text
                             (get-text-property 0 'eca-chat-context-item)))
             (item-str-length (-some->> text
                                (get-text-property 0 'eca-chat-item-str-length))))
        (cond
         ;; expandable item in context area
         ((and cur-ov
               context-item
               (not in-prompt?))
          (setq-local eca-chat--context (delete context-item eca-chat--context))
          (eca-chat--refresh-context))

         ;; expandable item in prompt
         ((and cur-ov
               item-str-length
               in-prompt?)
          (while (and (not (eolp))
                      (get-text-property (point) 'eca-chat-item-str-length))
            (forward-char 1))
          (delete-region (- (point) item-str-length) (point)))

         ;; start of the prompt
         ((and cur-ov
               (or (<= (point) (overlay-start cur-ov))
                   (and (eq 'backward-kill-word this-command)
                        (string-blank-p (buffer-substring-no-properties (line-beginning-position) (point))))))
          (ding))

         ;; in context area trying to remove a context space separator
         ((and cur-ov
               (overlay-get cur-ov 'eca-chat-context-area)
               (and (string= " " (string (char-before (point))))
                    (not (eolp))))
          )

         ;; in context area removing a context
         ((and cur-ov
               (overlay-get cur-ov 'eca-chat-context-area)
               (string= eca-chat-context-prefix (string (char-before (point)))))
          (setq-local eca-chat--context (delete (car (last eca-chat--context)) eca-chat--context))
          (eca-chat--refresh-context)
          (end-of-line))

         (t (apply side-effect-fn args))))
    (apply side-effect-fn args)))

(defun eca-chat--refine-context (context)
  "Refine CONTEXT before sending in prompt."
  (pcase (plist-get context :type)
    ("cursor" (-> context
                  (plist-put :path (plist-get eca-chat--cursor-context :path))
                  (plist-put :position (plist-get eca-chat--cursor-context :position))))
    (_ context)))

(defun eca-chat--normalize-prompt (prompt)
  "Normalize PROMPT before sending to server.
- If any expandable (@context or #file) is found, expand it.
- Removes # from #files."
  (let ((result "")
        (pos 0))
    (while (< pos (length prompt))
      (let* ((next-change (next-single-property-change pos 'eca-chat-expanded-item-str prompt (length prompt)))
             (expanded-str (get-text-property pos 'eca-chat-expanded-item-str prompt))
             (item-type (get-text-property pos 'eca-chat-item-type prompt)))
        (if expanded-str
            (setq result (concat result (if (eq 'filepath item-type)
                                            (substring expanded-str 1)
                                          expanded-str)))
          (setq result (concat result (substring prompt pos next-change))))
        (setq pos next-change)))
    result))

(defun eca-chat--send-prompt (session prompt)
  "Send PROMPT to server for SESSION."
  (let* ((prompt-start (eca-chat--prompt-field-start-point))
         (refined-contexts (-map #'eca-chat--refine-context eca-chat--context)))
    (when (seq-empty-p eca-chat--history) (eca-chat--clear))
    (add-to-list 'eca-chat--history prompt)
    (setq eca-chat--history-index -1)
    (goto-char prompt-start)
    (delete-region (point) (point-max))
    (eca-chat--set-chat-loading session t)
    (eca-api-request-async
     session
     :method "chat/prompt"
     :params (list :message (eca-chat--normalize-prompt prompt)
                   :request-id (cl-incf eca-chat--last-request-id)
                   :chatId eca-chat--id
                   :model (eca-chat--model session)
                   :behavior (eca-chat--behavior session)
                   :contexts (vconcat refined-contexts))
     :success-callback (-lambda (res)
                         (setq-local eca-chat--id (plist-get res :chatId))))))

(defun eca-chat--key-pressed-return ()
  "Send the current prompt to eca process if in prompt."
  (interactive)
  (eca-chat--allow-write
   (let* ((prompt-start (eca-chat--prompt-field-start-point))
          (session (eca-session))
          (prompt (save-excursion
                    (goto-char prompt-start)
                    (string-trim (buffer-substring (point) (point-max))))))
     (cond
      ;; check prompt
      ((and (not (string-empty-p prompt))
            (not eca-chat--chat-loading))
       (eca-chat--send-prompt session prompt))

      ;; check it's an actionable text
      ((-some->> (thing-at-point 'symbol) (get-text-property 0 'eca-button-on-action))
       (-some->> (thing-at-point 'symbol)
         (get-text-property 0 'eca-button-on-action)
         (funcall)))

      ;; check is inside a expandable text
      ((eca-chat--expandable-content-at-point)
       (let ((ov (eca-chat--expandable-content-at-point)))
         (eca-chat--expandable-content-toggle (overlay-get ov 'eca-chat--expandable-content-id))))

      (t nil)))))

(defun eca-chat--key-pressed-tab ()
  "Expand tool call if point is at expandable content, or use default behavior."
  (interactive)
  (cond
   ;; expandable toggle
   ((eca-chat--expandable-content-at-point)
    (eca-chat--allow-write
     (eca-chat--expandable-content-toggle (overlay-get (eca-chat--expandable-content-at-point) 'eca-chat--expandable-content-id))))

   ;; context completion
   ((and (eca-chat--prompt-context-field-ov)
         (eolp))
    (completion-at-point))

   (t t)))

(defun eca-chat--point-at-new-context-p ()
  "Return non-nil if point is at the context area."
  (and (eq (line-number-at-pos (point))
           (line-number-at-pos (eca-chat--new-context-start-point)))
       (eolp)))

(defun eca-chat--point-at-prompt-field-p ()
  "Return non-nil if point is at the prompt field area."
  (let ((prompt-start (eca-chat--prompt-field-start-point)))
    (and prompt-start
         (>= (point) prompt-start))))

(defun eca-chat--header-line-string (session)
  "Update chat header line for SESSION."
  (when session
    (let ((model-keymap (make-sparse-keymap))
          (behavior-keymap (make-sparse-keymap))
          (mcp-keymap (make-sparse-keymap)))
      (define-key model-keymap (kbd "<header-line> <mouse-1>") #'eca-chat-select-model)
      (define-key behavior-keymap (kbd "<header-line> <mouse-1>") #'eca-chat-select-behavior)
      (define-key mcp-keymap (kbd "<header-line> <mouse-1>") #'eca-mcp-details)
      (list (propertize "model:"
                        'font-lock-face 'eca-chat-option-key-face
                        'pointer 'hand
                        'keymap model-keymap)
            (-some-> (eca-chat--model session)
              (propertize
               'font-lock-face 'eca-chat-option-value-face
               'pointer 'hand
               'keymap model-keymap))
            "  "
            (propertize "behavior:"
                        'font-lock-face 'eca-chat-option-key-face
                        'pointer 'hand
                        'keymap behavior-keymap)
            (-some-> (eca-chat--behavior session)
              (propertize 'font-lock-face 'eca-chat-option-value-face
                          'pointer 'hand
                          'keymap behavior-keymap))
            "  "
            (propertize "mcps:"
                        'font-lock-face 'eca-chat-option-key-face
                        'pointer 'hand
                        'keymap mcp-keymap)
            (propertize (eca-chat--mcps-summary session)
                        'pointer 'hand
                        'keymap mcp-keymap)))))

(defun eca-chat--number->friendly-number (n)
  "Format N as `x.yM` for |N| >= 1M, `x.yK` for |N| >= 1K.
Otherwise show plain integer."
  (cond
   ((>= (abs n) 1000000)
    (let* ((m (/ (abs n) 1000000.0))
           (s (format "%.1f" m))
           (s (if (string-match "\\.0\\'" s) (substring s 0 -2) s)))
      (concat (if (< n 0) "-" "") s "M")))
   ((>= (abs n) 1000)
    (let* ((k (/ (abs n) 1000.0))
           (s (format "%.1f" k))
           (s (if (string-match "\\.0\\'" s) (substring s 0 -2) s)))
      (concat (if (< n 0) "-" "") s "K")))
   (t (number-to-string n))))

(defun eca-chat--mode-line-string ()
  "Update chat mode line."
  (let* ((usage-str
          (when (or eca-chat--message-input-tokens
                    eca-chat--message-output-tokens
                    eca-chat--session-tokens
                    eca-chat--message-cost
                    eca-chat--session-cost)
            (-> (-map (lambda (segment)
                        (pcase segment
                          (:message-input-tokens (eca-chat--number->friendly-number eca-chat--message-input-tokens))
                          (:message-output-tokens (eca-chat--number->friendly-number eca-chat--message-output-tokens))
                          (:session-tokens (eca-chat--number->friendly-number eca-chat--session-tokens))
                          (:message-cost (concat "$" eca-chat--message-cost))
                          (:session-cost (concat "$" eca-chat--session-cost))
                          (:context-limit (eca-chat--number->friendly-number eca-chat--session-limit-context))
                          (:output-limit (eca-chat--number->friendly-number eca-chat--session-limit-output))
                          (_ (propertize segment 'font-lock-face 'eca-chat-usage-string-face))))
                      eca-chat-usage-string-format)
                (string-join ""))))
         (fill-space (propertize " "
                                 'display `((space :align-to (- right ,(+ 1 (length usage-str))))))))
    (concat
     (when eca-chat--closed
       (propertize "*Closed session*" 'font-lock-face 'eca-chat-system-messages-face))
     (cond
      (eca-chat--custom-title
       (propertize eca-chat--custom-title 'font-lock-face 'eca-chat-title-face))
      (eca-chat--title
       (propertize eca-chat--title 'font-lock-face 'eca-chat-title-face)))
     fill-space
     usage-str)))

(defun eca-chat--select-window ()
  "Select the Window."
  (select-window (get-buffer-window (buffer-name))))

(defun eca-chat--display-buffer (buffer)
  "Display BUFFER in a side window according to customization.
The window is displayed on the side specified by
`eca-chat-window-side' with dimensions from
`eca-chat-window-width' or `eca-chat-window-height'.
If `eca-chat-focus-on-open' is non-nil, the window is selected."
  (let ((window
         (if eca-chat-use-side-window
             ;; Use side window
             (let* ((side eca-chat-window-side)
                    (slot 0)
                    (window-parameters '((no-delete-other-windows . t)))
                    (display-buffer-alist
                     `((,(regexp-quote (buffer-name buffer))
                        (display-buffer-in-side-window)
                        (side . ,side)
                        (slot . ,slot)
                        ,@(when (memq side '(left right))
                            `((window-width . ,eca-chat-window-width)))
                        ,@(when (memq side '(top bottom))
                            `((window-height . ,eca-chat-window-height)))
                        (window-parameters . ,window-parameters)))))
               (display-buffer buffer))
           ;; Use regular buffer
           (display-buffer buffer))))
    ;; Select the window to give it focus if configured to do so
    (when (and window eca-chat-focus-on-open)
      (select-window window))
    window))

(defun eca-chat--pop-window ()
  "Pop eca dedicated window if it exists."
  (let ((buffer (current-buffer)))
    (eca-chat--display-buffer buffer)))

(defun eca-chat--mark-header ()
  "Mark last messages header."
  (let ((context-start (eca-chat--prompt-area-start-point)))
    (save-excursion
      (goto-char context-start)
      (goto-char (1- (point)))
      (setq-local eca-chat--last-user-message-pos (point)))))

(defun eca-chat--add-header (content)
  "Add CONTENT to the chat just after last user input."
  (when eca-chat--last-user-message-pos
    (save-excursion
      (goto-char eca-chat--last-user-message-pos)
      (eca-chat--insert content))))

(defun eca-chat--add-text-content (text &optional overlay-key overlay-value)
  "Add TEXT to the chat current position.
Add a overlay before with OVERLAY-KEY = OVERLAY-VALUE if passed."
  (let ((context-start (eca-chat--prompt-area-start-point)))
    (save-excursion
      (goto-char context-start)
      (goto-char (1- (point)))
      (when overlay-key
        (let ((ov (make-overlay (point) (point) (current-buffer))))
          (overlay-put ov overlay-key overlay-value)
          (when (eq overlay-key 'eca-chat--user-message-id)
            (overlay-put ov 'eca-chat--timestamp (float-time)))))
      (eca-chat--insert text)
      (point))))

(defun eca-chat--expandable-content-at-point ()
  "Return expandable content overlay at point, or nil if none."
  (-first (-lambda (ov) (overlay-get ov 'eca-chat--expandable-content-id))
          (overlays-in (line-beginning-position) (point))))

(defun eca-chat--get-expandable-content (id)
  "Return the overlay if there is a expandable content for ID."
  (-first (-lambda (ov) (string= id (overlay-get ov 'eca-chat--expandable-content-id)))
          (overlays-in (point-min) (point-max))))

(defun eca-chat--propertize-only-first-word (str &rest properties)
  "Return a new string propertizing PROPERTIES to the first word of STR.
If STR is empty or PROPERTIES is nil, return STR unchanged. Existing
text properties on STR are preserved; only the first word gets the
additional PROPERTIES. The first word is the substring up to the first
space, tab, or newline."
  (if (or (string-empty-p str) (null properties))
      str
    (let* ((split-pos (or (string-match "[ \t\n]" str)
                          (length str)))
           (first (substring str 0 split-pos))
           (rest (substring str split-pos)))
      ;; Preserve existing properties on `first` (copied by `substring`)
      ;; and add/override with the provided PROPERTIES only for the first word.
      (add-text-properties 0 (length first) properties first)
      (concat first rest))))

(defun eca-chat--add-expandable-content (id label content)
  "Add LABEL to the chat current position for ID as a interactive text.
When expanded, shows CONTENT.
Applies LABEL-FACE to label and CONTENT-FACE to content."
  (save-excursion
    (let* ((context-start (eca-chat--prompt-area-start-point))
           (start-point (1- context-start)))
      (goto-char start-point)
      (unless (bolp) (eca-chat--insert "\n"))
      (let ((ov-label (make-overlay (point) (point) (current-buffer))))
        (overlay-put ov-label 'eca-chat--expandable-content-id id)
        (overlay-put ov-label 'eca-chat--expandable-content-toggle nil)
        (eca-chat--insert (propertize (eca-chat--propertize-only-first-word label
                                                                            'line-prefix (unless (string-empty-p content)
                                                                                           eca-chat-expandable-block-open-symbol))
                                      'keymap (let ((km (make-sparse-keymap)))
                                                (define-key km (kbd "<mouse-1>") (lambda () (eca-chat--expandable-content-toggle id)))
                                                (define-key km (kbd "<tab>") (lambda () (eca-chat--expandable-content-toggle id)))
                                                km)
                                      'help-echo "mouse-1 / tab / RET: expand/collapse"))
        (eca-chat--insert "\n")
        (let* ((start-point (point))
               (_ (eca-chat--insert "\n"))
               (ov-content (make-overlay start-point start-point (current-buffer) nil t)))
          (overlay-put ov-content 'eca-chat--expandable-content-content (propertize content 'line-prefix "   "))
          (overlay-put ov-label 'eca-chat--expandable-content-ov-content ov-content))))))

(defun eca-chat--update-expandable-content (id label content &optional append-content?)
  "Update to LABEL and CONTENT the expandable content of id ID."
  (when-let* ((ov-label (eca-chat--get-expandable-content id)))
    (let* ((ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content))
           (existing (overlay-get ov-content 'eca-chat--expandable-content-content))
           (delta (propertize content 'line-prefix "   "))
           (new-content (if append-content?
                            (concat existing delta)
                          delta))
           (open? (overlay-get ov-label 'eca-chat--expandable-content-toggle)))
      ;; Update stored content string
      (overlay-put ov-content 'eca-chat--expandable-content-content new-content)
      (save-excursion
        ;; Refresh the label line (cheap even when appending)
        (goto-char (overlay-start ov-label))
        (delete-region (point) (1- (overlay-start ov-content)))
        (eca-chat--insert (propertize (eca-chat--propertize-only-first-word label
                                                                            'line-prefix (unless (string-empty-p new-content)
                                                                                           (if open?
                                                                                               eca-chat-expandable-block-close-symbol
                                                                                             eca-chat-expandable-block-open-symbol)))
                                      'help-echo "mouse-1 / RET / tab: expand/collapse"))
        (when open?
          (if append-content?
              ;; Fast path: just append the delta to the visible content
              (progn
                (goto-char (overlay-end ov-content))
                (eca-chat--insert delta))
            ;; Replace the whole visible content
            (progn
              (delete-region (overlay-start ov-content) (overlay-end ov-content))
              (goto-char (overlay-start ov-content))
              (eca-chat--insert new-content))))))))

(defun eca-chat--expandable-content-toggle (id &optional force? close?)
  "Toggle the expandable-content of ID.
If FORCE? decide to CLOSE? or not."
  (when-let* ((ov-label (-first (-lambda (ov) (string= id (overlay-get ov 'eca-chat--expandable-content-id)))
                                (overlays-in (point-min) (point-max)))))
    (let* ((ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content))
           (content (overlay-get ov-content 'eca-chat--expandable-content-content))
           (empty-content? (string-empty-p content))
           (close? (if force?
                       close?
                     (overlay-get ov-label 'eca-chat--expandable-content-toggle))))
      (save-excursion
        (goto-char (overlay-start ov-label))
        (if (or close? empty-content?)
            (progn
              (put-text-property (point) (line-end-position)
                                 'line-prefix (unless empty-content?
                                                eca-chat-expandable-block-open-symbol))
              (goto-char (1+ (line-end-position)))
              (delete-region (overlay-start ov-content) (overlay-end ov-content))
              (overlay-put ov-label 'eca-chat--expandable-content-toggle nil))
          (progn
            (put-text-property (point) (line-end-position)
                               'line-prefix eca-chat-expandable-block-close-symbol)
            (goto-char (overlay-start ov-content))
            (eca-chat--insert content "\n")
            (overlay-put ov-label 'eca-chat--expandable-content-toggle t))))
      close?)))

(defun eca-chat--content-table (key-vals)
  "Return a string in table format for KEY-VALS."
  (-reduce-from
   (-lambda (a (k . v))
     (concat a "\n" (propertize (concat k ":") 'font-lock-face 'eca-chat--tool-call-table-key-face) " "
             (if (listp v)
                 (concat "\n"
                         (string-join (-map-indexed
                                       (lambda (i item)
                                         (if (cl-evenp i)
                                             (propertize (concat "  " (substring (symbol-name item) 1) ": ")
                                                         'font-lock-face 'eca-chat--tool-call-argument-key-face)
                                           (propertize (concat (prin1-to-string item) "\n")
                                                       'font-lock-face 'eca-chat--tool-call-argument-value-face)))
                                       v)
                                      ""))
               v)))
   ""
   key-vals))

(defun eca-chat--relativize-filename-for-workspace-root (filename roots &optional hide-filename?)
  "Relativize the FILENAME if a workspace root is found for ROOTS.
Show parent upwards if HIDE-FILENAME? is non nil."
  (let ((relative-path (or (-some->> (-first (lambda (root) (f-ancestor-of? root filename)) roots)
                             (f-relative filename))
                           filename)))
    (if hide-filename?
        (f-parent relative-path)
      relative-path)))

(defun eca-chat--file-change-diff (path diff roots)
  "Return a diff block for relative PATH from ROOTS with DIFF."
  (concat "\n"
          (if (f-exists? path)
              (eca-buttonize
               eca-chat-mode-map
               (propertize (eca-chat--relativize-filename-for-workspace-root path roots)
                           'font-lock-face 'eca-chat-file-path-face)
               (lambda () (find-file-other-window path)))
            path) "\n"
          "```diff\n" diff "\n```"))

(defun eca-chat--file-change-details-label (details)
  "Build the label from DETAILS for a file change block."
  (concat (propertize (f-filename (plist-get details :path)) 'font-lock-face 'eca-chat-file-change-label-face)
          " "
          (propertize (concat  "+" (number-to-string (plist-get details :linesAdded))) 'font-lock-face 'success)
          " "
          (propertize (concat  "-" (number-to-string (plist-get details :linesRemoved))) 'font-lock-face 'error)))

(defun eca-chat--context-presentable-path (filename)
  "Return the presentable string for FILENAME."
  (or (when (-first (lambda (root) (f-ancestor-of? root filename))
                    (eca--session-workspace-folders (eca-session)))
        (f-filename filename))
      filename))

(defun eca-chat--refresh-progress (chat-buffer)
  "Refresh the progress TEXT for CHAT-BUFFER."
  (when (buffer-live-p chat-buffer)
    (eca-chat--with-current-buffer chat-buffer
      (save-excursion
        (let ((ov (eca-chat--prompt-progress-field-ov)))
          (goto-char (overlay-start ov))
          (delete-region (point) (overlay-end ov)))
        (eca-chat--insert (propertize (if (string-empty-p eca-chat--progress-text)
                                          eca-chat-prompt-separator
                                        (concat eca-chat-prompt-separator "\n" eca-chat--progress-text))
                                      'font-lock-face 'eca-chat-system-messages-face)
                          eca-chat--spinner-string)))))

(defun eca-chat--context->str (context &optional static?)
  "Convert CONTEXT to a presentable str in buffer.
If STATIC? return strs with no dynamic values."
  (-let* (((&plist :type type) context)
          (context-str
           (pcase type
             ("file" (propertize (concat eca-chat-context-prefix
                                         (eca-chat--context-presentable-path (plist-get context :path))
                                         (-when-let ((&plist :start start :end end) (plist-get context :linesRange))
                                           (format "(%d-%d)" start end)))
                                 'eca-chat-expanded-item-str (concat eca-chat-context-prefix (plist-get context :path)
                                                                     (-when-let ((&plist :start start :end end) (plist-get context :linesRange))
                                                                       (format ":L%d-L%d" start end)))
                                 'font-lock-face 'eca-chat-context-file-face))
             ("directory" (propertize (concat eca-chat-context-prefix (eca-chat--context-presentable-path (plist-get context :path)))
                                      'eca-chat-expanded-item-str (concat eca-chat-context-prefix (plist-get context :path))
                                      'font-lock-face 'eca-chat-context-file-face))
             ("repoMap" (propertize (concat eca-chat-context-prefix "repoMap")
                                    'eca-chat-expanded-item-str (concat eca-chat-context-prefix "repoMap")
                                    'font-lock-face 'eca-chat-context-repo-map-face))
             ("mcpResource" (propertize (concat eca-chat-context-prefix (plist-get context :server) ":" (plist-get context :name))
                                        'eca-chat-expanded-item-str (concat eca-chat-context-prefix (plist-get context :server) ":" (plist-get context :name))
                                        'font-lock-face 'eca-chat-context-mcp-resource-face))
             ("cursor" (propertize (if static?
                                       (concat eca-chat-context-prefix "cursor")
                                     (concat eca-chat-context-prefix "cursor"
                                             "("
                                             (-some-> (plist-get eca-chat--cursor-context :path)
                                               (f-filename))
                                             " "
                                             (-some->>
                                                 (-> eca-chat--cursor-context
                                                     (plist-get :position)
                                                     (plist-get :start)
                                                     (plist-get :line))
                                               (funcall #'number-to-string))
                                             ":"
                                             (-some->>
                                                 (-> eca-chat--cursor-context
                                                     (plist-get :position)
                                                     (plist-get :start)
                                                     (plist-get :character))
                                               (funcall #'number-to-string))
                                             ")"))
                                   'eca-chat-expanded-item-str (concat eca-chat-context-prefix "cursor")
                                   'font-lock-face 'eca-chat-context-cursor-face))
             (_ (concat eca-chat-context-prefix "unkown:" type)))))
    (propertize context-str
                'eca-chat-item-type 'context
                'eca-chat-item-str-length (length context-str)
                'eca-chat-context-item context)))

(defun eca-chat--filepath->str (filepath lines-range)
  "Convert FILEPATH and LINES-RANGE to a presentable str in buffer."
  (let* ((item-str (concat eca-chat-filepath-prefix
                           (eca-chat--context-presentable-path filepath)
                           (-when-let ((&plist :start start :end end) lines-range)
                             (format "(%d-%d)" start end)))))
    (propertize item-str
                'eca-chat-item-type 'filepath
                'eca-chat-item-str-length (length item-str)
                'eca-chat-expanded-item-str (concat eca-chat-filepath-prefix
                                                    filepath
                                                    (-when-let ((&plist :start start :end end) lines-range)
                                                      (format ":L%d-L%d" start end)))
                'font-lock-face 'eca-chat-context-file-face)))

(defun eca-chat--refresh-context ()
  "Refresh chat context."
  (save-excursion
    (-some-> (eca-chat--prompt-context-field-ov)
      (overlay-start)
      (goto-char))
    (delete-region (point) (line-end-position))
    (seq-doseq (context eca-chat--context)
      (eca-chat--insert (eca-chat--context->str context))
      (eca-chat--insert " "))
    (eca-chat--insert (propertize eca-chat-context-prefix 'font-lock-face 'eca-chat-context-unlinked-face))))

(defconst eca-chat--kind->symbol
  '(("file" . file)
    ("directory" . folder)
    ("repoMap" . module)
    ("cursor" . class)
    ("mcpPrompt" . function)
    ("mcpResource" . file)
    ("native" . variable)))

(defun eca-chat--completion-item-kind (item)
  "Return the kind for ITEM."
  (alist-get (plist-get item :type)
             eca-chat--kind->symbol
             nil
             nil
             #'string=))

(defun eca-chat--completion-item-label-kind (item-label)
  "Return the kind for ITEM-LABEL."
  (eca-chat--completion-item-kind (get-text-property 0 'eca-chat-completion-item item-label)))

(defun eca-chat--completion-item-company-box-icon (item-label)
  "Return the kind for ITEM-LABEL."
  (let ((symbol (eca-chat--completion-item-label-kind item-label)))
    (intern (capitalize (symbol-name symbol)))))

(defun eca-chat--add-context (context)
  "Add to chat CONTEXT."
  (add-to-list 'eca-chat--context context t)
  (eca-chat--refresh-context))

(defun eca-chat--remove-context (context)
  "Remove from chat CONTEXT."
  (setq eca-chat--context (remove context eca-chat--context))
  (eca-chat--refresh-context))

(defun eca-chat--completion-context-annotate (roots item-label)
  "Annonate ITEM-LABEL detail for ROOTS."
  (-let (((&plist :type type :path path :description description) (get-text-property 0 'eca-chat-completion-item item-label)))
    (pcase type
      ("file" (eca-chat--relativize-filename-for-workspace-root path roots 'hide-filename))
      ("directory" (eca-chat--relativize-filename-for-workspace-root path roots 'hide-filename))
      ("repoMap" "Summary view of workspaces files")
      ("cursor" "Current cursor file + position")
      ("mcpResource" description)
      (_ ""))))

(defun eca-chat--completion-file-annotate (roots item-label)
  "Annonate ITEM-LABEL detail for ROOTS."
  (-let (((&plist :path path) (get-text-property 0 'eca-chat-completion-item item-label)))
    (eca-chat--relativize-filename-for-workspace-root path roots 'hide-filename)))

(defun eca-chat--completion-prompts-annotate (item-label)
  "Annotate prompt ITEM-LABEL."
  (-let (((&plist :description description :arguments args)
          (get-text-property 0 'eca-chat-completion-item item-label)))
    (concat "(" (string-join (--map (plist-get it :name) args) ", ")
            ") "
            (when description
              (truncate-string-to-width description (* 100 eca-chat-window-width))))))

(defun eca-chat--completion-context-from-new-context-exit-function (item _status)
  "Add to context the selected ITEM."
  (eca-chat--add-context (get-text-property 0 'eca-chat-completion-item item))
  (end-of-line))

(defun eca-chat--completion-context-from-prompt-exit-function (item _status)
  "Add to context the selected ITEM.
Add text property to prompt text to match context."
  (let ((context (get-text-property 0 'eca-chat-completion-item item)))
    (let ((start-pos (save-excursion
                       (search-backward eca-chat-context-prefix (line-beginning-position) t)))
          (end-pos (point)))
      (delete-region start-pos end-pos)
      (eca-chat--insert (eca-chat--context->str context 'static))))
  (eca-chat--insert " "))

(defun eca-chat--completion-file-from-prompt-exit-function (item _status)
  "Add to files the selected ITEM."
  (let* ((file (get-text-property 0 'eca-chat-completion-item item))
         (start-pos (save-excursion
                      (search-backward eca-chat-filepath-prefix (line-beginning-position) t)))
         (end-pos (point)))
    (delete-region start-pos end-pos)
    (eca-chat--insert (eca-chat--filepath->str (plist-get file :path) nil)))
  (eca-chat--insert " "))

(defun eca-chat--completion-prompt-exit-function (item _status)
  "Finish prompt completion for ITEM."
  (-let* (((&plist :arguments arguments) (get-text-property 0 'eca-chat-completion-item item)))
    (when (> (length arguments) 0)
      (seq-doseq (arg arguments)
        (-let (((&plist :name name :description description :required required) arg))
          (eca-chat--insert " ")
          (let ((arg-text (read-string (format "Arg: %s\nDescription: %s\nValue%s: "
                                               name
                                               description
                                               (if required "" " (leave blank for default)")))))
            (if (and arg-text (string-match-p " " arg-text))
                (eca-chat--insert (format "\"%s\"" arg-text))
              (eca-chat--insert arg-text)))))
      (end-of-line))))

(defun eca-chat--context-to-completion (context)
  "Convert CONTEXT to a completion item."
  (let ((raw-label (pcase (plist-get context :type)
                     ("file" (f-filename (plist-get context :path)))
                     ("directory" (f-filename (plist-get context :path)))
                     ("repoMap" "repoMap")
                     ("cursor" "cursor")
                     ("mcpResource" (concat (plist-get context :server) ":" (plist-get context :name)))
                     (_ (concat "Unknown - " (plist-get context :type)))))
        (face (pcase (plist-get context :type)
                ("file" 'eca-chat-context-file-face)
                ("directory" 'eca-chat-context-file-face)
                ("repoMap" 'eca-chat-context-repo-map-face)
                ("cursor" 'eca-chat-context-cursor-face)
                ("mcpResource" 'eca-chat-context-mcp-resource-face)
                (_ nil))))
    (propertize raw-label
                'eca-chat-completion-item context
                'face face)))

(defun eca-chat--file-to-completion (file)
  "Convert FILE to a completion item."
  (propertize (f-filename (plist-get file :path))
              'eca-chat-completion-item file
              'face 'eca-chat-context-file-face))

(defun eca-chat--command-to-completion (command)
  "Convert COMMAND to a completion item."
  (propertize (plist-get command :name)
              'eca-chat-completion-item command))

(defun eca-chat--go-to-overlay (ov-key range-min range-max first?)
  "Go to overlay finding from RANGE-MIN to RANGE-MAX if matches OV-KEY."
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (let ((get-fn (if first? #'-first #'-last)))
      (when-let ((ov (funcall get-fn (-lambda (ov) (overlay-get ov ov-key))
                              (overlays-in range-min range-max))))
        (goto-char (overlay-start ov))))))

(defun eca-chat--cur-position ()
  "Return the start and end positions for current point.
Returns a cons cell (START . END) where START and END are cons cells
of (LINE . CHARACTER) representing the current selection or cursor position."
  (save-excursion
    (let* ((start-pos (if (use-region-p) (region-beginning) (point)))
           (end-pos (if (use-region-p) (region-end) (point)))
           (start-line (line-number-at-pos start-pos))
           (start-char (1+ (progn
                             (goto-char start-pos)
                             (current-column))))
           (end-line (line-number-at-pos end-pos))
           (end-char (1+ (progn
                           (goto-char end-pos)
                           (current-column)))))
      (cons (cons start-line start-char)
            (cons end-line end-char)))))

(defun eca-chat--get-last-visited-buffer ()
  "Return the last visited buffer which has a filename."
  (-first (lambda (buff)
            (when (buffer-live-p buff)
              (with-current-buffer buff
                (buffer-file-name))))
          (buffer-list)))

(defun eca-chat--track-cursor (&rest _args)
  "Change chat context considering current open file and point."
  (when-let ((session (eca-session)))
    (when-let ((workspaces (eca--session-workspace-folders session)))
      (when-let ((buffer (eca-chat--get-last-visited-buffer)))
        (when-let ((path (buffer-file-name buffer)))
          (when (--any? (and it (f-ancestor-of? it path))
                        workspaces)
            (with-current-buffer buffer
              (when-let (chat-buffer (eca-chat--get-last-buffer session))
                (when (buffer-live-p chat-buffer)
                  (-let* (((start . end) (eca-chat--cur-position))
                          ((start-line . start-character) start)
                          ((end-line . end-character) end))
                    (eca-chat--with-current-buffer chat-buffer
                      (let ((new-context (list :path path
                                               :position (list :start (list :line start-line :character start-character)
                                                               :end (list :line end-line :character end-character)))))
                        (when (not (eca-plist-equal eca-chat--cursor-context new-context))
                          (setq eca-chat--cursor-context new-context)
                          (eca-chat--refresh-context))))))))))))))

(defun eca-chat--track-cursor-position-schedule ()
  "Debounce `eca-chat--track-cursor' via an idle timer."
  (unless eca-chat--cursor-context-timer
    (setq eca-chat--cursor-context-timer
          (run-with-idle-timer eca-chat-cursor-context-debounce t
                               #'eca-chat--track-cursor))))

(defun eca-chat--parse-unified-diff (diff-text)
  "Compatibility wrapper that delegates to `eca-diff-parse-unified-diff'.

DIFF-TEXT is the unified diff string to parse and returns the parsed
plist produced by `eca-diff-parse-unified-diff'."
  (eca-diff-parse-unified-diff diff-text))

(defun eca-chat--show-diff-ediff (path diff)
  "Compatibility wrapper delegating to `eca-diff-show-ediff'.

PATH is the file path being shown and DIFF is the unified diff text.
This wrapper passes the current chat-buffer as CHAT-BUF so `eca-diff' can
restore the chat display after Ediff quits."
  (eca-diff-show-ediff path diff (current-buffer) (lambda (b) (ignore-errors (eca-chat--display-buffer b)))))


(defun eca-chat--show-diff-smerge (path diff)
  "Compatibility wrapper delegating to `eca-diff-show-smerge'.

PATH is the file path being shown and DIFF is the unified diff text.
This wrapper passes the current chat-buffer as CHAT-BUF so `eca-diff' can
restore the chat display after smerge quits."
  (eca-diff-show-smerge path diff (current-buffer) (lambda (b) (ignore-errors (eca-chat--display-buffer b)))))


(defun eca-chat--show-diff (path diff)
  "Dispatch DIFF view based on `eca-chat-diff-tool` for PATH."
  (pcase eca-chat-diff-tool
    ('ediff (eca-chat--show-diff-ediff path diff))
    ('smerge (eca-chat--show-diff-smerge path diff))))

(defun eca-chat--find-typed-query (prefix)
  "Return the text typed after the last item after PREFIX (@ or #).
For example: `@foo @bar @baz` => `baz`. If nothing is typed, returns an empty
string."
  (when (eca-chat--point-at-new-context-p)
    (save-excursion
      (goto-char (eca-chat--new-context-start-point))
      (end-of-line)))
  (save-excursion
    (let* ((start (line-beginning-position))
           (end (point))
           (last-prefix-pos (search-backward prefix start t)))
      (if last-prefix-pos
          (string-trim (buffer-substring-no-properties (+ last-prefix-pos (length prefix)) end))
        ""))))

(declare-function dired-get-marked-files "dired")
(declare-function treemacs-node-at-point "treemacs")
(declare-function treemacs-button-get "treemacs")

(defun eca-chat--get-contexts-dwim ()
  "Get contexts in a DWIM manner."
  (cond
   ((and (buffer-file-name)
         (use-region-p))
    (-let (((start . end) `(,(line-number-at-pos (region-beginning)) . ,(line-number-at-pos (region-end)))))
      (list
       (list :type "file"
             :path (buffer-file-name)
             :linesRange (list :start start :end end)))))

   ((derived-mode-p 'dired-mode)
    (--map (list :type (if (f-dir? it) "directory" "file")
                 :path it)
           (dired-get-marked-files)))

   ((derived-mode-p 'treemacs-mode)
    (when-let (path (-some-> (treemacs-node-at-point)
                      (treemacs-button-get :path)))
      (list
       (list :type (if (f-dir? path) "directory" "file")
             :path path))))

   ((buffer-file-name)
    (list
     (list :type "file" :path (buffer-file-name))))))

(defun eca-chat--insert-prompt (text)
  "Insert TEXT to latest chat prompt point."
  (save-excursion
    (goto-char (eca-chat--prompt-field-start-point))
    (goto-char (line-end-position))
    (when (= (line-beginning-position) (line-end-position))
      (eca-chat--insert " "))
    (eca-chat--insert text)))

;; Public

(define-derived-mode eca-chat-mode gfm-view-mode  "eca-chat"
  "Major mode for ECA chat sessions.
\\{eca-chat-mode-map}"
  :group 'eca
  (visual-line-mode)
  (hl-line-mode -1)
  (read-only-mode -1)
  (setq-local eca-chat--history '())
  (setq-local eca-chat--history-index -1)

  ;; Show diff blocks in markdown-mode with colors.
  (setq-local markdown-fontify-code-blocks-natively t)

  (make-local-variable 'completion-at-point-functions)
  (setq-local completion-at-point-functions (list #'eca-chat-completion-at-point))

  (make-local-variable 'company-box-icons-functions)
  (when (featurep 'company-box)
    (add-to-list 'company-box-icons-functions #'eca-chat--completion-item-company-box-icon))

  (let ((session (eca-session)))
    (unless (listp header-line-format)
      (setq-local header-line-format (list header-line-format)))
    (add-to-list 'header-line-format `(t (:eval (eca-chat--header-line-string (eca-session)))))

    (when (eq 0 (length (string-trim (buffer-string))))
      (save-excursion
        (goto-char (point-min))
        (eca-chat--insert "\n")
        (eca-chat--insert (propertize (eca--session-chat-welcome-message session)
                                      'font-lock-face 'eca-chat-welcome-face))
        (eca-chat--insert-prompt-string)))

    ;; TODO is there a better way to do that?
    (advice-add 'delete-char :around #'eca-chat--key-pressed-deletion)
    (advice-add 'backward-kill-word :around #'eca-chat--key-pressed-deletion)
    (when (featurep 'evil)
      (advice-add 'evil-delete-backward-word :around #'eca-chat--key-pressed-deletion)
      (advice-add 'evil-delete-back-to-indentation :around #'eca-chat--key-pressed-deletion)
      (advice-add 'evil-delete-whole-line :around #'eca-chat--key-pressed-deletion)
      (advice-add 'evil-delete-line :around #'eca-chat--key-pressed-deletion)
      (advice-add 'evil-delete-char :around #'eca-chat--key-pressed-deletion)
      (advice-add 'evil-delete :around #'eca-chat--key-pressed-deletion)
      (advice-add 'evil-delete-backward-char :around #'eca-chat--key-pressed-deletion))

    (add-hook 'eldoc-documentation-functions #'eca-chat-eldoc-function nil t)
    (eldoc-mode 1)

    (let ((chat-buffer (current-buffer)))
      (run-with-timer
       0.05
       nil
       (lambda ()
         (eca-chat--with-current-buffer chat-buffer
           (display-line-numbers-mode -1)
           (when (fboundp 'vi-tilde-fringe-mode) (vi-tilde-fringe-mode -1))
           (when (fboundp 'company-mode)
             (setq-local company-backends '(company-capf)
                         company-minimum-prefix-length 0))
           (when (fboundp 'corfu-mode)
             (setq-local corfu-auto-prefix 0))
           (setq-local mode-line-format '(t (:eval (eca-chat--mode-line-string))))
           (force-mode-line-update)
           (run-hooks 'eca-chat-mode-hook))))))

  (face-remap-add-relative 'markdown-line-break-face
                           '(:underline nil))

  (goto-char (point-max)))

(defun eca-chat-eldoc-function (cb &rest _ignored)
  "Eldoc function to show details of context and prompt in eldoc.
Calls CB with the resulting message."
  (when-let ((item-type (get-text-property (point) 'eca-chat-item-type)))
    (when-let ((item-str (get-text-property (point) 'eca-chat-expanded-item-str)))
      (when-let ((face (get-text-property (point) 'font-lock-face)))
        (funcall cb (format "%s: %s"
                            (pcase item-type
                              ('context "Context")
                              ('filepath "Filepath"))
                            (propertize item-str 'face face)))))))

(defun eca-chat-completion-at-point ()
  "Complete at point in the chat."
  (let* ((full-text (buffer-substring-no-properties (line-beginning-position) (point)))
         (type (cond
                ;; completing contexts
                ((eca-chat--point-at-new-context-p)
                 'contexts-from-new-context)

                ((when-let (last-word (car (last (string-split full-text "[\s]"))))
                   (string-match-p (concat "^" eca-chat-context-prefix) last-word))
                 'contexts-from-prompt)

                ((when-let (last-word (car (last (string-split full-text "[\s]"))))
                   (string-match-p (concat "^" eca-chat-filepath-prefix) last-word))
                 'files-from-prompt)

                ;; completing commands with `/`
                ((and (eca-chat--point-at-prompt-field-p)
                      (string-prefix-p "/" full-text))
                 'prompts)

                (t nil)))
         (bounds-start (pcase type
                         ('prompts (1+ (line-beginning-position)))
                         (_ (or
                             (cl-first (bounds-of-thing-at-point 'symbol))
                             (point)))))
         (candidates-fn (lambda ()
                          (eca-api-catch 'input
                              (eca-api-while-no-input
                                (pcase type
                                  ((or 'contexts-from-prompt
                                       'contexts-from-new-context)
                                   (let ((query (eca-chat--find-typed-query eca-chat-context-prefix)))
                                     (or (gethash query eca-chat--context-completion-cache)
                                         (-let* (((&plist :contexts contexts) (eca-api-request-while-no-input
                                                                               (eca-session)
                                                                               :method "chat/queryContext"
                                                                               :params (list :chatId eca-chat--id
                                                                                             :query query
                                                                                             :contexts (vconcat eca-chat--context))))
                                                 (items (-map #'eca-chat--context-to-completion contexts)))
                                           (clrhash eca-chat--context-completion-cache)
                                           (puthash query items eca-chat--context-completion-cache)
                                           items))))

                                  ('files-from-prompt
                                   (let ((query (eca-chat--find-typed-query eca-chat-filepath-prefix)))
                                     (or (gethash query eca-chat--file-completion-cache)
                                         (-let* (((&plist :files files) (eca-api-request-while-no-input
                                                                         (eca-session)
                                                                         :method "chat/queryFiles"
                                                                         :params (list :chatId eca-chat--id
                                                                                       :query query)))
                                                 (items (-map #'eca-chat--file-to-completion files)))
                                           (clrhash eca-chat--file-completion-cache)
                                           (puthash query items eca-chat--file-completion-cache)
                                           items))))

                                  ('prompts
                                   (let ((query (substring full-text 1)))
                                     (or (gethash query eca-chat--command-completion-cache)
                                         (-let* (((&plist :commands commands) (eca-api-request-while-no-input
                                                                               (eca-session)
                                                                               :method "chat/queryCommands"
                                                                               :params (list :chatId eca-chat--id
                                                                                             :query query)))
                                                 (items (-map #'eca-chat--command-to-completion commands)))
                                           (clrhash eca-chat--command-completion-cache)
                                           (puthash query items eca-chat--command-completion-cache)
                                           items))))

                                  (_ nil)))
                            (:interrupted nil)
                            (`,res res))))
         (exit-fn (pcase type
                    ('contexts-from-new-context #'eca-chat--completion-context-from-new-context-exit-function)
                    ('contexts-from-prompt #'eca-chat--completion-context-from-prompt-exit-function)
                    ('files-from-prompt #'eca-chat--completion-file-from-prompt-exit-function)
                    ('prompts #'eca-chat--completion-prompt-exit-function)
                    (_ nil)))
         (annotation-fn (pcase type
                          ((or 'contexts-from-prompt
                               'contexts-from-new-context) (-partial #'eca-chat--completion-context-annotate (eca--session-workspace-folders (eca-session))))
                          ('files-from-prompt (-partial #'eca-chat--completion-file-annotate (eca--session-workspace-folders (eca-session))))
                          ('prompts #'eca-chat--completion-prompts-annotate))))
    (list
     bounds-start
     (point)
     (lambda (probe pred action)
       (cond
        ((eq action 'metadata)
         '(metadata (category . eca-capf)
           (display-sort-function . identity)
           (cycle-sort-function . identity)))
        ((eq (car-safe action) 'boundaries) nil)
        (t
         (complete-with-action action (funcall candidates-fn) probe pred))))
     :company-kind #'eca-chat--completion-item-label-kind
     :company-require-match 'never
     :annotation-function annotation-fn
     :exit-function exit-fn)))

(defun eca-chat-config-updated (session chat-config)
  "Update chat based on the CHAT-CONFIG for SESSION."
  (-some->> (plist-get chat-config :welcomeMessage)
    (setf (eca--session-chat-welcome-message session)))
  (-some->> (plist-get chat-config :models)
    (setf (eca--session-models session)))
  (-some->> (plist-get chat-config :behaviors)
    (setf (eca--session-chat-behaviors session)))
  (-some->> (plist-get chat-config :selectModel)
    (setf (eca--session-chat-selected-model session)))
  (-some->> (plist-get chat-config :selectBehavior)
    (setf (eca--session-chat-selected-behavior session))))

(defun eca-chat--tool-call-file-change-details
  (content approval-text time status tool-call-next-line-spacing roots)
  "Update tool call UI based showing file change details.
CONTENT is the tool call content.
Can include optional APPROVAL-TEXT and TIME.
Append STATUS, TOOL-CALL-NEXT-LINE-SPACING and ROOTS"
  (-let* (((&plist :name name :details details :id id :summary summary) content)
          (path (plist-get details :path))
          (diff (plist-get details :diff))
          (view-diff-btn
           (eca-buttonize
            eca-chat-mode-map
            (propertize "view diff" 'font-lock-face 'eca-chat-diff-view-face)
            (lambda ()
              (interactive)
              (eca-chat--show-diff path diff)))))
    (eca-chat--update-expandable-content
     id
     (concat (propertize summary 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
             " " (eca-chat--file-change-details-label details)
             " " status time
             "\n"
             (propertize view-diff-btn 'line-prefix tool-call-next-line-spacing)
             approval-text)
     (concat "Tool: `" name "`\n"
             (eca-chat--file-change-diff path diff roots)))))

(defun eca-chat--tool-call-json-outputs-details (content time status)
  "Update tool call UI for json output given CONTENT, TIME and STATUS."
  (-let* (((&plist :name name :arguments arguments :server server :details details :id
                   id :summary summary) content)
          (jsons (plist-get details :jsons)))
    (eca-chat--update-expandable-content
     id
     (concat (propertize summary 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
             " " status time)
     (eca-chat--content-table
      `(("Tool"   . ,name)
        ("Server" . ,server)
        ("Arguments" . ,arguments)
        ("Json output" . ,(concat "\n"
                                  "```javascript\n"
                                  (string-join jsons "\n")
                                  "\n```")))))))

(defun eca-chat-content-received (session params)
  "Handle the content received notification with PARAMS for SESSION."
  (let* ((chat-id (plist-get params :chatId))
         (role (plist-get params :role))
         (content (plist-get params :content))
         (roots (eca--session-workspace-folders session))
         (tool-call-next-line-spacing (make-string (1+ (length eca-chat-expandable-block-open-symbol)) ?\s))
         (chat-buffer (eca-chat--get-chat-buffer session chat-id)))
    (eca-chat--with-current-buffer chat-buffer
      (setq-local eca-chat--empty nil)
      (pcase (plist-get content :type)
        ("metadata"
         (setq-local eca-chat--title (plist-get content :title)))
        ("text"
         (when-let* ((text (plist-get content :text)))
           (pcase role
             ("user"
              (eca-chat--add-text-content
               (propertize text
                           'font-lock-face 'eca-chat-user-messages-face
                           'line-prefix (propertize eca-chat-prompt-prefix
                                                    'font-lock-face 'eca-chat-user-messages-face)
                           'line-spacing 10)
               'eca-chat--user-message-id eca-chat--last-request-id)
              (eca-chat--mark-header)
              (font-lock-ensure))
             ("system"
              (eca-chat--add-text-content
               (propertize text
                           'font-lock-face 'eca-chat-system-messages-face
                           'line-height 20)))
             (_
              (eca-chat--add-text-content text)))))
        ("url"
         (eca-chat--add-header
          (concat "🌐 "
                  (eca-buttonize
                   eca-chat-mode-map
                   (plist-get content :title)
                   (lambda () (browse-url (plist-get content :url))))
                  "\n\n")))
        ("reasonStarted"
         (let ((id (plist-get content :id))
               (label (propertize "Thinking..." 'font-lock-face 'eca-chat-reason-label-face)))
           (eca-chat--add-expandable-content id label "")))
        ("reasonText"
         (let ((id (plist-get content :id))
               (label (propertize "Thinking..." 'font-lock-face 'eca-chat-reason-label-face))
               (text (plist-get content :text)))
           (eca-chat--update-expandable-content id label text t)))
        ("reasonFinished"
         (let* ((id (plist-get content :id))
                (base (propertize "Thought" 'font-lock-face 'eca-chat-reason-label-face))
                (time (when-let ((ms (plist-get content :totalTimeMs)))
                        (concat " " (eca-chat--time->presentable-time ms))))
                (label (concat base time)))
           (eca-chat--update-expandable-content id label "" t)))
        ("hookActionStarted"
         (let* ((id (plist-get content :id))
                (name (plist-get content :name))
                (label (propertize (format "Running hook '%s'..." name) 'font-lock-face 'eca-chat-hook-label-face)))
           (eca-chat--add-expandable-content id label "")))
        ("hookActionFinished"
         (let* ((id (plist-get content :id))
                (name (plist-get content :name))
                (status (number-to-string (plist-get content :status)))
                (output (plist-get content :output))
                (error (plist-get content :error))
                (label (propertize (format "Executed hook '%s'" name) 'font-lock-face 'eca-chat-hook-label-face)))
           (eca-chat--update-expandable-content id label (eca-chat--content-table
                                                          `(("Name" . ,name)
                                                            ("Status" . ,status)
                                                            ("Output" . ,output)
                                                            ("Error" . ,error))))))
        ("toolCallPrepare"
         (let* ((id (plist-get content :id))
                (name (plist-get content :name))
                (server (plist-get content :server))
                (argsText (plist-get content :argumentsText))
                (label (or (plist-get content :summary)
                             (format "Preparing tool: %s" name)))
                (current-count (gethash id eca-chat--tool-call-prepare-counters 0))
                (cached-content (gethash id eca-chat--tool-call-prepare-content-cache ""))
                (new-content (concat cached-content argsText))
                (should-update-ui-p
                 (pcase eca-chat-tool-call-prepare-throttle
                   ('all t)
                   ('smart (or (= current-count 0)
                               (= (mod current-count eca-chat-tool-call-prepare-update-interval) 0))))))
           ;; Always cache the metadata and content
           (puthash id (1+ current-count) eca-chat--tool-call-prepare-counters)
           (puthash id new-content eca-chat--tool-call-prepare-content-cache)
           ;; Only update UI when throttling permits
           (when should-update-ui-p
             (let ((label (concat (propertize label 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                                  " " eca-chat-mcp-tool-call-loading-symbol)))
               (if (eca-chat--get-expandable-content id)
                   ;; Update with accumulated content, not just this chunk
                   (eca-chat--update-expandable-content
                    id label new-content nil) ; nil = replace, not append
                 (eca-chat--add-expandable-content
                  id label
                  (eca-chat--content-table
                   `(("Tool" . ,name)
                     ("Server" . ,server)
                     ("Arguments" . ,new-content)))))))))
        ("toolCallRun"
         (let* ((id (plist-get content :id))
                (args (plist-get content :arguments))
                (name (plist-get content :name))
                (server (plist-get content :server))
                (label (or (plist-get content :summary)
                             (format "Calling tool: %s__%s" server name)))
                (manual? (plist-get content :manualApproval))
                (status eca-chat-mcp-tool-call-loading-symbol)
                (approval-text (when manual?
                                 (eca-chat--build-tool-call-approval-str-content session id tool-call-next-line-spacing)))
                (details (plist-get content :details)))
           (pcase (plist-get details :type)
             ("fileChange" (eca-chat--tool-call-file-change-details content approval-text nil status tool-call-next-line-spacing roots))
             (_ (eca-chat--update-expandable-content
                 id
                 (concat (propertize label 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                         " " status
                         "\n"
                         approval-text)
                 (eca-chat--content-table
                  `(("Tool" . ,name)
                    ("Server" . ,server)
                    ("Arguments" . ,args))))))
           (when (and eca-chat-expand-pending-approval-tools manual?)
             (eca-chat--expandable-content-toggle id t nil))))
        ("toolCallRunning"
         (let* ((id (plist-get content :id))
                (args (plist-get content :arguments))
                (name (plist-get content :name))
                (server (plist-get content :server))
                (label (or (plist-get content :summary)
                             (format "Running tool: %s__%s" server name)))
                (details (plist-get content :details))
                (status eca-chat-mcp-tool-call-loading-symbol))
           (pcase (plist-get details :type)
             ("fileChange" (eca-chat--tool-call-file-change-details content nil nil status tool-call-next-line-spacing roots))
             (_ (eca-chat--update-expandable-content
                 id
                 (concat (propertize label 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                         " " status)
                 (eca-chat--content-table
                  `(("Tool" . ,name)
                    ("Server" . ,server)
                    ("Arguments" . ,args))))))))
        ("toolCalled"
         (let* ((id (plist-get content :id))
                (name (plist-get content :name))
                (server (plist-get content :server))
                (label (or (plist-get content :summary)
                             (format "Called tool: %s__%s" server name)))
                (args (plist-get content :arguments))
                (outputs (plist-get content :outputs))
                (output-text (if outputs
                                 (mapconcat (lambda (o) (or (plist-get o :text) "")) outputs "\n")
                               ""))
                (details (plist-get content :details))
                (time (when-let ((ms (plist-get content :totalTimeMs)))
                        (concat " " (eca-chat--time->presentable-time ms))))
                (status (if (plist-get content :error)
                            eca-chat-mcp-tool-call-error-symbol
                          eca-chat-mcp-tool-call-success-symbol)))
           ;; Cleanup counters for this tool-call id to avoid unbounded growth
           (remhash id eca-chat--tool-call-prepare-counters)
           (remhash id eca-chat--tool-call-prepare-content-cache)
           (pcase (plist-get details :type)
             ("fileChange" (eca-chat--tool-call-file-change-details content nil time status tool-call-next-line-spacing roots))
             ("jsonOutputs" (eca-chat--tool-call-json-outputs-details content time status))
             (_ (eca-chat--update-expandable-content
                 id
                 (concat (propertize label 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                         " " status time)
                 (eca-chat--content-table
                  `(("Tool"   . ,name)
                    ("Server" . ,server)
                    ("Arguments" . ,args)
                    ("Output" . ,output-text))))))
           (when eca-chat-shrink-called-tools
             (eca-chat--expandable-content-toggle id t t))))
        ("toolCallRejected"
         (let* ((name (plist-get content :name))
                (server (plist-get content :server))
                (args (plist-get content :arguments))
                (details (plist-get content :details))
                (status eca-chat-mcp-tool-call-error-symbol)
                (id (plist-get content :id)))
           ;; Cleanup counters for this tool-call id
           (remhash id eca-chat--tool-call-prepare-counters)
           (remhash id eca-chat--tool-call-prepare-content-cache)
           (pcase (plist-get details :type)
             ("fileChange" (eca-chat--tool-call-file-change-details content nil nil status tool-call-next-line-spacing roots))
             (_ (eca-chat--update-expandable-content
                 id
                 (concat (propertize (format "Rejected tool: %s__%s" server name)
                                     'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                         " "
                         eca-chat-mcp-tool-call-error-symbol)
                 (eca-chat--content-table `(("Tool" . ,name)
                                            ("Server" . ,server)
                                            ("Arguments" . ,args))))))))
        ("progress"
         (pcase (plist-get content :state)
           ("running"
            (setq-local eca-chat--progress-text (plist-get content :text))
            (unless eca-chat--spinner-timer
              (eca-chat--spinner-start
               (lambda ()
                 (eca-chat--refresh-progress chat-buffer))))
            (eca-chat--refresh-progress chat-buffer))
           ("finished"
            (setq-local eca-chat--progress-text "")
            (eca-chat--spinner-stop)
            (eca-chat--add-text-content "\n")
            (eca-chat--set-chat-loading session nil)
            (eca-chat--refresh-progress chat-buffer))))
        ("usage"
         (setq-local eca-chat--message-input-tokens  (plist-get content :messageInputTokens))
         (setq-local eca-chat--message-output-tokens (plist-get content :messageOutputTokens))
         (setq-local eca-chat--session-tokens        (plist-get content :sessionTokens))
         (setq-local eca-chat--session-limit-context (plist-get (plist-get content :limit) :context))
         (setq-local eca-chat--session-limit-output  (plist-get (plist-get content :limit) :output))
         (setq-local eca-chat--message-cost          (plist-get content :messageCost))
         (setq-local eca-chat--session-cost          (plist-get content :sessionCost)))
        (_ nil)))))

(defun eca-chat--handle-mcp-server-updated (session _server)
  "Handle mcp SERVER updated for SESSION."
  ;; TODO do for all chats
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
    (force-mode-line-update)))

(defun eca-chat-open (session)
  "Open or create dedicated eca chat window for SESSION."
  (eca-assert-session-running session)
  (unless (buffer-live-p (eca-chat--get-last-buffer session))
    (eca-chat--create-buffer session))
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
    (unless (derived-mode-p 'eca-chat-mode)
      (eca-chat-mode)
      (eca-chat--track-cursor-position-schedule)
      (when eca-chat-auto-add-cursor
        (eca-chat--add-context (list :type "cursor")))
      (when eca-chat-auto-add-repomap
        (eca-chat--add-context (list :type "repoMap"))))
    (unless (eq (current-buffer) (eca-get (eca--session-chats session) 'empty))
      (setf (eca--session-chats session) (eca-assoc (eca--session-chats session) 'empty (current-buffer))))
    (if (window-live-p (get-buffer-window (buffer-name)))
        (eca-chat--select-window)
      (eca-chat--pop-window))
    (unless (eca--session-last-chat-buffer session)
      (setf (eca--session-last-chat-buffer session) (current-buffer))))
  (eca-chat--track-cursor))

(defun eca-chat-exit (session)
  "Exit the ECA chat for SESSION."
  (when (buffer-live-p (eca-chat--get-last-buffer session))
    (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
      (setq eca-chat--closed t)
      (force-mode-line-update)
      (goto-char (point-max))
      (rename-buffer (concat (buffer-name) ":closed") t)
      ;; Keep only the most recently closed chat buffer; kill older ones.
      (let ((current (current-buffer)))
        (dolist (b (buffer-list))
          (when (and (not (eq b current))
                     (string-match-p "^<eca-chat:.*>:closed$" (buffer-name b)))
            (kill-buffer b))))
      (when-let* ((window (get-buffer-window (eca-chat--get-last-buffer session))))
        (quit-window nil window)))))

;;;###autoload
(defun eca-chat-clear ()
  "Clear the eca chat."
  (interactive)
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (eca-chat--clear)))

;;;###autoload
(defun eca-chat-select-model ()
  "Select which model to use in the chat from what server supports."
  (interactive)
  (eca-assert-session-running (eca-session))
  (when eca-chat-custom-model
    (error (eca-error "The eca-chat-custom-model variable is already set: %s" eca-chat-custom-model)))
  (when-let* ((model (completing-read "Select a model:" (append (eca--session-models (eca-session)) nil) nil t)))
    (setf (eca--session-chat-selected-model (eca-session)) model)))

;;;###autoload
(defun eca-chat-tool-call-accept-all ()
  "Accept all pending approval tool call in chat."
  (interactive)
  (eca-assert-session-running (eca-session))
  (save-excursion
    (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
      (goto-char (point-min))
      (when (text-property-search-forward 'eca-tool-call-pending-approval-accept t t)
        (call-interactively #'eca-chat--key-pressed-return)))))

;;;###autoload
(defun eca-chat-tool-call-accept-all-and-remember ()
  "Accept all pending approval tool call in chat and remember for session."
  (interactive)
  (eca-assert-session-running (eca-session))
  (save-excursion
    (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
      (goto-char (point-min))
      (when (text-property-search-forward 'eca-tool-call-pending-approval-accept-and-remember t t)
        (call-interactively #'eca-chat--key-pressed-return)))))

;;;###autoload
(defun eca-chat-tool-call-accept-next ()
  "Search the next pending approval tool call after cursor and approve it."
  (interactive)
  (eca-assert-session-running (eca-session))
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (save-excursion
      (when (text-property-search-forward 'eca-tool-call-pending-approval-accept t t)
        (call-interactively #'eca-chat--key-pressed-return)))))

;;;###autoload
(defun eca-chat-tool-call-reject-next ()
  "Search the next pending approval tool call after cursor and reject it."
  (interactive)
  (eca-assert-session-running (eca-session))
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (save-excursion
      (when (text-property-search-forward 'eca-tool-call-pending-approval-reject t)
        (call-interactively #'eca-chat--key-pressed-return)))))

;;;###autoload
(defun eca-chat-select-behavior ()
  "Select which chat behavior to use from what server supports."
  (interactive)
  (eca-assert-session-running (eca-session))
  (when-let* ((behavior (completing-read "Select a behavior:" (append (eca--session-chat-behaviors (eca-session)) nil) nil t)))
    (setf (eca--session-chat-selected-behavior (eca-session)) behavior)
    (eca-api-notify (eca-session)
                    :method "chat/selectedBehaviorChanged"
                    :params (list :behavior behavior))))

;;;###autoload
(defun eca-chat-reset ()
  "Request a chat reset."
  (interactive)
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (when eca-chat--id
      (eca-api-request-sync (eca-session)
                            :method "chat/delete"
                            :params (list :chatId eca-chat--id))
      (setq-local eca-chat--message-input-tokens nil)
      (setq-local eca-chat--message-output-tokens nil)
      (setq-local eca-chat--session-tokens nil)
      (setq-local eca-chat--message-cost nil)
      (setq-local eca-chat--session-cost nil)
      (setq-local eca-chat--empty t)
      (setq-local eca-chat--title nil)
      (clrhash eca-chat--context-completion-cache)
      (clrhash eca-chat--file-completion-cache)
      (setq-local eca-chat--custom-title nil)
      ;; Reset per-buffer tool prepare counters to avoid leaking across sessions
      (setq-local eca-chat--tool-call-prepare-counters (make-hash-table :test 'equal))
      (setq-local eca-chat--tool-call-prepare-content-cache (make-hash-table :test 'equal))
      (eca-chat--clear))))

;;;###autoload
(defun eca-chat-go-to-prev-user-message ()
  "Go to the previous user message from point."
  (interactive)
  (eca-assert-session-running (eca-session))
  (eca-chat--go-to-overlay 'eca-chat--user-message-id (point-min) (point) nil))

;;;###autoload
(defun eca-chat-go-to-next-user-message ()
  "Go to the next user message from point.
If there is no next user message, go to the chat prompt line."
  (interactive)
  (eca-assert-session-running (eca-session))
  (unless (eca-chat--go-to-overlay 'eca-chat--user-message-id (1+ (point)) (point-max) t)
    (goto-char (eca-chat--prompt-field-start-point))))

;;;###autoload
(defun eca-chat-go-to-prev-expandable-block ()
  "Go to the previous expandable block from point."
  (interactive)
  (eca-assert-session-running (eca-session))
  (eca-chat--go-to-overlay 'eca-chat--expandable-content-id (point-min) (point) nil))

;;;###autoload
(defun eca-chat-go-to-next-expandable-block ()
  "Go to the next expandable block from point."
  (interactive)
  (eca-assert-session-running (eca-session))
  (eca-chat--go-to-overlay 'eca-chat--expandable-content-id (1+ (point)) (point-max) t))

;;;###autoload
(defun eca-chat-toggle-expandable-block (&optional force-open?)
  "Toggle current expandable block at point.
Just open if FORCE-OPEN? is non-nil."
  (interactive)
  (eca-assert-session-running (eca-session))
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (unless (eca-chat--expandable-content-at-point)
      (eca-chat-go-to-prev-expandable-block))
    (when-let ((ov (eca-chat--expandable-content-at-point)))
      (eca-chat--expandable-content-toggle (overlay-get ov 'eca-chat--expandable-content-id) (when force-open? t) (not force-open?)))))

;;;###autoload
(defun eca-chat-add-context-to-system-prompt ()
  "Add context to system prompt in chat in a DWIM manner.

- If a region selected, add file with lines range selected.
- If in Dired, add the marked files/dirs or current file/dir at point.
- If in Treemacs, add selected file/dir.
- Else add current file."
  (interactive)
  (eca-assert-session-running (eca-session))
  (let* ((contexts (eca-chat--get-contexts-dwim)))
    (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
      (seq-doseq (context contexts)
        (eca-chat--add-context context)))))

;;;###autoload
(defun eca-chat-add-context-to-user-prompt ()
  "Add context to user prompt in chat in a DWIM manner.

- If a region selected, add file with lines range selected.
- If in Dired, add the marked files/dirs or current file/dir at point.
- If in Treemacs, add selected file/dir.
- Else add current file."
  (interactive)
  (eca-assert-session-running (eca-session))
  (let* ((contexts (eca-chat--get-contexts-dwim)))
    (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
      (seq-doseq (context contexts)
        (eca-chat--insert-prompt (concat (eca-chat--context->str context 'static)
                                         " ")))
      (eca-chat--select-window)
      (goto-char (line-end-position)))))

;;;###autoload
(defun eca-chat-add-filepath-to-user-prompt ()
  "Add filepath to user prompt in chat in a DWIM manner.

- If a region selected, add filepath with lines range selected.
- If in Dired, add the marked files/dirs / current file/dir paths at point.
- If in Treemacs, add selected file/dir path.
- Else add current filepath."
  (interactive)
  (eca-assert-session-running (eca-session))
  (let* ((contexts (eca-chat--get-contexts-dwim)))
    (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
      (seq-doseq (context contexts)
        (eca-chat--insert-prompt (concat (eca-chat--filepath->str (plist-get context :path) (plist-get context :linesRange))
                                         " ")))
      (eca-chat--select-window)
      (goto-char (line-end-position)))))

;;;###autoload
(defun eca-chat-drop-context-from-system-prompt (&optional arg)
  "Drop context from system prompt in chat if found.
if ARG is current prefix, ask for file, otherwise drop current file."
  (interactive "P")
  (eca-assert-session-running (eca-session))
  (-let ((path (if (equal arg '(4))
                   (read-file-name "Select the file to drop from context: " (funcall eca-find-root-for-buffer-function))
                 (buffer-file-name))))
    (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
      (eca-chat--remove-context (list :type "file"
                                      :path path))
      (eca-chat-open (eca-session)))))

;;;###autoload
(defun eca-chat-stop-prompt ()
  "Stop chat prompt."
  (interactive)
  (eca-assert-session-running (eca-session))
  (eca-chat--stop-prompt (eca-session)))

;;;###autoload
(defun eca-chat-send-prompt (prompt)
  "Send PROMPT to current chat session."
  (interactive "sPrompt: ")
  (eca-assert-session-running (eca-session))
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (eca-chat--send-prompt (eca-session) prompt)))

;;;###autoload
(defun eca-chat-send-prompt-at-chat ()
  "Send the prompt in chat if not empty."
  (interactive)
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (let* ((prompt-start (eca-chat--prompt-field-start-point))
           (session (eca-session))
           (prompt (save-excursion
                     (goto-char prompt-start)
                     (string-trim (buffer-substring (point) (point-max))))))
      (when (and (not (string-empty-p prompt))
                 (not eca-chat--chat-loading))
        (eca-chat--send-prompt session prompt)))))

;;;###autoload
(defun eca-chat-toggle-window ()
  "Toggle presenting ECA chat window."
  (interactive)
  (let ((session (eca-session)))
    (eca-assert-session-running session)
    (let ((buffer (eca-chat--get-last-buffer session)))
      (if (buffer-live-p buffer)
          (if-let ((win (get-buffer-window buffer t)))
              ;; If visible, hide it
              (quit-window nil win)
            ;; If not visible, display it according to user settings
            (progn
              (eca-chat--display-buffer buffer)
              (with-current-buffer buffer
                (goto-char (point-max)))))
        (if (window-live-p (get-buffer-window (buffer-name)))
            (eca-chat--select-window)
          (eca-chat--pop-window))))))

(defvar eca-chat-new-chat-label
  (propertize "New chat" 'face 'font-lock-keyword-face))

;;;###autoload
(defun eca-chat-select ()
  "Select a chat."
  (interactive)
  (let ((session (eca-session))
        (get-title-fn (lambda ()
                        (or eca-chat--custom-title
                            eca-chat--title
                            eca-chat--id))))
    (eca-assert-session-running session)
    (let ((items (append
                  (sort
                   (-keep (lambda (buffer)
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (when-let ((item (funcall get-title-fn)))
                                  (propertize item
                                              'face (when eca-chat--chat-loading 'warning))))))
                          (eca-vals (eca--session-chats session)))
                   #'string<)
                  (list eca-chat-new-chat-label))))
      (when-let (chosen-title (completing-read
                               "Select the chat: "
                               (lambda (string pred action)
                                 (if (eq action 'metadata)
                                     `(metadata (display-sort-function . ,#'identity))
                                   (complete-with-action action items string pred)))
                               nil
                               t))
        (if-let (buffer (-first (lambda (buffer)
                                  (when (buffer-live-p buffer)
                                    (with-current-buffer buffer
                                      (string= chosen-title (funcall get-title-fn)))))
                                (eca-vals (eca--session-chats session))))
            (progn
              (setf (eca--session-last-chat-buffer session) buffer)
              (eca-chat-open session))
          (eca-chat-new))))))

;;;###autoload
(defun eca-chat-rename (new-name)
  "Rename last visited chat to a custom NEW-NAME."
  (interactive "sInform the new chat title: ")
  (eca-assert-session-running (eca-session))
  (with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (setq eca-chat--custom-title new-name)))

;;;###autoload
(defun eca-chat-new ()
  "Start a new ECA chat for same session."
  (interactive)
  (let ((session (eca-session)))
    (eca-assert-session-running session)
    (let ((_ (cl-incf eca-chat--new-chat-id))
          (new-chat-buffer (eca-chat--create-buffer session)))
      (setf (eca--session-last-chat-buffer session) new-chat-buffer)
      (eca-chat-open session))))

(declare-function whisper-run "ext:whisper" ())

;;;###autoload
(defun eca-chat-talk ()
  "Talk to the assistent by recording audio and transcribing it."
  (interactive)
  (unless (require 'whisper nil t)
    (user-error "Whisper.el is not available, please install it first"))
  (let ((session (eca-session)))
    (eca-assert-session-running session)
    (eca-chat-open session)
    (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
      (goto-char (point-max)))
    (let ((buffer (get-buffer-create "*whisper-stdout*")))
      (with-current-buffer buffer
        (erase-buffer)
        (make-local-variable 'whisper-after-transcription-hook)
        (add-hook 'whisper-after-transcription-hook
                  (lambda ()
                    (let ((transcription (buffer-substring
                                          (line-beginning-position)
                                          (line-end-position))))
                      (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
                        (eca-chat--insert transcription)
                        (newline)
                        (eca-chat--key-pressed-return))))
                  nil t)
        (whisper-run)
        (eca-info "Recording audio. Press RET when you are done.")
        (while (not (equal ?\r (read-char)))
          (sit-for 0.5))
        (whisper-run)))))

(defun eca-chat--format-message-for-completion (msg)
  "Format MSG for display in completion interface.
If MSG has :timestamp, prepends [HH:MM] to the text."
  (let ((timestamp (plist-get msg :timestamp))
        (text (plist-get msg :text)))
    (if timestamp
        (format "[%s] %s"
                (format-time-string "%H:%M" timestamp)
                text)
      text)))

(defun eca-chat--get-user-messages (&optional buffer)
  "Extract all user messages from the chat BUFFER.
If BUFFER is nil, use the last chat buffer from current session.
Returns a list of plists, each containing:
  :text      - the message text
  :start     - start position in buffer
  :end       - end position in buffer
  :id        - message ID from overlay
  :line      - line number of the message
  :timestamp - timestamp when message was sent

Messages are ordered from newest to oldest.
Returns empty list if session is not running or buffer is not available."
  (when-let* ((session (eca-session))
              (chat-buffer (or buffer (eca-chat--get-last-buffer session)))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (let ((messages '()))
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when-let* ((msg-id (overlay-get ov 'eca-chat--user-message-id))
                      (start (overlay-start ov))
                      (end (save-excursion
                             (goto-char start)
                             (while (and (not (eobp))
                                         (progn (forward-line 1)
                                                (eq (get-text-property (point) 'font-lock-face)
                                                    'eca-chat-user-messages-face))))
                             (line-end-position 0)))
                      (text (string-trim (buffer-substring-no-properties start end)))
                      (timestamp (overlay-get ov 'eca-chat--timestamp)))
            (unless (string-empty-p text)
              (push (list :text text
                          :start start
                          :end end
                          :id msg-id
                          :timestamp timestamp
                          :line (line-number-at-pos start))
                    messages))))
        messages))))

(defun eca-chat--select-message-from-completion (prompt)
  "Show completion with user messages using PROMPT.
Returns selected message plist or nil if no messages or cancelled."
  (when-let ((messages (eca-chat--get-user-messages)))
    (let ((table (make-hash-table :test 'equal)))
      (dolist (msg (reverse messages))
        (puthash (eca-chat--format-message-for-completion msg) msg table))
      (when-let ((choice (completing-read
                          prompt
                          (lambda (string pred action)
                            (if (eq action 'metadata)
                                `(metadata (display-sort-function . identity))
                              (complete-with-action action (hash-table-keys table) string pred)))
                          nil t)))
        (gethash choice table)))))

;;;###autoload
(defun eca-chat-timeline ()
  "Navigate to a user message via completion."
  (interactive)
  (if-let* ((selected-msg (eca-chat--select-message-from-completion "Timeline: "))
            (pos (plist-get selected-msg :start))
            (chat-buffer (eca-chat--get-last-buffer (eca-session))))
      (progn
        (eca-chat--display-buffer chat-buffer)
        (with-current-buffer chat-buffer
          (goto-char pos)
          (recenter)))
    (message "No user messages found")))

;;;###autoload
(defun eca-chat-clear-prompt ()
  "Clear the prompt input field in chat."
  (interactive)
  (when-let ((chat-buffer (eca-chat--get-last-buffer (eca-session))))
    (with-current-buffer chat-buffer
      (eca-chat--set-prompt ""))))

;;;###autoload
(defun eca-chat-repeat-prompt ()
  "Select a previous message and insert its text into the prompt."
  (interactive)
  (if-let* ((selected-msg (eca-chat--select-message-from-completion "Repeat prompt: "))
            (text (plist-get selected-msg :text))
            (chat-buffer (eca-chat--get-last-buffer (eca-session))))
      (progn
        (eca-chat--display-buffer chat-buffer)
        (with-current-buffer chat-buffer
          (eca-chat--set-prompt text)))
    (message "No user messages found")))

(provide 'eca-chat)
;;; eca-chat.el ends here
