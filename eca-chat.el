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
(require 'tab-line)
(require 'compat)

(require 'eca-util)
(require 'eca-api)
(require 'eca-mcp)
(require 'eca-diff)
(require 'eca-table)
(require 'eca-chat-expandable)
(require 'eca-chat-context)

(require 'evil nil t)

;; Variables

(eval-and-compile
  (defcustom eca-chat-parent-mode 'gfm-mode
    "The parent mode to eca-chat-mode inherit."
    :type 'symbol
    :group 'eca))

(defcustom eca-chat-mode-hook '()
  "Hooks to run after entering in eca chat mode hook."
  :type 'hook
  :group 'eca)

(defcustom eca-chat-finished-hook nil
  "List of functions to be called after ECA chat is finished.
For when chat went back to idle state."
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

(defcustom eca-chat-mcp-tool-call-loading-symbol "⏳"
  "The string used in eca chat buffer for mcp tool calls while loading."
  :type 'string
  :group 'eca)

(defcustom eca-chat-mcp-tool-call-pending-approval-symbol "🚧"
  "The string used in eca chat buffer for mcp tool calls waiting for approval."
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

(defcustom eca-chat-tab-line t
  "Whether to show a tab line with chat tabs at the top of each chat window.
When non-nil, enables `tab-line-mode' in chat buffers with tabs
for every open chat in the session.  Each tab shows the chat status
\(pending approval, loading), title and elapsed time."
  :type 'boolean
  :group 'eca)

(defcustom eca-chat-custom-model nil
  "Which model to use during chat, nil means use server's default.
Must be a valid model supported by server, check `eca-chat-select-model`."
  :type 'string
  :group 'eca)

(defcustom eca-chat-custom-agent nil
  "Which chat agent to use, if nil use server's default."
  :type 'string
  :group 'eca)

(defcustom eca-chat-trust-enable nil
  "When non-nil, auto-accept all tool calls."
  :type 'boolean
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

(defcustom eca-chat-mode-line-format
  '(:workspace-folders :add-workspace-button :spacer :elapsed-time "   " :usage " " :trust)
  "Format for the ECA chat mode line.

When set to a list, each element is a module keyword or a
literal string.  Modules are rendered in order; use `:spacer'
to separate left-aligned and right-aligned content.

Available modules:
  `:workspace-folders' - project root paths
  `:add-workspace-button' - clickable [+] button
  `:title' - chat title
  `:elapsed-time' - turn duration timer
  `:usage' - token/cost info (see `eca-chat-usage-string-format')
  `:server-version' - shows \"ECA <version>\"
  `:trust' - trust mode indicator (● red/gray)
  `:spacer' - elastic space that right-aligns everything after it

When set to a function, it receives the session as its sole
argument and should return a valid `mode-line-format' value.
The function is called once at buffer creation; include
`:eval' forms in the result for dynamic content.
This gives full control for powerline or doom-modeline users."
  :type '(choice
          (repeat
           (choice
            (string :tag "Literal string")
            (const :tag "Workspace folders" :workspace-folders)
            (const :tag "Add workspace button" :add-workspace-button)
            (const :tag "Chat title" :title)
            (const :tag "Elapsed time" :elapsed-time)
            (const :tag "Usage info" :usage)
            (const :tag "ECA server version" :server-version)
            (const :tag "Trust mode indicator" :trust)
            (const :tag "Right-align spacer" :spacer)))
          (function :tag "Custom function (receives session)"))
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

(defcustom eca-chat-save-chat-initial-path 'workspace-root
  "The initial path to show in the `eca-chat-save-to-file' prompt."
  :type '(choice
          (const :tag "Workspace root" workspace-root)
          (string :tag "Custom path"))
  :group 'eca)

(defcustom eca-chat-table-beautify t
  "When non-nil, apply enhanced visual styling to markdown tables.
Adds header highlighting, dimmed separators, zebra-striped rows,
and subtler pipe characters.  Only affects visual presentation via
overlays — the underlying buffer text is unchanged, so copy/paste
works normally."
  :type 'boolean
  :group 'eca)

;; Faces

(defface eca-chat-prompt-prefix-face
  '((((background dark))  (:foreground "lime green" :weight bold))
    (((background light)) (:foreground "dark green" :weight bold)))
  "Face for the `eca-chat-prompt-prefix`."
  :group 'eca)

(defface eca-chat-prompt-stop-face
  '((t (:inherit error :underline t :weight bold)))
  "Face for the stop action when loading."
  :group 'eca)

(defface eca-chat-queued-prompt-face
  '((t :inherit font-lock-comment-face :slant italic :underline nil))
  "Face for the queued prompt indicator."
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
  '((((background dark))  (:foreground "dodger blue" :underline t :weight bold))
    (((background light)) (:foreground "blue3" :underline t :weight bold)))
  "Face for the diff view button."
  :group 'eca)

(defface eca-chat-title-face
  '((t :height 0.9))
  "Face for the chat title."
  :group 'eca)

(defface eca-chat-user-messages-face
  '((t :inherit font-lock-doc-face))
  "Face for the user sent messages in chat."
  :group 'eca)

(defface eca-chat-rollback-face
  '((t (:inherit eca-chat-user-messages-face
        :weight bold
        :underline t)))
  "Face for the rollback button."
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

(defface eca-chat-subagent-tool-call-label-face
  '((t :inherit font-lock-constant-face))
  "Face for subagent tool call labels in chat."
  :group 'eca)

(defface eca-chat-subagent-steps-info-face
  '((t :inherit font-lock-comment-face :slant italic :height 0.9))
  "Face for the steps done by subagent."
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

(defface eca-chat-task-prefix-face
  '((t :inherit font-lock-operator-face :slant italic))
  "Face for the task text prefix in task label."
  :group 'eca)

(defface eca-chat-task-label-face
  '((t :height 0.9))
  "Face for the task area label in chat."
  :group 'eca)

(defface eca-chat-task-label-in-progress-face
  '((t :inherit font-lock-string-face))
  "Face for the task area label when a task is in progress."
  :group 'eca)

(defface eca-chat-task-in-progress-face
  '((t :inherit font-lock-string-face :weight bold))
  "Face for in-progress tasks in the task area."
  :group 'eca)

(defface eca-chat-task-progress-face
  '((t :inherit font-lock-comment-face :slant italic :height 0.9))
  "Face for the progress counter (e.g. 1/5) in the task label."
  :group 'eca)

(defface eca-chat-task-done-face
  '((t :inherit font-lock-comment-face :strike-through t))
  "Face for completed tasks in the task area."
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

(defface eca-chat-trust-on-face
  '((t :weight bold :inherit 'error))
  "Face for trust mode when on in mode-line."
  :group 'eca)

(defface eca-chat-trust-off-face
  '((t :inherit default))
  "Face for trust mode when off in mode-line."
  :group 'eca)

(defface eca-chat-usage-string-face
  '((t :height 0.9 :inherit font-lock-doc-face))
  "Face for the strings segments in usage string in mode-line of the chat."
  :group 'eca)

(defface eca-chat-elapsed-time-face
  '((t :height 0.9 :inherit font-lock-comment-face))
  "Face for the elapsed time indicator in mode-line of the chat."
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
(defvar-local eca-chat--selected-model nil)
(defvar-local eca-chat--selected-agent nil)
(defvar-local eca-chat--selected-variant nil)
(defvar-local eca-chat--selected-trust nil)
(defvar-local eca-chat--last-request-id 0)
(defvar-local eca-chat--spinner-string "")
(defvar-local eca-chat--spinner-timer nil)
(defvar-local eca-chat--prompt-start-time nil
  "Start time of the current prompt, from `current-time'.")
(defvar-local eca-chat--turn-duration-secs nil
  "Duration in seconds of the last completed turn.")
(defvar-local eca-chat--modeline-timer nil
  "Timer that refreshes the mode-line every second during loading.")

(defvar-local eca-chat--tool-call-elapsed-times (make-hash-table :test 'equal)
  "Mapping tool-call ID to `current-time' when toolCallRunning was received.")
(defvar-local eca-chat--tool-call-elapsed-timer nil
  "Repeating timer that updates elapsed-time display for running tool calls.")

(defvar-local eca-chat--table-resize-timer nil)
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
(defvar-local eca-chat--queued-prompt nil)
(defvar-local eca-chat--subagent-chat-id->tool-call-id (make-hash-table :test 'equal)
  "Hash table mapping subagent chatId to the parent tool call expandable block id.")
(defvar-local eca-chat--subagent-usage (make-hash-table :test 'equal)
  "Hash table mapping tool-call-id to a plist (:session-tokens N :context-limit N).
Stores the latest usage data received for each running subagent.")

(defvar-local eca-chat--server-version nil
  "Cached ECA server version string for mode-line display.")

(defvar-local eca-chat--task-state nil
  "Current task state plist with :goal and :tasks.
Each task is a plist with :id, :content, :status, :priority, etc.")



(defvar eca-chat--new-chat-id 0)
(defvar eca-chat--last-known-model nil)
(defvar eca-chat--last-known-agent nil)
(defvar eca-chat--last-known-variant nil)

(defvar eca--chat-init-session nil
  "Dynamically bound session during `eca-chat-mode' initialization.")

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
    (define-key map (kbd "TAB") #'eca-chat--key-pressed-tab)
    (define-key map (kbd "C-c C-k") #'eca-chat-reset)
    (define-key map (kbd "C-c C-l") #'eca-chat-clear)
    (define-key map (kbd "C-c C-t") #'eca-chat-toggle-trust)
    (define-key map (kbd "C-c C-S-t") #'eca-chat-talk)
    (define-key map (kbd "C-c C-S-b") #'eca-chat-select-agent)
    (define-key map (kbd "C-c C-b") #'eca-chat-cycle-agent)
    (define-key map (kbd "C-c C-m") #'eca-chat-select-model)
    (define-key map (kbd "C-c C-v") #'eca-chat-select-variant)
    (define-key map (kbd "C-c C-n") #'eca-chat-new)
    (define-key map (kbd "C-c C-f") #'eca-chat-select)
    (define-key map (kbd "C-c C-p") #'eca-chat-repeat-prompt)
    (define-key map (kbd "C-c C-d") #'eca-chat-clear-prompt)
    (define-key map (kbd "C-c C-h") #'eca-chat-timeline)
    (define-key map (kbd "C-c C-a") #'eca-chat-tool-call-accept-all)
    (define-key map (kbd "C-c C-S-a") #'eca-chat-tool-call-accept-next)
    (define-key map (kbd "C-c C-y") #'eca-chat-tool-call-accept-all-and-remember)
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

(defun eca-chat--delete-chat ()
  "Delete current chat."
  (when (and (or (eq #'kill-current-buffer this-command)
                 (eq #'kill-buffer this-command)
                 (and (symbolp this-command)
                      (string-prefix-p "eca-" (symbol-name this-command))))
             eca-chat--id
             (not eca-chat--closed)
             (yes-or-no-p "Delete chat from server side (otherwise it will just kill the buffer) ?"))
    (eca-api-request-sync (eca-session)
                          :method "chat/delete"
                          :params (list :chatId eca-chat--id))))

(defun eca-chat--insert (&rest contents)
  "Insert CONTENTS reseting undo-list to avoid buffer inconsistencies."
  (apply #'insert contents)
  (setq-local buffer-undo-list nil))

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
  "Return a propertized presentable time for MS."
  (let ((secs (/ ms 1000)))
    (propertize (eca-chat--format-duration secs)
                'font-lock-face 'eca-chat-time-face)))

(defun eca-chat--elapsed-time-string (start-time)
  "Return a propertized elapsed-time string since START-TIME.
Uses `eca-chat--format-duration' for display, with
`eca-chat-time-face' and a `eca-chat--elapsed-time' text
property so the timer can locate it."
  (let* ((elapsed (floor (float-time (time-subtract (current-time) start-time))))
         (str (concat " " (propertize (eca-chat--format-duration elapsed)
                                      'font-lock-face 'eca-chat-time-face))))
    (propertize str 'eca-chat--elapsed-time t)))

(defun eca-chat--tool-call-elapsed-start (id)
  "Start tracking elapsed time for tool call ID.
Records current time (only on first call for ID) and ensures the shared
update timer is running."
  (unless (gethash id eca-chat--tool-call-elapsed-times)
    (puthash id (current-time) eca-chat--tool-call-elapsed-times))
  (unless eca-chat--tool-call-elapsed-timer
    (let ((buf (current-buffer))
          (timer nil))
      (setq timer
            (run-with-timer
             1 1
             (lambda ()
               (if (buffer-live-p buf)
                   (with-current-buffer buf
                     (eca-chat--tool-call-elapsed-tick))
                 ;; Buffer was killed — cancel ourselves to avoid leak
                 (cancel-timer timer)))))
      (setq eca-chat--tool-call-elapsed-timer timer))))

(defun eca-chat--tool-call-elapsed-stop (id)
  "Stop tracking elapsed time for tool call ID.
Cancels the shared timer when no more tool calls are being tracked."
  (remhash id eca-chat--tool-call-elapsed-times)
  (when (and eca-chat--tool-call-elapsed-timer
             (zerop (hash-table-count eca-chat--tool-call-elapsed-times)))
    (cancel-timer eca-chat--tool-call-elapsed-timer)
    (setq eca-chat--tool-call-elapsed-timer nil)))

(defun eca-chat--tool-call-elapsed-tick ()
  "Timer callback: update elapsed-time display for all running tool call."
  (eca-chat--allow-write
   (maphash
    (lambda (id start-time)
      (when-let* ((ov-label (eca-chat--get-expandable-content id)))
        (let* ((label-start (overlay-start ov-label))
               (ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content))
               (new-time (eca-chat--elapsed-time-string start-time)))
          (when ov-content
            (let ((label-end (1- (overlay-start ov-content))))
              (save-excursion
                ;; Find the text span with eca-chat--elapsed-time property
                (goto-char label-start)
                (let ((prop-start (text-property-any label-start label-end
                                                     'eca-chat--elapsed-time t)))
                  (when prop-start
                    (let ((prop-end (next-single-property-change
                                    prop-start 'eca-chat--elapsed-time nil label-end)))
                      (goto-char prop-start)
                      (delete-region prop-start prop-end)
                      (insert new-time)))))))
          ;; Keep the overlay property in sync so that functions which
          ;; rebuild the label from stored properties (e.g.
          ;; eca-chat--update-parent-subagent-status) use the latest value.
          (when (overlay-get ov-label 'eca-chat--tool-call-time)
            (overlay-put ov-label 'eca-chat--tool-call-time new-time)))))
    eca-chat--tool-call-elapsed-times)))

(defun eca-chat--tool-call-elapsed-stop-all ()
  "Cancel the elapsed-time timer and clear all tracked tool call."
  (when eca-chat--tool-call-elapsed-timer
    (cancel-timer eca-chat--tool-call-elapsed-timer)
    (setq eca-chat--tool-call-elapsed-timer nil))
  (clrhash eca-chat--tool-call-elapsed-times))

(defun eca-chat--agent ()
  "The chat agent considering default and user option."
  (or eca-chat-custom-agent
      eca-chat--selected-agent
      eca-chat--last-known-agent))

(defun eca-chat--model ()
  "The chat model considering default and user option."
  (or eca-chat-custom-model
      eca-chat--selected-model
      eca-chat--last-known-model))

(defun eca-chat--variant ()
  "The chat variant for the current model."
  (or eca-chat--selected-variant
      eca-chat--last-known-variant))

(defun eca-chat--trust ()
  "Non-nil when trust mode is on, auto-accepts tool call."
  (or eca-chat-trust-enable
      eca-chat--selected-trust))

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
            ("requires-auth" (cl-incf starting))
            ("failed" (cl-incf failed))))
        (concat (funcall propertize-fn failed 'error (or (> running 0) (> starting 0)))
                (funcall propertize-fn starting 'warning (> running 0))
                (funcall propertize-fn running 'success))))))

(defun eca-chat--build-tool-call-approval-str-content (session id spacing-line-prefix &optional chat-id)
  "Build the tool call approval string for SESSION, ID and SPACING-LINE-PREFIX.
CHAT-ID overrides the buffer-local `eca-chat--id' for the approval
request, useful for subagent tool calls."
  (let ((keybinding-for (lambda (command)
                          (concat "("
                                  (key-description (car (where-is-internal command eca-chat-mode-map)))
                                  ")")))
        (effective-chat-id (or chat-id eca-chat--id)))
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
                               :params (list :chatId effective-chat-id
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
                               :params (list :chatId effective-chat-id
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
                               :params (list :chatId effective-chat-id
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
  (let ((task-area-ov (make-overlay (1+ (point)) (line-end-position) (current-buffer) nil t)))
    (overlay-put task-area-ov 'eca-chat-task-area t)
    (eca-chat--insert " ")
    (move-overlay task-area-ov (overlay-start task-area-ov) (1- (overlay-end task-area-ov))))
  (let ((progress-area-ov (make-overlay (1+ (point)) (line-end-position) (current-buffer) nil t)))
    (overlay-put progress-area-ov 'eca-chat-progress-area t)
    (eca-chat--insert "\n")
    (move-overlay progress-area-ov (overlay-start progress-area-ov) (1- (overlay-end progress-area-ov))))
  (let ((queued-area-ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)) (current-buffer))))
    (overlay-put queued-area-ov 'eca-chat-queued-area t))
  (let ((context-area-ov (make-overlay (line-beginning-position) (line-end-position) (current-buffer) nil t)))
    (overlay-put context-area-ov 'eca-chat-context-area t)
    (eca-chat--insert (propertize eca-chat-context-prefix 'font-lock-face 'eca-chat-context-unlinked-face))
    (eca-chat--insert "\n")
    (move-overlay context-area-ov (overlay-start context-area-ov) (1- (overlay-end context-area-ov))))
  (let ((loading-area-ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)) (current-buffer))))
    (overlay-put loading-area-ov 'eca-chat-loading-area t))
  (eca-chat--insert "\n")
  (let ((prompt-field-ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)) (current-buffer))))
    (overlay-put prompt-field-ov 'eca-chat-prompt-field t)
    (overlay-put prompt-field-ov 'before-string (propertize eca-chat-prompt-prefix 'font-lock-face 'eca-chat-prompt-prefix-face))))

(defun eca-chat--clear (&optional new-prompt-content)
  "Clear the chat for SESSION and then insert NEW-PROMPT-CONTENT."
  (erase-buffer)
  (remove-overlays (point-min) (point-max))
  (setq-local eca-chat--task-state nil)
  (clrhash eca-chat--subagent-chat-id->tool-call-id)
  (clrhash eca-chat--subagent-usage)
  (eca-chat--insert "\n")
  (eca-chat--insert-prompt-string)
  (eca-chat--refresh-context)
  (when new-prompt-content
    (eca-chat--set-prompt new-prompt-content)))

(defun eca-chat--stop-prompt (session)
  "Stop the running chat prompt for SESSION."
  (when eca-chat--chat-loading
    (eca-api-notify session
                    :method "chat/promptStop"
                    :params (list :chatId eca-chat--id))
    (eca-chat--set-chat-loading session nil)))

(defun eca-chat--rollback (session content-id)
  "Rollback chat messages for SESSION to before CONTENT-ID."
  (unless eca-chat--chat-loading
    (let ((rollback-messages-and-tools-str "1. Rollback messages and changes done by tool calls")
          (rollback-messages-str "2. Rollback only messages")
          (rollback-tools-str "3. Rollback only changes done by tool calls"))
      (when-let* ((rollback-type (completing-read "Select the rollback type:"
                                                  (lambda (s pred action)
                                                    (if (eq 'metadata action)
                                                        `(metadata (display-sort-function . ,#'identity))
                                                      (complete-with-action action
                                                                            (list rollback-messages-and-tools-str
                                                                                  rollback-messages-str
                                                                                  rollback-tools-str)
                                                                            s
                                                                            pred))) nil t)))
        (let ((include (cond
                        ((string= rollback-type rollback-messages-str) ["messages"])
                        ((string= rollback-type rollback-tools-str) ["tools"])
                        ((string= rollback-type rollback-messages-and-tools-str) ["messages" "tools"]))))
          (eca-api-request-sync session
                                :method "chat/rollback"
                                :params (list :chatId eca-chat--id
                                              :contentId content-id
                                              :include include)))))))

(defun eca-chat--set-chat-loading (session loading)
  "Set the SESSION chat to a loading state if LOADING is non nil.
Otherwise to a not loading state."
  (unless (eq eca-chat--chat-loading loading)
    (setq-local eca-chat--chat-loading loading)
    (if loading
        (progn
          (setq-local eca-chat--prompt-start-time (current-time))
          (let ((buf (current-buffer)))
            (setq-local eca-chat--modeline-timer
                        (run-with-timer 1 1
                                        (lambda ()
                                          (when (buffer-live-p buf)
                                            (with-current-buffer buf
                                              (eca-chat--force-tab-line-update))))))))
      (when eca-chat--prompt-start-time
        (setq-local eca-chat--turn-duration-secs
                    (floor (float-time (time-subtract (current-time) eca-chat--prompt-start-time))))
        (setq-local eca-chat--prompt-start-time nil))
      (when eca-chat--modeline-timer
        (cancel-timer eca-chat--modeline-timer)
        (setq-local eca-chat--modeline-timer nil))
      (eca-chat--force-tab-line-update))
    ;; (setq-local buffer-read-only loading)
    (let ((loading-area-ov (eca-chat--loading-area-ov))
          (stop-text (eca-buttonize
                      eca-chat-mode-map
                      (propertize "stop" 'font-lock-face 'eca-chat-prompt-stop-face)
                      (lambda () (eca-chat--stop-prompt session)))))
      (if eca-chat--chat-loading
          (progn
            (overlay-put loading-area-ov 'before-string (propertize eca-chat-prompt-prefix-loading 'font-lock-face 'default))
            (save-excursion
              (goto-char (overlay-start loading-area-ov))
              (eca-chat--insert stop-text)))
        (progn
          (overlay-put loading-area-ov 'before-string "")
          (save-excursion
            (goto-char (overlay-start loading-area-ov))
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

(defun eca-chat--loading-area-ov ()
  "Return the overlay for the loading area."
  (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-loading-area)))
          (overlays-in (point-min) (point-max))))

(defun eca-chat--prompt-field-ov ()
  "Return the overlay for the prompt field."
  (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-prompt-field)))
          (overlays-in (point-min) (point-max))))

(defun eca-chat--prompt-field-start-point ()
  "Return the metadata overlay for the prompt field start point."
  (-some-> (eca-chat--prompt-field-ov) (overlay-start)))

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

(defun eca-chat--task-area-ov ()
  "Return the overlay for the task area."
  (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-task-area)))
          (overlays-in (point-min) (point-max))))

(defun eca-chat--queued-area-ov ()
  "Return the overlay for the queued prompt area."
  (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-queued-area)))
          (overlays-in (point-min) (point-max))))

(defun eca-chat--prompt-area-start-point ()
  "Return the metadata overlay for the prompt area start point."
  (-some-> (eca-chat--prompt-area-ov)
    (overlay-start)))

(defconst eca-chat--task-block-id "eca-chat-task"
  "Fixed expandable block ID for the task area widget.")

(defun eca-chat--content-insertion-point ()
  "Return the point where new chat content should be inserted.
Returns the position just before the prompt area start."
  (1- (eca-chat--prompt-area-start-point)))

(defun eca-chat--ensure-prompt-visible ()
  "Scroll the chat window so the prompt area stays visible.
Only acts when the user is currently viewing the bottom of the
buffer.  When the user has scrolled up to read earlier content,
scrolling is suppressed so the view does not jump."
  (when-let* ((win (get-buffer-window (current-buffer))))
    (let* ((prompt-start (eca-chat--prompt-area-start-point))
           (win-end (window-end win t)))
      ;; Only auto-scroll when the prompt separator was already
      ;; visible — meaning the user is at the bottom of the chat.
      (when (and prompt-start (>= win-end prompt-start))
        (with-selected-window win
          (goto-char (point-max))
          (recenter -1))))))

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

         ;; Handle some evil commands
         ((and in-prompt?
               (or (eq #'evil-delete this-command)
                   (eq #'evil-change-line this-command)))
          (setf (nth 2 args) nil) ;; do not delete prompt line passing nil argument
          (apply side-effect-fn args))

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

(defun eca-chat--prompt-content ()
  "Return the current prompt content."
  (when-let ((prompt-start (eca-chat--prompt-field-start-point)))
    (save-excursion
      (goto-char prompt-start)
      (string-trim (buffer-substring (point) (point-max))))))

(defun eca-chat--extract-contexts-from-prompt ()
  "Extract contexts from prompt text properties.
Resteps a list of context plists found in the prompt field."
  (when-let ((prompt-start (eca-chat--prompt-field-start-point)))
    (let ((contexts '())
          (pos prompt-start)
          (end (point-max)))
      (while (< pos end)
        (when-let ((context (get-text-property pos 'eca-chat-context-item)))
          (unless (member context contexts)
            (push context contexts)))
        (setq pos (next-single-property-change pos 'eca-chat-context-item nil end)))
      (nreverse contexts))))

(defun eca-chat--send-prompt (session prompt)
  "Send PROMPT to server for SESSION."
  (when eca-chat--closed
    (user-error (eca-error "This chat is closed")))
  (let* ((prompt-contexts (eca-chat--extract-contexts-from-prompt))
         (refined-contexts (-map #'eca-chat--refine-context
                                 (append eca-chat--context prompt-contexts))))
    (when (seq-empty-p eca-chat--history) (eca-chat--clear))
    (add-to-list 'eca-chat--history prompt)
    (setq eca-chat--history-index -1)
    (eca-chat--set-prompt "")
    (eca-chat--set-chat-loading session t)
    (eca-api-request-async
     session
     :method "chat/prompt"
     :params (append (list :message (eca-chat--normalize-prompt prompt)
                          :request-id (cl-incf eca-chat--last-request-id)
                          :chatId eca-chat--id
                          :model (eca-chat--model)
                          :agent (eca-chat--agent)
                          :contexts (vconcat refined-contexts))
                     (when-let* ((variant (eca-chat--variant)))
                       (unless (string= variant "-")
                         (list :variant variant)))
                     (when (eca-chat--trust)
                       (list :trust t)))
     :success-callback (-lambda (res)
                         (setq-local eca-chat--id (plist-get res :chatId))))))

(defun eca-chat--queued-prompt-display-string (text)
  "Return a display string for queued prompt TEXT, truncated to 40 chars."
  (let* ((single-line (replace-regexp-in-string "\n" " " text))
         (truncated (if (> (length single-line) 40)
                        (concat (substring single-line 0 40) "...")
                      single-line)))
    (propertize (concat "Queued: " truncated "\n")
                'face 'eca-chat-queued-prompt-face)))

(defun eca-chat--update-queued-area ()
  "Update the queued-area overlay to reflect `eca-chat--queued-prompt'."
  (when-let* ((ov (eca-chat--queued-area-ov)))
    (overlay-put ov 'before-string
                 (if eca-chat--queued-prompt
                     (eca-chat--queued-prompt-display-string eca-chat--queued-prompt)
                   ""))))

(defun eca-chat--queue-prompt (prompt)
  "Queue PROMPT to be sent to SESSION when it finish current prompt."
  (setq-local eca-chat--queued-prompt (if eca-chat--queued-prompt
                                          (concat eca-chat--queued-prompt "\n" prompt)
                                        prompt))
  (eca-chat--update-queued-area)
  (eca-chat--set-prompt ""))

(defun eca-chat--send-queued-prompt (session)
  "Send any queued prompt for SESSION."
  (when eca-chat--queued-prompt
    (eca-chat--send-prompt session eca-chat--queued-prompt)
    (setq-local eca-chat--queued-prompt nil)
    (eca-chat--update-queued-area)))

(defun eca-chat--key-pressed-return ()
  "Send the current prompt to eca process if in prompt."
  (interactive)
  (eca-chat--allow-write
   (let* ((session (eca-session))
          (prompt (eca-chat--prompt-content)))
     (cond
      ;; check it's an actionable text
      ((-some->> (thing-at-point 'symbol) (get-text-property 0 'eca-button-on-action))
       (-some->> (thing-at-point 'symbol)
         (get-text-property 0 'eca-button-on-action)
         (funcall)))

      ;; check is inside a expandable text
      ((eca-chat--expandable-content-at-point)
       (let ((ov (eca-chat--expandable-content-at-point)))
         (eca-chat--expandable-content-toggle (overlay-get ov 'eca-chat--expandable-content-id))))

      ;; check prompt
      ((and (not (string-empty-p prompt))
            (not eca-chat--chat-loading))
       (eca-chat--send-prompt session prompt))

      ((and (not (string-empty-p prompt))
            eca-chat--chat-loading)
       (eca-chat--queue-prompt prompt))

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
          (agent-keymap (make-sparse-keymap))
          (variant-keymap (make-sparse-keymap))
          (mcp-keymap (make-sparse-keymap)))
      (define-key model-keymap (kbd "<header-line> <mouse-1>") #'eca-chat-select-model)
      (define-key agent-keymap (kbd "<header-line> <mouse-1>") #'eca-chat-select-agent)
      (define-key variant-keymap (kbd "<header-line> <mouse-1>") #'eca-chat-select-variant)
      (define-key mcp-keymap (kbd "<header-line> <mouse-1>") #'eca-mcp-details)
      (append
       (list (propertize "model:"
                         'font-lock-face 'eca-chat-option-key-face
                         'pointer 'hand
                         'keymap model-keymap)
             (-some-> (eca-chat--model)
               (propertize
                'font-lock-face 'eca-chat-option-value-face
                'pointer 'hand
                'keymap model-keymap))
             "  "
             (propertize "agent:"
                         'font-lock-face 'eca-chat-option-key-face
                         'pointer 'hand
                         'keymap agent-keymap)
             (-some-> (eca-chat--agent)
               (propertize 'font-lock-face 'eca-chat-option-value-face
                           'pointer 'hand
                           'keymap agent-keymap))
             "  ")
       (list (propertize "variant:"
                        'font-lock-face 'eca-chat-option-key-face
                        'pointer 'hand
                        'keymap variant-keymap)
             (propertize (or (eca-chat--variant) "-")
                         'font-lock-face 'eca-chat-option-value-face
                         'pointer 'hand
                         'keymap variant-keymap)
             "  ")
       (list (propertize "mcps:"
                         'font-lock-face 'eca-chat-option-key-face
                         'pointer 'hand
                         'keymap mcp-keymap)
             (propertize (eca-chat--mcps-summary session)
                         'pointer 'hand
                         'keymap mcp-keymap))))))

(defun eca-chat--number->friendly-number (n)
  "Format N as `x.yM` for |N| >= 1M, `x.yK` for |N| >= 1K.
Otherwise show plain integer."
  (cond
   ((not n)
    "")

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

(defun eca-chat--subagent-usage-str (tool-call-id)
  "Return a formatted usage string for subagent TOOL-CALL-ID.
Returns a string like \"31.5K / 200K\" or \"\" if no usage data."
  (if-let* ((usage (gethash tool-call-id eca-chat--subagent-usage))
            (session-tokens (plist-get usage :session-tokens))
            (context-limit (plist-get usage :context-limit)))
      (format "%s / %s"
              (eca-chat--number->friendly-number session-tokens)
              (eca-chat--number->friendly-number context-limit))
    ""))

(defun eca-chat--subagent-steps-info (step max-steps usage-str)
  "Build a propertized steps-info string from STEP, MAX-STEPS and USAGE-STR."
  (propertize (cond
               ((and step max-steps (not (string-empty-p usage-str)))
                (format " (%d/%d steps, %s)" step max-steps usage-str))
               ((and step (not (string-empty-p usage-str)))
                (format " (%d steps, %s)" step usage-str))
               ((not (string-empty-p usage-str))
                (format " (%s)" usage-str))
               ((and step max-steps)
                (format " (%d/%d steps)" step max-steps))
               (step
                (format " (%d steps)" step))
               (t ""))
              'font-lock-face 'eca-chat-subagent-steps-info-face))

(defun eca-chat--usage-str ()
  "Return the usage string of this chat."
  (when (or eca-chat--message-input-tokens
            eca-chat--message-output-tokens
            eca-chat--session-tokens
            eca-chat--message-cost
            eca-chat--session-cost)
    (-> (-map (lambda (segment)
                (propertize
                 (pcase segment
                   (:message-input-tokens (eca-chat--number->friendly-number eca-chat--message-input-tokens))
                   (:message-output-tokens (eca-chat--number->friendly-number eca-chat--message-output-tokens))
                   (:session-tokens (eca-chat--number->friendly-number eca-chat--session-tokens))
                   (:message-cost (concat "$" eca-chat--message-cost))
                   (:session-cost (concat "$" eca-chat--session-cost))
                   (:context-limit (eca-chat--number->friendly-number eca-chat--session-limit-context))
                   (:output-limit (eca-chat--number->friendly-number eca-chat--session-limit-output))
                   (_ segment))
                 'font-lock-face 'eca-chat-usage-string-face))
              eca-chat-usage-string-format)
        (string-join ""))))

(defun eca-chat--format-duration (secs)
  "Format SECS into a human-readable duration string.
Returns \"Xs\" for < 60s, \"Xm Ys\" for >= 60s,
or \"Xm\" when seconds are zero."
  (let ((mins (/ secs 60))
        (remaining-secs (mod secs 60)))
    (cond
     ((< secs 60)          (format "%ds" secs))
     ((zerop remaining-secs) (format "%dm" mins))
     (t                      (format "%dm %ds" mins remaining-secs)))))

(defun eca-chat--turn-duration-str ()
  "Return formatted turn duration string, or nil."
  (when-let* ((dur (cond
                    (eca-chat--prompt-start-time
                     (floor (float-time
                             (time-subtract (current-time)
                                            eca-chat--prompt-start-time))))
                    (eca-chat--turn-duration-secs
                     eca-chat--turn-duration-secs))))
    (concat (eca-chat--format-duration dur)
            (when eca-chat--prompt-start-time "…"))))

(defun eca-chat--has-pending-approvals-p ()
  "Return non-nil if current buffer has any pending approval tool call."
  (save-excursion
    (goto-char (point-min))
    (not (null (text-property-search-forward
                'eca-tool-call-pending-approval-accept t t)))))

(defun eca-chat--chat-status-prefix ()
  "Return a status prefix string for the current chat buffer.
Returns \"🚧 \" for pending approvals, \"⏳ \" for loading, or \"\" otherwise."
  (cond
   ((eca-chat--has-pending-approvals-p) "🚧 ")
   (eca-chat--chat-loading "⏳ ")
   (t "")))

(defun eca-chat--tab-line-tab-name (buffer)
  "Return a formatted tab label for chat BUFFER.
Shows 🚧 prefix for pending approvals."
  (with-current-buffer buffer
    (let* ((title (eca-chat-title))
           (pending (eca-chat--has-pending-approvals-p)))
      (concat " " (when pending "🚧 ") title " "))))

(defun eca-chat--tab-line-face (tab _tabs face _selected-p _buffer)
  "Apply warning face to loading chat tabs considering TAB.
Merges `warning' with the default tab FACE via inheritance."
  (let ((buf (cdr (assq 'buffer tab))))
    (if (and buf (buffer-live-p buf)
             (buffer-local-value 'eca-chat--chat-loading buf))
        `(:inherit (warning ,face))
      face)))

(defun eca-chat--tab-line-tabs ()
  "Return tab descriptors for all chats in the current session.
Each tab is an alist with `name', `buffer', and `selected' entries.
Tabs are ordered oldest-first so new chats appear on the right."
  (when-let ((session (ignore-errors (eca-session))))
    (let* ((current-buf (current-buffer))
           (tabs (-keep
                  (lambda (buf)
                    (when (buffer-live-p buf)
                      `(tab
                        (name . ,(eca-chat--tab-line-tab-name buf))
                        (buffer . ,buf)
                        (selected . ,(eq buf current-buf)))))
                  (eca-vals (eca--session-chats session)))))
      (nreverse tabs))))

(defun eca-chat--tab-line-close-tab (&optional e)
  "Close the chat tab clicked on.
If closing the current buffer, switch to another chat tab first.
E is the mouse event."
  (interactive "e")
  (let* ((posnp (event-start e))
         (window (posn-window posnp))
         (tab-prop (get-pos-property 1 'tab (car (posn-string posnp))))
         (buffer (if (bufferp tab-prop)
                     tab-prop
                   (cdr (assq 'buffer tab-prop)))))
    (when (and buffer (buffer-live-p buffer))
      (with-selected-window window
        (when (eq buffer (current-buffer))
          ;; Switch to another chat before killing
          (let* ((session (ignore-errors (eca-session)))
                 (other (-first (lambda (buf)
                                  (and (buffer-live-p buf)
                                       (not (eq buf buffer))))
                                (eca-vals (eca--session-chats session)))))
            (when other
              (setf (eca--session-last-chat-buffer session) other)
              (switch-to-buffer other))))
        (kill-buffer buffer)))))

(defvar eca-chat--tab-close-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-line mouse-1] #'eca-chat--tab-line-close-tab)
    (define-key map [tab-line mouse-2] #'eca-chat--tab-line-close-tab)
    map)
  "Keymap for the tab-line close button in chat buffers.")

(defun eca-chat--force-tab-line-update ()
  "Force tab-line to redraw in all chat windows by clearing the render cache."
  (walk-windows
   (lambda (win)
     (when (provided-mode-derived-p
            (buffer-local-value 'major-mode (window-buffer win))
            'eca-chat-mode)
       (set-window-parameter win 'tab-line-cache nil)))
   nil t)
  (force-mode-line-update t))

(defun eca-chat--sync-last-buffer ()
  "Update session last-chat-buffer to track the current chat buffer."
  (when-let ((session (ignore-errors (eca-session))))
    (unless (eq (eca--session-last-chat-buffer session) (current-buffer))
      (setf (eca--session-last-chat-buffer session) (current-buffer)))))

(defun eca-chat-add-workspace-root ()
  "Prompt for a directory and add it as workspace."
  (interactive)
  (when-let ((session (eca-session)))
    (let ((folder (read-directory-name "Add workspace: ")))
      (eca--session-add-workspace-folder session folder)
      (force-mode-line-update))))

(defvar eca-chat--add-workspace-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
                #'eca-chat-add-workspace-root)
    map)
  "Keymap for the modeline [+] workspace button.")

(defvar eca-chat--trust-toggle-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
                #'eca-chat-toggle-trust)
    map)
  "Keymap for the modeline trust indicator.")

(defun eca-chat--mode-line-module (session keyword)
  "Return mode-line string segment for module KEYWORD in SESSION."
  (pcase keyword
    (:workspace-folders
     (let ((home (expand-file-name "~")))
       (string-join
        (mapcar (lambda (f)
                  (if (string-prefix-p home f)
                      (concat "~" (substring f (length home)))
                    f))
                (eca--session-workspace-folders session))
        ", ")))
    (:add-workspace-button
     (propertize " [+]"
                 'face 'shadow
                 'mouse-face 'highlight
                 'help-echo "Add workspace folder"
                 'local-map eca-chat--add-workspace-map))
    (:title
     (eca-chat-title))
    (:elapsed-time
     (when-let* ((str (eca-chat--turn-duration-str)))
       (propertize (concat "⏱ " str) 'font-lock-face 'eca-chat-elapsed-time-face)))
    (:usage
     (eca-chat--usage-str))
    (:server-version
     (when eca-chat--server-version
       (concat "ECA " eca-chat--server-version)))
    (:trust
     (propertize "⬤"
                 'face (if (eca-chat--trust)
                           'eca-chat-trust-on-face
                         'eca-chat-trust-off-face)
                 'mouse-face 'highlight
                 'help-echo (if (eca-chat--trust)
                                "Trust ON - auto-accepting tool calls"
                              "Trust OFF")
                 'local-map eca-chat--trust-toggle-map))
    ((pred stringp) keyword)
    (_ "")))

(defun eca-chat--mode-line-string (session)
  "Build mode-line string for SESSION from `eca-chat-mode-line-format'."
  (if eca-chat--closed
      (propertize "*Closed session*"
                  'font-lock-face 'eca-chat-system-messages-face)
    (let* ((fmt eca-chat-mode-line-format)
           (spacer-pos (-elem-index :spacer fmt))
           (left-modules (if spacer-pos (-take spacer-pos fmt) fmt))
           (right-modules (when spacer-pos
                            (-drop (1+ spacer-pos) fmt)))
           (left (string-join
                  (-non-nil
                   (-map (lambda (m)
                           (eca-chat--mode-line-module session m))
                         left-modules))
                  ""))
           (right (string-trim
                   (string-join
                    (-non-nil
                     (-map (lambda (m)
                             (eca-chat--mode-line-module session m))
                           right-modules))
                    "")))
           (fill (if (string-empty-p right)
                     ""
                   (propertize
                    " " 'display
                    `((space :align-to
                             (- right ,(1+ (length right)))))))))
      (concat left fill right))))

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
  (save-excursion
    (goto-char (eca-chat--content-insertion-point))
    (setq-local eca-chat--last-user-message-pos (point))))

(defun eca-chat--add-header (content)
  "Add CONTENT to the chat just after last user input."
  (when eca-chat--last-user-message-pos
    (save-excursion
      (goto-char eca-chat--last-user-message-pos)
      (eca-chat--insert content))))

(defun eca-chat--align-tables (&optional from)
  "Align all markdown tables in the chat content area.
When FROM is non-nil, scan from that position; otherwise scan from
the last user message."
  (eca-table-align (or from eca-chat--last-user-message-pos (point-min))
                   (eca-chat--prompt-area-start-point)))

(defun eca-chat--beautify-tables (&optional from)
  "Apply visual enhancements to markdown tables in the chat buffer.
When FROM is non-nil, scan from that position; otherwise scan from
the last user message.  Respects `eca-chat-table-beautify'."
  (eca-table-beautify (or from eca-chat--last-user-message-pos (point-min))
                      (eca-chat--prompt-area-start-point)))

(defun eca-chat--on-window-size-change (frame)
  "Debounced handler for window resize; re-evaluates table action bars.
FRAME is the resized frame."
  (dolist (win (window-list frame 'no-mini))
    (let ((buf (window-buffer win)))
      (when (and (buffer-live-p buf)
                 (eq (buffer-local-value 'major-mode buf) 'eca-chat-mode)
                 (buffer-local-value 'eca-chat-table-beautify buf))
        (with-current-buffer buf
          (when (timerp eca-chat--table-resize-timer)
            (cancel-timer eca-chat--table-resize-timer))
          (setq eca-chat--table-resize-timer
                (run-with-idle-timer
                 0.3 nil
                 (lambda (b)
                   (when (buffer-live-p b)
                     (with-current-buffer b
                       (eca-chat--beautify-tables (point-min))
                       ;; Reset truncation if no table wants it anymore
                       (unless (eca-table--any-truncated-p)
                         (setq-local truncate-lines nil)
                         (setq-local word-wrap t)))))
                 buf)))))))

(defun eca-chat--add-text-content (text &optional overlay-key overlay-value)
  "Add TEXT to the chat current position.
Add a overlay before with OVERLAY-KEY = OVERLAY-VALUE if passed."
  (save-excursion
    (goto-char (eca-chat--content-insertion-point))
    (when overlay-key
      (let ((ov (make-overlay (point) (point) (current-buffer))))
        (overlay-put ov overlay-key overlay-value)
        (when (eq overlay-key 'eca-chat--user-message-id)
          (overlay-put ov 'eca-chat--timestamp (float-time)))))
    (eca-chat--insert text)
    (point)))

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
          (if (and path (f-exists? path))
              (eca-buttonize
               eca-chat-mode-map
               (propertize (eca-chat--relativize-filename-for-workspace-root path roots)
                           'font-lock-face 'eca-chat-file-path-face)
               (lambda () (find-file-other-window path)))
            (or path "")) "\n"
          "```diff\n" (or diff "") "\n```"))

(defun eca-chat--file-change-details-label (details)
  "Build the label from DETAILS for a file change block."
  (let ((path (plist-get details :path))
        (added (plist-get details :linesAdded))
        (removed (plist-get details :linesRemoved)))
    (concat (propertize (if path (f-filename path) "") 'font-lock-face 'eca-chat-file-change-label-face)
            " "
            (propertize (concat "+" (number-to-string (or added 0))) 'font-lock-face 'success)
            " "
            (propertize (concat "-" (number-to-string (or removed 0))) 'font-lock-face 'error))))


(defun eca-chat--refresh-progress (chat-buffer)
  "Refresh the progress TEXT for CHAT-BUFFER."
  (when (buffer-live-p chat-buffer)
    (eca-chat--with-current-buffer chat-buffer
      (save-excursion
        (let ((ov (eca-chat--prompt-progress-field-ov)))
          (goto-char (overlay-start ov))
          (delete-region (point) (overlay-end ov)))
        (eca-chat--insert (propertize (if (string-empty-p eca-chat--progress-text)
                                          ""
                                          (concat "\n" eca-chat--progress-text))
                                      'font-lock-face 'eca-chat-system-messages-face)
                          eca-chat--spinner-string)))))

(defun eca-chat--go-to-overlay (ov-key range-min range-max first?)
  "Go to overlay finding from RANGE-MIN to RANGE-MAX if matches OV-KEY."
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (let ((get-fn (if first? #'-first #'-last)))
      (when-let ((ov (funcall get-fn (-lambda (ov) (overlay-get ov ov-key))
                              (overlays-in range-min range-max))))
        (goto-char (overlay-start ov))))))

(defun eca-chat--parse-unified-diff (diff-text)
  "Compatibility wrapper that delegates to `eca-diff-parse-unified-diff'.

DIFF-TEXT is the unified diff string to parse and resteps the parsed
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

(defun eca-chat--insert-prompt (text)
  "Insert TEXT to latest chat prompt point unless point is already in prompt."
  (save-excursion
    (if (eca-chat--point-at-prompt-field-p)
        (progn
          (when (and (eolp)
                     (= (line-beginning-position) (line-end-position)))
            (eca-chat--insert " "))
          (eca-chat--insert text))
      (goto-char (eca-chat--prompt-field-start-point))
      (goto-char (line-end-position))
      (when (= (line-beginning-position) (line-end-position))
        (eca-chat--insert " "))
      (eca-chat--insert text))))

(defmacro eca-chat-define-derived-mode (child name &optional docstring &rest body)
  "Wrapper for `define-derived-mode' with support for custom parent mode.
CHILD, NAME, DOCSTRING and BODY are passed down."
  (declare (indent defun))
  `(define-derived-mode ,child ,eca-chat-parent-mode ,name ,docstring ,@body))

;; Public

(eca-chat-define-derived-mode eca-chat-mode "eca-chat"
  "Major mode for ECA chat sessions.
\\{eca-chat-mode-map}"
  :group 'eca
  ;; Use word-wrap instead of visual-line-mode to preserve table formatting.
  ;; visual-line-mode wraps all lines including tables, breaking their layout.
  (setq-local word-wrap t)
  (setq-local truncate-lines nil)
  (hl-line-mode -1)
  (setq-local eca-chat--history '())
  (setq-local eca-chat--history-index -1)

  ;; Show diff blocks in markdown-mode with colors.
  (setq-local markdown-fontify-code-blocks-natively t)
  ;; Enable gfm-view-mode-like rendering without read-only
  (setq-local markdown-hide-markup t)
  (add-to-invisibility-spec 'markdown-markup)

  ;; markdown-mode declares keymap, help-echo, and mouse-face as
  ;; font-lock-extra-managed-props, which causes font-lock-ensure to
  ;; strip these properties from the entire buffer on every
  ;; refontification cycle.  ECA uses these properties on interactive
  ;; elements (approval buttons, expandable block labels, etc.) that
  ;; must survive font-lock.  Remove them so font-lock leaves our
  ;; interactive text properties intact.
  (setq-local font-lock-extra-managed-props
              (seq-difference font-lock-extra-managed-props
                              '(keymap help-echo mouse-face)))

  (make-local-variable 'completion-at-point-functions)
  (setq-local completion-at-point-functions (list #'eca-chat-completion-at-point))

  (make-local-variable 'company-box-icons-functions)
  (when (featurep 'company-box)
    (add-to-list 'company-box-icons-functions #'eca-chat--completion-item-company-box-icon))

  (let ((session (or eca--chat-init-session (eca-session))))
    (when session
      (setq-local eca--session-id-cache (eca--session-id session)))
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
    (advice-add 'delete-backward-char :around #'eca-chat--key-pressed-deletion)
    (advice-add 'backward-delete-char :around #'eca-chat--key-pressed-deletion)
    (advice-add 'backward-delete-char-untabify :around #'eca-chat--key-pressed-deletion)
    (advice-add 'backward-kill-word :around #'eca-chat--key-pressed-deletion)
    (when (featurep 'evil)
      (advice-add 'evil-delete-backward-word :around #'eca-chat--key-pressed-deletion)
      (advice-add 'evil-delete-back-to-indentation :around #'eca-chat--key-pressed-deletion)
      (advice-add 'evil-delete-whole-line :around #'eca-chat--key-pressed-deletion)
      (advice-add 'evil-delete-char :around #'eca-chat--key-pressed-deletion)
      (advice-add 'evil-delete :around #'eca-chat--key-pressed-deletion)
      (advice-add 'evil-delete-backward-char :around #'eca-chat--key-pressed-deletion))

    (add-hook 'eldoc-documentation-functions #'eca-chat-eldoc-function nil t)
    (eldoc-mode 1)

    (add-hook 'kill-buffer-hook #'eca-chat--delete-chat nil t)

    ;; Paste image from clipboard support
    (when (fboundp 'yank-media-handler)
      ;; reset current handlers inherit from markdown
      (setq-local yank-media--registered-handlers nil)
      (yank-media-handler "image/png" #'eca-chat--yank-image-handler)
      (yank-media-handler "image/jpeg" #'eca-chat--yank-image-handler)
      (yank-media-handler "image/jpg" #'eca-chat--yank-image-handler)
      (yank-media-handler "image/gif" #'eca-chat--yank-image-handler)
      (yank-media-handler "image/webp" #'eca-chat--yank-image-handler)
      (advice-add 'yank :around #'eca-chat--yank-considering-image)
      (when (featurep 'evil)
        (advice-add 'evil-paste-after :around #'eca-chat--yank-considering-image)
        (advice-add 'evil-paste-before :around #'eca-chat--yank-considering-image)))

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
           (setq-local eca-chat--server-version
                        (eca-process--get-current-server-version))
           (setq-local mode-line-format
                        (if (functionp eca-chat-mode-line-format)
                            (funcall eca-chat-mode-line-format session)
                          `(t (:eval (eca-chat--mode-line-string ,session)))))

           ;; Tab-line: show a tab for each open chat
           (when eca-chat-tab-line
             (setq-local tab-line-tabs-function #'eca-chat--tab-line-tabs)
             (setq-local tab-line-new-button-show t)
             (setq-local tab-line-close-button-show t)
             (setq-local tab-line-new-tab-function #'eca-chat-new)
             (setq-local tab-line-separator "")
             (setq-local tab-line-tab-face-functions '(eca-chat--tab-line-face))
             (face-remap-add-relative 'tab-line :height 0.9)
             (let ((fallback-bg (face-background 'default nil t)))
               (face-remap-add-relative 'tab-line-tab :box `(:line-width 4 :color ,(or (face-background 'tab-line-tab nil t) fallback-bg)))
               (face-remap-add-relative 'tab-line-tab-current :box `(:line-width 4 :color ,(or (face-background 'tab-line-tab-current nil t) fallback-bg)))
               (face-remap-add-relative 'tab-line-tab-inactive :box `(:line-width 4 :color ,(or (face-background 'tab-line-tab-inactive nil t) fallback-bg)))
)
             ;; Use text × instead of XPM image so it inherits the tab background
             (setq-local tab-line-close-button
                         (propertize " × "
                                     'keymap eca-chat--tab-close-map
                                     'mouse-face 'tab-line-close-highlight
                                     'help-echo "Click to close tab"))
             (tab-line-mode 1))

           ;; Keep session last-chat-buffer in sync with the displayed chat
           (add-hook 'post-command-hook #'eca-chat--sync-last-buffer nil t)

           (force-mode-line-update)
           (run-hooks 'eca-chat-mode-hook))))))

  (face-remap-add-relative 'markdown-line-break-face
                           '(:underline nil))

  ;; Ensure tables use a monospace font for proper alignment.
  (face-remap-add-relative 'markdown-table-face
                           '(:inherit fixed-pitch))

  ;; Compute expandable-block background faces from current theme and
  ;; keep them in sync when the user switches themes.
  (eca-chat--update-expandable-block-faces)
  (eca-table-update-faces)
  (add-hook 'enable-theme-functions
            (lambda (&rest _)
              (eca-chat--update-expandable-block-faces)
              (eca-table-update-faces))
            nil t)

  ;; Re-evaluate table action bars when window is resized.
  (add-hook 'window-size-change-functions
            #'eca-chat--on-window-size-change)

  (goto-char (point-max)))

(defun eca-chat--task-find-by-id (id)
  "Find a task by ID in the current task state.
Returns the task plist or nil."
  (when-let* ((tasks (append (plist-get eca-chat--task-state :tasks) nil)))
    (-first (lambda (task) (equal id (plist-get task :id))) tasks)))



(defun eca-chat-title ()
  "Return the chat title."
  (cond
   (eca-chat--custom-title
    (propertize eca-chat--custom-title 'font-lock-face 'eca-chat-title-face))
   (eca-chat--title
    (propertize eca-chat--title 'font-lock-face 'eca-chat-title-face))
   (t "Empty chat")))

(defun eca-chat--handle-mcp-server-updated (session _server)
  "Handle mcp SERVER updated for SESSION."
  ;; TODO do for all chats
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
    (force-mode-line-update)))

(defun eca-chat--set-agent (session new-agent &optional buffer)
  "Set new agent to NEW-AGENT notifying server for SESSION.
When BUFFER is provided, set the agent in that buffer instead of
the last chat buffer of SESSION."
  (eca-chat--with-current-buffer (or buffer (eca-chat--get-last-buffer session))
    (setq-local eca-chat--selected-agent new-agent)
    (setq eca-chat--last-known-agent new-agent))
  (eca-api-notify session
                  :method "chat/selectedAgentChanged"
                  :params (list :agent new-agent)))

(defun eca-chat--set-trust (session value &optional buffer)
  "Set trust mode to VALUE for SESSION.
When BUFFER is provided, set in that buffer instead of
the last chat buffer of SESSION."
  (eca-chat--with-current-buffer (or buffer (eca-chat--get-last-buffer session))
    (setq-local eca-chat--selected-trust value)
    (force-mode-line-update)))

(defun eca-chat--tool-call-file-change-details
    (content label approval-text time status tool-call-next-line-spacing roots &optional parent-id)
  "Update tool call UI based showing file change details.
LABEL is the tool call label.
CONTENT is the tool call content.
Can include optional APPROVAL-TEXT and TIME.
Append STATUS, TOOL-CALL-NEXT-LINE-SPACING, ROOTS and optional PARENT-ID."
  (-let* (((&plist :name name :details details :id id) content)
          (path (plist-get details :path))
          (diff (plist-get details :diff))
          (view-diff-btn
           (when (and path diff)
             (eca-buttonize
              eca-chat-mode-map
              (propertize "view diff" 'font-lock-face 'eca-chat-diff-view-face)
              (lambda ()
                (eca-chat--show-diff path diff))))))
    (eca-chat--update-expandable-content
     id
     (concat (propertize label 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
             " " (eca-chat--file-change-details-label details)
             " " status time
             (when view-diff-btn
               (concat "\n" (propertize view-diff-btn 'line-prefix tool-call-next-line-spacing)))
             approval-text)
     (concat "Tool: `" name "`\n"
             (eca-chat--file-change-diff path diff roots))
     nil
     parent-id)))

(defun eca-chat--tool-call-json-outputs-details (content time status &optional parent-id)
  "Update tool call UI for json output given CONTENT, TIME, STATUS and PARENT-ID."
  (-let* (((&plist :name name :arguments arguments :server server :details details :id
                   id :summary summary) content)
          (jsons (plist-get details :jsons))
          (label (or summary (format "Called tool: %s__%s" server name))))
    (eca-chat--update-expandable-content
     id
     (concat (propertize label 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
             " " status time)
     (eca-chat--content-table
      `(("Tool"   . ,name)
        ("Server" . ,server)
        ("Arguments" . ,arguments)
        ("Json output" . ,(concat "\n"
                                  "```javascript\n"
                                  (string-join jsons "\n")
                                  "\n```"))))
     nil
     parent-id)))

(defun eca-chat--tool-call-subagent-details (id args label approval-text time status parent-id details &optional output-text)
  "Update tool call UI for a subagent tool call.
ID and ARGS are from the tool call content.
LABEL is the expandable block label.
DETAILS is the details of the tool call.
Can include optional APPROVAL-TEXT and TIME.
Append STATUS symbol.  Optional PARENT-ID for nested rendering."
  (-let* ((agent-name (plist-get args :agent))
          (task (plist-get args :task))
          (model (plist-get details :model))
          (step (plist-get details :step))
          (max-steps (plist-get details :maxSteps))
          (usage-str (eca-chat--subagent-usage-str id))
          (steps-info (eca-chat--subagent-steps-info step max-steps usage-str))
          (existing-ov (eca-chat--get-expandable-content id))
          ;; Preserve pending-approval status when a step update arrives with
          ;; loading status — an inner tool call may be waiting for approval.
          (status (if (and existing-ov
                          (string= status eca-chat-mcp-tool-call-loading-symbol)
                          (string= (overlay-get existing-ov 'eca-chat--tool-call-status)
                                   eca-chat-mcp-tool-call-pending-approval-symbol))
                     eca-chat-mcp-tool-call-pending-approval-symbol
                   status))
          (new-label (concat (propertize label 'font-lock-face 'eca-chat-subagent-tool-call-label-face)
                             steps-info " " status time
                             (when approval-text (concat "\n" approval-text))))
          (has-children? (and existing-ov
                              (eca-chat--segments-children
                               (overlay-get existing-ov 'eca-chat--expandable-content-segments)))))
    (if has-children?
        ;; Block already has nested child content (subagent tool calls, reasoning,
        ;; approval prompts, etc).  Only update the label line to reflect the new
        ;; step count / status, preserving all rendered children.
        (let* ((ov-content (overlay-get existing-ov 'eca-chat--expandable-content-ov-content))
               (open? (overlay-get existing-ov 'eca-chat--expandable-content-toggle))
               (content (overlay-get ov-content 'eca-chat--expandable-content-content))
               (has-content? (or (and content (not (string-empty-p content)))
                                 has-children?))
               (new-icon-face (get-text-property 0 'font-lock-face new-label))
               (label-indent (when (overlay-get existing-ov 'eca-chat--expandable-content-nested)
                               eca-chat--expandable-content-base-indent))
               (new-icons (eca-chat--make-expandable-icons new-icon-face label-indent)))
          (overlay-put existing-ov 'eca-chat--expandable-content-open-icon (car new-icons))
          (overlay-put existing-ov 'eca-chat--expandable-content-close-icon (cdr new-icons))
          (save-excursion
            (goto-char (overlay-start existing-ov))
            (delete-region (point) (1- (overlay-start ov-content)))
            (eca-chat--insert
             (propertize (eca-chat--propertize-only-first-word
                          new-label
                          'line-prefix (when has-content?
                                         (if open?
                                             (cdr new-icons)
                                           (car new-icons))))
                         'help-echo "mouse-1 / RET / tab: expand/collapse"))
            (eca-chat--paint-nested-label existing-ov)))
      ;; No children yet — safe to replace the full content body
      (eca-chat--update-expandable-content
       id
       new-label
       (eca-chat--content-table
        `(("Agent" . ,agent-name)
          ("Model" . ,model)
          ,@(when task `(("Task" . ,(concat task "\n\n"))))
          ,@(when output-text `(("Output" . ,(concat "\n" output-text))))))
       nil
       parent-id))
    ;; Store status and label on the overlay so we can update them later
    (when-let* ((ov (eca-chat--get-expandable-content id)))
      (overlay-put ov 'eca-chat--tool-call-status status)
      (overlay-put ov 'eca-chat--tool-call-label label)
      (overlay-put ov 'eca-chat--tool-call-steps-info steps-info)
      (overlay-put ov 'eca-chat--tool-call-step step)
      (overlay-put ov 'eca-chat--tool-call-max-steps max-steps)
      (overlay-put ov 'eca-chat--tool-call-time time))))

(defun eca-chat--refresh-subagent-usage-label (tool-call-id)
  "Refresh the label of subagent TOOL-CALL-ID to reflect latest usage data.
Rebuilds the steps-info string with current usage and updates the overlay."
  (when-let* ((ov-label (eca-chat--get-expandable-content tool-call-id))
              (label (overlay-get ov-label 'eca-chat--tool-call-label))
              (status (or (overlay-get ov-label 'eca-chat--tool-call-status) ""))
              (time (or (overlay-get ov-label 'eca-chat--tool-call-time) ""))
              (ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content)))
    (let* ((step (overlay-get ov-label 'eca-chat--tool-call-step))
           (max-steps (overlay-get ov-label 'eca-chat--tool-call-max-steps))
           (usage-str (eca-chat--subagent-usage-str tool-call-id))
           (steps-info (eca-chat--subagent-steps-info step max-steps usage-str))
           (new-label (concat (propertize label 'font-lock-face 'eca-chat-subagent-tool-call-label-face)
                              steps-info " " status time))
           (open? (overlay-get ov-label 'eca-chat--expandable-content-toggle))
           (content (overlay-get ov-content 'eca-chat--expandable-content-content))
           (has-content? (and content (not (string-empty-p content)))))
      (overlay-put ov-label 'eca-chat--tool-call-steps-info steps-info)
      (save-excursion
        (goto-char (overlay-start ov-label))
        (delete-region (point) (1- (overlay-start ov-content)))
        (eca-chat--insert
         (propertize (eca-chat--propertize-only-first-word
                      new-label
                      'line-prefix (when has-content?
                                     (if open?
                                         (overlay-get ov-label 'eca-chat--expandable-content-close-icon)
                                       (overlay-get ov-label 'eca-chat--expandable-content-open-icon))))
                     'help-echo "mouse-1 / RET / tab: expand/collapse"))))))

(defun eca-chat--update-parent-subagent-status (parent-tool-call-id new-status)
  "Update to NEW-STATUS symbol of a parent subagent tool call PARENT-TOOL-CALL-ID.
Only updates the label line, preserving all nested child content."
  (when-let* ((ov-label (eca-chat--get-expandable-content parent-tool-call-id))
              (label (overlay-get ov-label 'eca-chat--tool-call-label))
              (time (or (overlay-get ov-label 'eca-chat--tool-call-time) ""))
              (ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content)))
    (overlay-put ov-label 'eca-chat--tool-call-status new-status)
    (let* ((steps-info (or (overlay-get ov-label 'eca-chat--tool-call-steps-info) ""))
           (new-label (concat (propertize label 'font-lock-face 'eca-chat-subagent-tool-call-label-face)
                              steps-info " " new-status time))
           (open? (overlay-get ov-label 'eca-chat--expandable-content-toggle))
           (content (overlay-get ov-content 'eca-chat--expandable-content-content))
           (has-content? (and content (not (string-empty-p content)))))
      (save-excursion
        (goto-char (overlay-start ov-label))
        (delete-region (point) (1- (overlay-start ov-content)))
        (eca-chat--insert
         (propertize (eca-chat--propertize-only-first-word
                      new-label
                      'line-prefix (when has-content?
                                     (if open?
                                         (overlay-get ov-label 'eca-chat--expandable-content-close-icon)
                                       (overlay-get ov-label 'eca-chat--expandable-content-open-icon))))
                     'help-echo "mouse-1 / RET / tab: expand/collapse"))))))

(defun eca-chat--task-tool-call-p (content)
  "Return non-nil if CONTENT represents an eca__task tool call."
  (and (string= "eca" (plist-get content :server))
       (string= "task" (plist-get content :name))))

(defun eca-chat--task-format-task (task)
  "Format a single TASK as a checkbox line string."
  (let* ((status (plist-get task :status))
         (subject (plist-get task :subject))
         (done (string= status "done"))
         (in-progress (string= status "in-progress")))
    (cond
     (done
      (propertize (format "- [x] %s" subject)
                  'font-lock-face 'eca-chat-task-done-face
                  'eca-chat-task task))
     (in-progress
      (propertize (format "- [ ] %s" subject)
                  'font-lock-face 'eca-chat-task-in-progress-face
                  'eca-chat-task task))
     (t
      (propertize (format "- [ ] %s" subject)
                  'eca-chat-task task)))))

(defun eca-chat--task-build-content (tasks)
  "Build the expandable block content string from TASKS list."
  (mapconcat #'eca-chat--task-format-task tasks "\n"))

(defun eca-chat--update-task-state (content)
  "Extract task state from tool-call CONTENT details and update the task area.
Uses the expandable block system to render the task widget.
The server sends a :details plist with :type \"task\", :activeSummary, :tasks,
:inProgressTaskIds, and :summary."
  (when-let* ((details (plist-get content :details)))
    (setq-local eca-chat--task-state details)
    (let* ((tasks (append (plist-get details :tasks) nil)))
      (if (null tasks)
          ;; Task list was cleared — remove the expandable block
          (eca-chat--remove-expandable-content eca-chat--task-block-id)
        (let* ((active-summary (plist-get details :activeSummary))
               (done-count (length (-filter (lambda (task) (string= "done" (plist-get task :status))) tasks)))
               (total-count (length tasks))
               (in-progress-task (-first (lambda (task) (string= "in-progress" (plist-get task :status))) tasks))
               (label-text (or active-summary
                               (when in-progress-task (plist-get in-progress-task :subject))
                               ""))
               (prefix-text (if active-summary "Task: " "Tasks "))
               (progress-text (format " (%d/%d)" done-count total-count))
               (label-face (if in-progress-task 'eca-chat-task-label-in-progress-face 'eca-chat-task-label-face))
               (label (concat
                       (propertize prefix-text 'font-lock-face 'eca-chat-task-prefix-face)
                       (propertize label-text 'font-lock-face label-face)
                       (propertize progress-text 'font-lock-face 'eca-chat-task-progress-face)))
               (body (eca-chat--task-build-content tasks)))
          (if (eca-chat--get-expandable-content eca-chat--task-block-id)
              (eca-chat--update-expandable-content eca-chat--task-block-id label body)
            (eca-chat--add-expandable-content eca-chat--task-block-id label body nil
                                              (overlay-start (eca-chat--task-area-ov)))))))))

(defun eca-chat--render-content (session chat-buffer role content roots &optional parent-tool-call-id chat-id)
  "Render CONTENT inside CHAT-BUFFER for SESSION.
ROLE is the message role.  ROOTS is the list of workspace roots.
When PARENT-TOOL-CALL-ID is non-nil, renders as nested content inside
that expandable block (subagent mode).
CHAT-ID is the chat session the content belongs to, used for tool call
approval requests.  Falls back to the buffer-local `eca-chat--id'.
Must be called with `eca-chat--with-current-buffer' or equivalent."
  (let* ((content-id (plist-get content :contentId))
         (tool-call-next-line-spacing (make-string (1+ (length eca-chat-expandable-block-open-symbol)) ?\s)))
    (pcase (plist-get content :type)
      ("metadata"
       (unless parent-tool-call-id
         (setq-local eca-chat--title (plist-get content :title))))
      ("text"
       (when-let* ((text (plist-get content :text)))
         (pcase role
           ("user"
            (unless parent-tool-call-id
              (progn
                (eca-chat--add-expandable-content
                 content-id
                 (propertize (string-trim text) 'font-lock-face 'eca-chat-user-messages-face)
                 (eca-buttonize
                  eca-chat-mode-map
                  (propertize "Rollback chat to before this message" 'font-lock-face 'eca-chat-rollback-face)
                  (lambda () (eca-chat--rollback session content-id))))
                (when-let* ((ov (eca-chat--get-expandable-content content-id)))
                  (overlay-put ov 'eca-chat--user-message-id content-id)
                  (overlay-put ov 'eca-chat--timestamp (float-time)))
                (eca-chat--mark-header)
                (font-lock-ensure))))
           ("system"
            (eca-chat--add-text-content
             (propertize text
                         'font-lock-face 'eca-chat-system-messages-face
                         'line-height 20)))
           (_
            (if parent-tool-call-id
                ;; Subagent: append assistant text to the parent tool call content
                (eca-chat--update-expandable-content
                 parent-tool-call-id nil text t)
              (eca-chat--add-text-content text)
              (font-lock-ensure))))))
      ("url"
       (unless parent-tool-call-id
         (eca-chat--add-header
          (concat "🌐 "
                  (eca-buttonize
                   eca-chat-mode-map
                   (plist-get content :title)
                   (lambda () (browse-url (plist-get content :url))))
                  "\n\n"))))
      ("reasonStarted"
       (let ((id (plist-get content :id))
             (label (propertize "Thinking..." 'font-lock-face 'eca-chat-reason-label-face)))
         (eca-chat--add-expandable-content id label "" parent-tool-call-id)))
      ("reasonText"
       (let ((id (plist-get content :id))
             (label (propertize "Thinking..." 'font-lock-face 'eca-chat-reason-label-face))
             (text (plist-get content :text)))
         (eca-chat--update-expandable-content id label text t parent-tool-call-id)))
      ("reasonFinished"
       (let* ((id (plist-get content :id))
              (base (propertize "Thought" 'font-lock-face 'eca-chat-reason-label-face))
              (time (when-let ((ms (plist-get content :totalTimeMs)))
                      (concat " " (eca-chat--time->presentable-time ms))))
              (label (concat base time)))
         (eca-chat--update-expandable-content id label "" t parent-tool-call-id)))
      ("hookActionStarted"
       (let* ((id (plist-get content :id))
              (name (plist-get content :name))
              (label (propertize (format "Running hook '%s'..." name) 'font-lock-face 'eca-chat-hook-label-face)))
         (eca-chat--add-expandable-content id label "" parent-tool-call-id)))
      ("hookActionFinished"
       (let* ((id (plist-get content :id))
              (name (plist-get content :name))
              (status (number-to-string (plist-get content :status)))
              (output (plist-get content :output))
              (error (plist-get content :error))
              (label (propertize (format "Executed hook '%s'" name) 'font-lock-face 'eca-chat-hook-label-face)))
         (eca-chat--update-expandable-content id label (eca-chat--content-table
                                                        (append
                                                         `(("Name" . ,name)
                                                           ("Status" . ,status))
                                                         (when output `(("Output" . ,output)))
                                                         (when error `(("Error" . ,error)))))
                                              nil parent-tool-call-id)))
      ("toolCallPrepare"
       (if (eca-chat--task-tool-call-p content)
           (unless (eca-chat--get-expandable-content eca-chat--task-block-id)
             (let ((label (concat
                           (propertize "Creating tasks... " 'font-lock-face 'eca-chat-task-prefix-face)
                           eca-chat-mcp-tool-call-loading-symbol)))
               (eca-chat--add-expandable-content
                eca-chat--task-block-id label "" nil
                (overlay-start (eca-chat--task-area-ov)))))
         (let* ((id (plist-get content :id))
                (name (plist-get content :name))
                (server (plist-get content :server))
                (argsText (plist-get content :argumentsText))
                (details (plist-get content :details))
                (subagent? (string= "subagent" (plist-get details :type)))
                (label (or (plist-get content :summary)
                           (format "Preparing tool: %s__%s" server name)))
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
             (let* ((label-face (if subagent?
                                    'eca-chat-subagent-tool-call-label-face
                                  'eca-chat-mcp-tool-call-label-face))
                    (label (concat (propertize label 'font-lock-face label-face)
                                   " " eca-chat-mcp-tool-call-loading-symbol))
                    (body (if subagent?
                              (eca-chat--content-table `())
                            (eca-chat--content-table
                             `(("Tool" . ,name)
                               ("Server" . ,server)
                               ("Arguments" . ,new-content))))))
               (if (eca-chat--get-expandable-content id)
                   ;; Update with accumulated content, not just this chunk
                   (eca-chat--update-expandable-content
                    id label (if subagent? body new-content) nil parent-tool-call-id)
                 (eca-chat--add-expandable-content
                  id label body parent-tool-call-id)))))))
      ("toolCallRun"
       (unless (eca-chat--task-tool-call-p content)
         (let* ((id (plist-get content :id))
                (args (plist-get content :arguments))
                (name (plist-get content :name))
                (server (plist-get content :server))
                (label (or (plist-get content :summary)
                           (format "Calling tool: %s__%s" server name)))
                (manual? (plist-get content :manualApproval))
                (status (if manual?
                            eca-chat-mcp-tool-call-pending-approval-symbol
                          eca-chat-mcp-tool-call-loading-symbol))
                (approval-text (when manual?
                                 (eca-chat--build-tool-call-approval-str-content session id tool-call-next-line-spacing chat-id)))
                (details (plist-get content :details)))
           ;; Register subagent mapping only for top-level tool calls
           (when (and (not parent-tool-call-id)
                      (string= "subagent" (plist-get details :type)))
             (puthash (plist-get details :subagentChatId) id eca-chat--subagent-chat-id->tool-call-id))
           (pcase (plist-get details :type)
             ("fileChange" (eca-chat--tool-call-file-change-details content label approval-text nil status tool-call-next-line-spacing roots parent-tool-call-id))
             ("subagent" (eca-chat--tool-call-subagent-details id args label approval-text nil status parent-tool-call-id details))
             (_ (eca-chat--update-expandable-content
                 id
                 (concat (propertize label 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                         " " status
                         "\n"
                         approval-text)
                 (eca-chat--content-table
                  `(("Tool" . ,name)
                    ("Server" . ,server)
                    ("Arguments" . ,args)))
                 nil
                 parent-tool-call-id)))
           ;; Mark this ID as having received toolCallRun so that any late-arriving
           ;; toolCallPrepare events (still in-flight for long files) don't overwrite
           ;; the approval prompt we just rendered.  Set this AFTER the pcase dispatch
           ;; so that if rendering errors, the flag doesn't poison subsequent prepare
           ;; events (which would prevent the block from ever being created).
           (when (and eca-chat-expand-pending-approval-tools manual?)
             (when parent-tool-call-id
               (eca-chat--expandable-content-toggle parent-tool-call-id t nil))
             (eca-chat--expandable-content-toggle id t nil)
             (eca-chat--ensure-prompt-visible))
           ;; Update parent subagent status to show pending approval
           (when (and manual? parent-tool-call-id)
             (eca-chat--update-parent-subagent-status
              parent-tool-call-id eca-chat-mcp-tool-call-pending-approval-symbol)))))
      ("toolCallRunning"
       (unless (eca-chat--task-tool-call-p content)
         (let* ((id (plist-get content :id))
                (args (plist-get content :arguments))
                (name (plist-get content :name))
                (server (plist-get content :server))
                (label (or (plist-get content :summary)
                           (format "Running tool: %s__%s" server name)))
                (details (plist-get content :details))
                (status eca-chat-mcp-tool-call-loading-symbol)
                (elapsed-time (progn
                                (eca-chat--tool-call-elapsed-start id)
                                (eca-chat--elapsed-time-string
                                 (gethash id eca-chat--tool-call-elapsed-times)))))
           ;; Register subagent mapping only for top-level tool calls
           (when (and (not parent-tool-call-id)
                      (string= "subagent" (plist-get details :type)))
             (let ((subagent-chat-id (plist-get details :subagentChatId)))
               (unless (gethash subagent-chat-id eca-chat--subagent-chat-id->tool-call-id)
                 (puthash subagent-chat-id id eca-chat--subagent-chat-id->tool-call-id))))
           (pcase (plist-get details :type)
             ("fileChange" (eca-chat--tool-call-file-change-details content label nil elapsed-time status tool-call-next-line-spacing roots parent-tool-call-id))
             ("subagent" (eca-chat--tool-call-subagent-details id args label nil elapsed-time status parent-tool-call-id details))
             (_ (eca-chat--update-expandable-content
                 id
                 (concat (propertize label 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                         " " status elapsed-time)
                 (eca-chat--content-table
                  `(("Tool" . ,name)
                    ("Server" . ,server)
                    ("Arguments" . ,args)))
                 nil
                 parent-tool-call-id)))
           ;; Restore parent subagent status back to loading
           (when parent-tool-call-id
             (eca-chat--update-parent-subagent-status
              parent-tool-call-id eca-chat-mcp-tool-call-loading-symbol)))))
      ("toolCalled"
       (if (eca-chat--task-tool-call-p content)
           (eca-chat--update-task-state content)
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
           ;; Stop elapsed-time tracking for this tool call
           (eca-chat--tool-call-elapsed-stop id)
           ;; Cleanup subagent mapping only for top-level tool calls
           (when (and (not parent-tool-call-id)
                      (string= "subagent" (plist-get details :type)))
             (remhash (plist-get details :subagentChatId) eca-chat--subagent-chat-id->tool-call-id))
           (pcase (plist-get details :type)
             ("fileChange" (eca-chat--tool-call-file-change-details content label nil time status tool-call-next-line-spacing roots parent-tool-call-id))
             ("jsonOutputs" (eca-chat--tool-call-json-outputs-details content time status parent-tool-call-id))
             ("subagent" (eca-chat--tool-call-subagent-details id args label nil time status parent-tool-call-id details output-text))
             (_ (eca-chat--update-expandable-content
                 id
                 (concat (propertize label 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                         " " status time)
                 (eca-chat--content-table
                  `(("Tool"   . ,name)
                    ("Server" . ,server)
                    ("Arguments" . ,args)
                    ("Output" . ,output-text)))
                 nil
                 parent-tool-call-id)))
           (when eca-chat-shrink-called-tools
             (eca-chat--expandable-content-toggle id t t)
             (eca-chat--ensure-prompt-visible))
           ;; Restore parent subagent status back to loading
           (when parent-tool-call-id
             (eca-chat--update-parent-subagent-status
              parent-tool-call-id eca-chat-mcp-tool-call-loading-symbol)))))
      ("toolCallRejected"
       (unless (eca-chat--task-tool-call-p content)
         (let* ((name (plist-get content :name))
                (server (plist-get content :server))
                (label (or (plist-get content :summary)
                           (format "Rejected tool: %s__%s" server name)))
                (args (plist-get content :arguments))
                (details (plist-get content :details))
                (status eca-chat-mcp-tool-call-error-symbol)
                (id (plist-get content :id)))
           ;; Cleanup counters for this tool-call id
           (remhash id eca-chat--tool-call-prepare-counters)
           (remhash id eca-chat--tool-call-prepare-content-cache)
           ;; Cleanup subagent mapping only for top-level tool calls
           (when (and (not parent-tool-call-id)
                      (string= "subagent" (plist-get details :type)))
             (remhash (plist-get details :subagentChatId) eca-chat--subagent-chat-id->tool-call-id))
           (pcase (plist-get details :type)
             ("fileChange" (eca-chat--tool-call-file-change-details content label nil nil status tool-call-next-line-spacing roots parent-tool-call-id))
             ("subagent" (eca-chat--tool-call-subagent-details id args label nil nil status parent-tool-call-id details))
             (_ (eca-chat--update-expandable-content
                 id
                 (concat (propertize label
                                     'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                         " "
                         eca-chat-mcp-tool-call-error-symbol)
                 (eca-chat--content-table `(("Tool" . ,name)
                                            ("Server" . ,server)
                                            ("Arguments" . ,args)))
                 nil
                 parent-tool-call-id)))
           ;; Restore parent subagent status back to loading
           (when parent-tool-call-id
             (eca-chat--update-parent-subagent-status
              parent-tool-call-id eca-chat-mcp-tool-call-loading-symbol)))))
      ("progress"
       (unless parent-tool-call-id
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
            (eca-chat--tool-call-elapsed-stop-all)
            (eca-chat--add-text-content "\n")
            (eca-chat--align-tables (point-min))
            (eca-chat--beautify-tables (point-min))
            (eca-chat--set-chat-loading session nil)
            (eca-chat--refresh-progress chat-buffer)
            (eca-chat--send-queued-prompt session)
            (run-hooks 'eca-chat-finished-hook)))))
      ("usage"
       (progn
         (if parent-tool-call-id
             ;; Subagent usage — store and refresh the tool call label
             (let ((session-tokens (plist-get content :sessionTokens))
                   (context-limit (plist-get (plist-get content :limit) :context)))
               (puthash parent-tool-call-id
                        (list :session-tokens session-tokens :context-limit context-limit)
                        eca-chat--subagent-usage)
               (eca-chat--refresh-subagent-usage-label parent-tool-call-id))
           (setq-local eca-chat--message-input-tokens  (plist-get content :messageInputTokens))
           (setq-local eca-chat--message-output-tokens (plist-get content :messageOutputTokens))
           (setq-local eca-chat--session-tokens        (plist-get content :sessionTokens))
           (setq-local eca-chat--session-limit-context (plist-get (plist-get content :limit) :context))
           (setq-local eca-chat--session-limit-output  (plist-get (plist-get content :limit) :output))
           (setq-local eca-chat--message-cost          (plist-get content :messageCost))
           (setq-local eca-chat--session-cost          (plist-get content :sessionCost)))
         (force-mode-line-update)))
      (_ nil))))

(defun eca-chat-content-received (session params)
  "Handle the content received notification with PARAMS for SESSION."
  (let* ((chat-id (plist-get params :chatId))
         (parent-chat-id (plist-get params :parentChatId))
         (role (plist-get params :role))
         (content (plist-get params :content))
         (roots (eca--session-workspace-folders session)))
    (if parent-chat-id
        ;; Subagent content → route to parent chat buffer, nested under tool call
        (when-let* ((parent-buffer (eca-get (eca--session-chats session) parent-chat-id))
                    ((buffer-live-p parent-buffer)))
          (eca-chat--with-current-buffer parent-buffer
            (when-let* ((tool-call-id (gethash chat-id eca-chat--subagent-chat-id->tool-call-id)))
              (eca-chat--render-content session parent-buffer role content roots tool-call-id chat-id))))
      ;; Normal content
      (let ((chat-buffer (eca-chat--get-chat-buffer session chat-id)))
        (eca-chat--with-current-buffer chat-buffer
          (eca-chat--render-content session chat-buffer role content roots))))))

(defun eca-chat-cleared (session params)
  "Clear chat for SESSION and PARAMS requested by server."
  (-let* ((chat-id (plist-get params :chatId))
          (messages? (plist-get params :messages))
          (chat-buffer (eca-chat--get-chat-buffer session chat-id)))
    (eca-chat--with-current-buffer chat-buffer
      (when messages?
        (eca-chat--clear)))))

(defun eca-chat-config-updated (session chat-config)
  "Update chat based on the CHAT-CONFIG for SESSION."
  (-some->> (plist-get chat-config :welcomeMessage)
    (setf (eca--session-chat-welcome-message session)))
  (-some->> (plist-get chat-config :models)
    (setf (eca--session-models session)))
  (-some->> (plist-get chat-config :agents)
    (setf (eca--session-chat-agents session)))
  (when (plist-member chat-config :variants)
    (setf (eca--session-chat-variants session)
          (append (plist-get chat-config :variants) nil)))
  (seq-doseq (chat-buffers (eca-vals (eca--session-chats session)))
    (with-current-buffer chat-buffers
      (when-let* ((new-model (plist-get chat-config :selectModel)))
        (setq-local eca-chat--selected-model new-model)
        (setq eca-chat--last-known-model new-model))
      (when-let* ((new-agent (plist-get chat-config :selectAgent)))
        (setq-local eca-chat--selected-agent new-agent)
        (setq eca-chat--last-known-agent new-agent))
      (when (plist-member chat-config :selectVariant)
        (let ((new-variant (plist-get chat-config :selectVariant)))
          (setq-local eca-chat--selected-variant new-variant)
          (setq eca-chat--last-known-variant new-variant)))
      (force-mode-line-update))))

(defun eca-chat-open (session)
  "Open or create dedicated eca chat window for SESSION."
  (eca-assert-session-running session)
  (unless (buffer-live-p (eca-chat--get-last-buffer session))
    (eca-chat--create-buffer session))
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
    (unless (derived-mode-p 'eca-chat-mode)
      (let ((eca--chat-init-session session))
        (eca-chat-mode))
      (setq-local eca-chat--selected-agent eca-chat--last-known-agent)
      (setq-local eca-chat--selected-model eca-chat--last-known-model)
      (setq-local eca-chat--selected-variant eca-chat--last-known-variant)
      (eca-chat--track-cursor-position-schedule)
      (when eca-chat-auto-add-cursor
        (eca-chat--add-context (list :type "cursor")))
      (when eca-chat-auto-add-repomap
        (eca-chat--add-context (list :type "repoMap"))))
    (unless (member (current-buffer) (eca-vals (eca--session-chats session)))
      (setf (eca--session-chats session) (eca-assoc (eca--session-chats session) 'empty (current-buffer))))
    (if (window-live-p (get-buffer-window (buffer-name)))
        (eca-chat--select-window)
      (eca-chat--pop-window))
    (unless (eca--session-last-chat-buffer session)
      (setf (eca--session-last-chat-buffer session) (current-buffer))))
  (eca-chat--track-cursor))

(defun eca-chat-exit (session)
  "Exit the ECA chat for SESSION."
  ;; Cancel the global repeating idle timer that tracks cursor position.
  (when (timerp eca-chat--cursor-context-timer)
    (cancel-timer eca-chat--cursor-context-timer)
    (setq eca-chat--cursor-context-timer nil))
  ;; Remove the global window-size-change handler registered by eca-chat-mode.
  (remove-hook 'window-size-change-functions #'eca-chat--on-window-size-change)
  (mapcar (lambda (title+buffer)
            (let ((chat-buffer (cdr title+buffer)))
              (when (buffer-live-p chat-buffer)
                (eca-chat--with-current-buffer chat-buffer
                  ;; Cancel spinner timer if chat was still loading.
                  (eca-chat--spinner-stop)
                  (eca-chat--tool-call-elapsed-stop-all)
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
                  (when-let* ((window (get-buffer-window chat-buffer)))
                    (quit-window nil window))))))
          (eca--session-chats session)))

;;;###autoload
(defun eca-chat-clear ()
  "Clear the eca chat messages history on server and visually."
  (interactive)
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (when eca-chat--id
      (eca-api-request-sync (eca-session)
                            :method "chat/clear"
                            :params (list :chatId eca-chat--id :messages t)))
    (eca-chat--clear)))

;;;###autoload
(defun eca-chat-select-model ()
  "Select which model to use in the chat from what server supports."
  (interactive)
  (eca-assert-session-running (eca-session))
  (when eca-chat-custom-model
    (error (eca-error "The eca-chat-custom-model variable is already set: %s" eca-chat-custom-model)))
  (when-let* ((model (completing-read "Select a model:" (append (eca--session-models (eca-session)) nil) nil t)))
    (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
      (setq-local eca-chat--selected-model model)
      (setq eca-chat--last-known-model model))
    (eca-api-notify (eca-session)
                    :method "chat/selectedModelChanged"
                    :params (list :model model
                                  :variant eca-chat--selected-variant))))

;;;###autoload
(defun eca-chat-select-variant ()
  "Select which variant to use for the current model."
  (interactive)
  (eca-assert-session-running (eca-session))
  (let* ((variants (append (eca--session-chat-variants (eca-session)) nil))
         (candidates (cons "-" (sort variants #'string-lessp)))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata (display-sort-function . ,#'identity)
                                 (cycle-sort-function . ,#'identity))
                    (complete-with-action action candidates string pred)))))
    (when-let* ((variant (completing-read "Select a variant:" table nil t)))
      (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
        (setq-local eca-chat--selected-variant variant)
        (setq eca-chat--last-known-variant variant)))))

;;;###autoload
(defun eca-chat-select-agent ()
  "Select which chat agent to use from what server supports."
  (interactive)
  (eca-assert-session-running (eca-session))
  (when-let* ((agent (completing-read "Select an agent:" (append (eca--session-chat-agents (eca-session)) nil) nil t)))
    (eca-chat--set-agent (eca-session) agent (current-buffer))))

;;;###autoload
(defun eca-chat-cycle-agent ()
  "Cycle between existing chat agents to use."
  (interactive)
  (eca-assert-session-running (eca-session))
  (let* ((session (eca-session))
         (current-agent (eca-chat--agent))
         (all-agents (append (eca--session-chat-agents session) nil))
         (current-agent-index (seq-position all-agents current-agent))
         (next-agent (or (nth (1+ current-agent-index) all-agents)
                         (nth 0 all-agents))))
    (eca-chat--set-agent session next-agent (current-buffer))))

;;;###autoload
(defun eca-chat-toggle-trust ()
  "Toggle trust mode (auto-accept all tool call)."
  (interactive)
  (let ((new-value (not (eca-chat--trust))))
    (eca-chat--set-trust (eca-session) new-value)
    (eca-info (if new-value
                  "Enabled trust-mode (Auto accept tool calls)"
                "Disabled trust-mode (Auto accept tool calls)"))))

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
  "Search the next pending approval tool call in the buffer and approve it.
Starting from the beginning of the buffer."
  (interactive)
  (eca-assert-session-running (eca-session))
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (save-excursion
      (goto-char (point-min))
      (when (text-property-search-forward 'eca-tool-call-pending-approval-accept t t)
        (call-interactively #'eca-chat--key-pressed-return)))))

;;;###autoload
(defun eca-chat-tool-call-reject-next ()
  "Search the next pending approval tool call in the buffer and reject it.
Starting from the beginning of the buffer."
  (interactive)
  (eca-assert-session-running (eca-session))
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (save-excursion
      (goto-char (point-min))
      (when (text-property-search-forward 'eca-tool-call-pending-approval-reject t t)
        (call-interactively #'eca-chat--key-pressed-return)))))

;;;###autoload
(defun eca-chat-reset ()
  "Kill current chat (asking to keep or not history) and start a new."
  (interactive)
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (when eca-chat--id
      (kill-buffer)
      (eca-chat-new))))

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
    (unless (eca-chat--expandable-content-at-point-dwim)
      (eca-chat-go-to-prev-expandable-block))
    (when-let ((ov (eca-chat--expandable-content-at-point-dwim)))
      (eca-chat--expandable-content-toggle (overlay-get ov 'eca-chat--expandable-content-id) (when force-open? t) (not force-open?)))))

;;;###autoload
(defun eca-chat-expand-all-blocks ()
  "Expand all expandable blocks in current chat."
  (interactive)
  (eca-assert-session-running (eca-session))
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (let ((expandable-overlays
           (-filter (lambda (ov) (overlay-get ov 'eca-chat--expandable-content-id))
                    (overlays-in (point-min) (point-max)))))
      (seq-doseq (ov expandable-overlays)
        (eca-chat--expandable-content-toggle (overlay-get ov 'eca-chat--expandable-content-id) t nil)))))

;;;###autoload
(defun eca-chat-collapse-all-blocks ()
  "Collapse all expandable blocks in current chat."
  (interactive)
  (eca-assert-session-running (eca-session))
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (let ((expandable-overlays
           (-filter (lambda (ov) (overlay-get ov 'eca-chat--expandable-content-id))
                    (overlays-in (point-min) (point-max)))))
      (seq-doseq (ov expandable-overlays)
        (eca-chat--expandable-content-toggle (overlay-get ov 'eca-chat--expandable-content-id) t t)))))

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
    (let* ((session (eca-session))
           (prompt (eca-chat--prompt-content)))
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
  "Select a chat.
Shows each chat with its status (🚧 pending approval, ⏳ loading),
title, and elapsed time annotation."
  (interactive)
  (let* ((session (eca-session))
         (buf-by-label (make-hash-table :test 'equal))
         (annotation-by-label (make-hash-table :test 'equal)))
    (eca-assert-session-running session)
    ;; Build items oldest-first (same order as tab-line)
    (let ((items (append
                  (nreverse
                   (-keep (lambda (buffer)
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (let* ((title (eca-chat-title))
                                       (status (eca-chat--chat-status-prefix))
                                       (label (concat status title))
                                       ;; Disambiguate duplicate titles
                                       (label (if (gethash label buf-by-label)
                                                  (concat label " (" eca-chat--id ")")
                                                label))
                                       (face (cond
                                              ((eca-chat--has-pending-approvals-p) 'warning)
                                              (eca-chat--chat-loading 'shadow)
                                              (t nil)))
                                       (display (if face
                                                    (propertize label 'face face)
                                                  label))
                                       (time-str (eca-chat--turn-duration-str)))
                                  (puthash display buffer buf-by-label)
                                  (when time-str
                                    (puthash display
                                             (propertize (concat "  " time-str)
                                                         'face 'eca-chat-elapsed-time-face)
                                             annotation-by-label))
                                  display))))
                          (eca-vals (eca--session-chats session))))
                  (list eca-chat-new-chat-label))))
      (when-let (chosen (completing-read
                         "Select the chat: "
                         (lambda (string pred action)
                           (if (eq action 'metadata)
                               `(metadata
                                 (display-sort-function . ,#'identity)
                                 (annotation-function
                                  . ,(lambda (candidate)
                                       (gethash candidate annotation-by-label))))
                             (complete-with-action action items string pred)))
                         nil t))
        (if-let (buffer (gethash chosen buf-by-label))
            (progn
              (setf (eca--session-last-chat-buffer session) buffer)
              (eca-chat-open session))
          (eca-chat-new))))))

;;;###autoload
(defun eca-chat-rename ()
  "Rename last visited chat to a custom NEW-NAME."
  (interactive)
  (let ((new-name (read-string "Inform the new chat title: ")))
    (eca-assert-session-running (eca-session))
    (with-current-buffer (eca-chat--get-last-buffer (eca-session))
      (setq eca-chat--custom-title new-name))))

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
Resteps a list of plists, each containing:
  :text      - the message text
  :start     - start position in buffer
  :end       - end position in buffer
  :id        - message ID from overlay
  :line      - line number of the message
  :timestamp - timestamp when message was sent

Messages are ordered from newest to oldest.
Resteps empty list if session is not running or buffer is not available."
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
Resteps selected message plist or nil if no messages or cancelled."
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

;;;###autoload
(defun eca-chat-save-to-file (&optional file)
  "Export the current chat to a FILE."
  (interactive)
  (eca-assert-session-running (eca-session))
  (with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (let* ((initial-dir (pcase eca-chat-save-chat-initial-path
                          ('workspace-root (eca-find-root-for-buffer))
                          (_ eca-chat-save-chat-initial-path)))
           (initial-name (concat (or eca-chat--custom-title eca-chat--title)
                                 ".md"))
           (file (or file
                     (read-file-name "Select the file path to save the chat: " initial-dir nil nil initial-name)))
           (chat-content (buffer-string))
           (new-buffer (find-file-noselect file)))
      (with-current-buffer new-buffer
        (delete-region (point-min) (point-max))
        (insert chat-content)
        (save-buffer))
      (eca-info (format "Saved chat to '%s'" file)))))

(provide 'eca-chat)
;;; eca-chat.el ends here
