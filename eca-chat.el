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
(require 'find-func)
(require 'markdown-mode)
(require 'compat)

(require 'eca-util)
(require 'eca-api)
(require 'eca-mcp)
(require 'eca-diff)
(require 'eca-table)
(require 'eca-chat-expandable)
(require 'eca-chat-context)
(require 'eca-chat-image)

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

(defvar eca-chat-session-status-changed-functions nil
  "Abnormal hook run when a session aggregated status may have changed.
Each function is called with a single argument, the session.  It is
invoked on transitions that can change `eca-chat-session-status' (a chat
starting or finishing, a tool call requesting or resolving an approval, a
question being asked or answered).  Subscribers should be idempotent: the
hook may run even when the status is unchanged.  Used by integrations
such as the Doom workspaces tabline to refresh external indicators.")

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

(defcustom eca-chat-read-only-history t
  "When non-nil, make the chat history/output area read-only.
Only the prompt block at the bottom stays editable, which prevents
accidental edits to previous messages and assistant output.  Set
to nil to keep the whole chat buffer writable."
  :type 'boolean
  :group 'eca)

(defcustom eca-chat-prompt-prefix-loading "⏳ "
  "The prompt prefix string used in eca chat buffer when loading."
  :type 'string
  :group 'eca)

(defcustom eca-chat-prompt-prefix-question "Answer> "
  "The prompt prefix string used when a question is pending."
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

(defcustom eca-chat-trust-on-symbol "🔥"
  "The string used in eca chat buffer mode-line when trust is ON."
  :type 'string
  :group 'eca)

(defcustom eca-chat-trust-off-symbol "🛡️"
  "The string used in eca chat buffer mode-line when trust is OFF."
  :type 'string
  :group 'eca)

(defcustom eca-chat-trust-on-symbol-tty "●"
  "Mode-line trust ON glyph used on terminal (non-graphic) frames."
  :type 'string
  :group 'eca)

(defcustom eca-chat-trust-off-symbol-tty "○"
  "Mode-line trust OFF glyph used on terminal (non-graphic) frames."
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

(defcustom eca-chat-show-copy-buttons t
  "Whether to show copy buttons for chat responses and code blocks."
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
           (const :tag "Last message cost" :last-message-cost)
           (const :tag "The percentage of context limt used in the current session" :session-tokens-percentage)))
  :group 'eca)

(defcustom eca-chat-mode-line-format
  '(:workspace-folders :add-workspace-button :remove-workspace-button :spacer :init-progress "  " :bg-jobs " " :elapsed-time "   " :context-bar :usage " " :trust)
  "Format for the ECA chat mode line.

When set to a list, each element is a module keyword or a
literal string.  Modules are rendered in order; use `:spacer'
to separate left-aligned and right-aligned content.

Available modules:
  `:workspace-folders' - project root paths
  `:add-workspace-button' - clickable [+] button
  `:remove-workspace-button' - clickable [-] button
  `:title' - chat title
  `:elapsed-time' - turn duration timer
  `:context-bar' - colored context-window usage bar (hover for details)
  `:usage' - token/cost info (see `eca-chat-usage-string-format')
  `:server-version' - shows \"ECA <version>\"
  `:init-progress' - init progress (auto-hides when done)
  `:trust' - trust mode indicator (🔥 when ON, 🛡 when OFF)
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
            (const :tag "Remove workspace button" :remove-workspace-button)
            (const :tag "Background jobs" :bg-jobs)
            (const :tag "Chat title" :title)
            (const :tag "Elapsed time" :elapsed-time)
            (const :tag "Context usage bar" :context-bar)
            (const :tag "Usage info" :usage)
            (const :tag "ECA server version" :server-version)
            (const :tag "Init progress" :init-progress)
            (const :tag "Trust mode indicator" :trust)
            (const :tag "Right-align spacer" :spacer)))
          (function :tag "Custom function (receives session)"))
  :group 'eca)

(defcustom eca-chat-override-mode-line t
  "When non-nil, ECA chat sets a custom mode line for chat buffers.
Set this to nil to keep the default Emacs mode line (including buffer name)."
  :type 'boolean
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

(defcustom eca-chat-fontify-debounce-interval 0.15
  "Idle delay in seconds before a deferred fontify runs during streaming.
Instead of calling `font-lock-ensure' on every streamed chunk,
`eca-chat--render-content' schedules it via an idle timer with this
delay.  A single guaranteed `font-lock-ensure' always runs when the
response finishes, before table alignment.

When nil, no intermediate fontify is scheduled and the buffer is
only fontified at end-of-stream (jit-lock still handles visible-area
updates during streaming)."
  :type '(choice (const :tag "Disabled (final ensure only)" nil)
                 (number :tag "Seconds"))
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

(defcustom eca-chat-history-page-size 50
  "Number of newest messages to load when opening a persisted chat.
When non-nil, `eca-chat-resume' opens chats with a bounded window of the
newest messages and shows a \"Load older messages\" control to page
through earlier history on demand, avoiding a full replay of very long
chats.  When nil, the entire history is replayed on open (legacy
behavior)."
  :type '(choice (const :tag "Full replay (no pagination)" nil)
                 (integer :tag "Newest messages per page"))
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

(defface eca-chat-steer-prompt-face
  '((t :inherit font-lock-keyword-face :slant italic :underline nil))
  "Face for the steer prompt indicator."
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

(defface eca-chat-copy-button-face
  '((t (:inherit link :underline nil :weight bold)))
  "Face for chat copy buttons."
  :group 'eca)

(defface eca-chat-copy-label-face
  '((t (:inherit font-lock-comment-face :height 0.9)))
  "Face for chat copy button labels."
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

(defface eca-chat-resume-link-face
  '((((background dark))  (:foreground "deep sky blue" :underline t))
    (((background light)) (:foreground "dark cyan" :underline t)))
  "Face for the resume session link in the welcome area."
  :group 'eca)

(defface eca-chat-load-more-face
  '((((background dark))  (:foreground "deep sky blue" :underline t))
    (((background light)) (:foreground "dark cyan" :underline t)))
  "Face for the \"Load older messages\" control in the chat."
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
  '((t :inherit shadow))
  "Face for trust mode when off in mode-line."
  :group 'eca)

(defface eca-chat-usage-string-face
  '((t :height 0.9 :inherit font-lock-doc-face))
  "Face for the strings segments in usage string in mode-line of the chat."
  :group 'eca)

(defface eca-chat-context-system-prompt-face
  '((((background dark))  (:foreground "#61afef"))
    (((background light)) (:foreground "#2c6fbf")))
  "Face for the system-prompt segment of the context-usage bar."
  :group 'eca)

(defface eca-chat-context-tool-definitions-face
  '((((background dark))  (:foreground "#e8924a"))
    (((background light)) (:foreground "#c2772e")))
  "Face for the tool-definitions segment of the context-usage bar."
  :group 'eca)

(defface eca-chat-context-conversation-face
  '((((background dark))  (:foreground "#98c379"))
    (((background light)) (:foreground "#4f8f2f")))
  "Face for the conversation segment of the context-usage bar."
  :group 'eca)

(defface eca-chat-context-rules-face
  '((((background dark))  (:foreground "#c678dd"))
    (((background light)) (:foreground "#8a3fa0")))
  "Face for the rules segment of the context-usage bar."
  :group 'eca)

(defface eca-chat-context-skills-face
  '((((background dark))  (:foreground "#e5c07b"))
    (((background light)) (:foreground "#b58900")))
  "Face for the skills segment of the context-usage bar."
  :group 'eca)

(defface eca-chat-context-agents-face
  '((((background dark))  (:foreground "#d19a66"))
    (((background light)) (:foreground "#b5651d")))
  "Face for the AGENTS.md segment of the context-usage bar."
  :group 'eca)

(defface eca-chat-context-tool-calls-face
  '((((background dark))  (:foreground "#e06c75"))
    (((background light)) (:foreground "#c0392b")))
  "Face for the tool-calls segment of the context-usage bar."
  :group 'eca)

(defface eca-chat-context-free-face
  '((t :inherit shadow))
  "Face for the free-space segment of the context-usage bar."
  :group 'eca)

(defface eca-chat-elapsed-time-face
  '((t :height 0.9 :inherit font-lock-comment-face))
  "Face for the elapsed time indicator in mode-line of the chat."
  :group 'eca)

(defface eca-chat-command-description-face
  '((t :inherit font-lock-comment-face))
  "Face for the descriptions in chat command completion."
  :group 'eca)

(defface eca-chat-approval-modeline-face
  '((((background dark))  :background "#4a4000")
    (((background light)) :background "#fff8dc"))
  "Face for modeline when approval is pending."
  :group 'eca)

(defface eca-tab-inactive-face
  '((t :inherit shadow))
  "Face for non-selected idle tab-line tabs."
  :group 'eca)

(defface eca-chat-tab-active-face
  '((t :inherit warning))
  "Face for selected active chat tabs.
Active means loading or pending approval."
  :group 'eca)

(defface eca-chat-tab-inactive-active-face
  '((((background dark))  :foreground "#b8860b")
    (((background light)) :foreground "#8b6914"))
  "Face for non-selected active chat tabs.
A dimmer yellow for loading/approval tabs that
are not currently selected."
  :group 'eca)

(defface eca-chat-flag-face
  '((t :inherit font-lock-number-face))
  "Face for flag markers in chat."
  :group 'eca)

(defface eca-chat-question-face
  '((t :inherit font-lock-string-face :weight bold))
  "Face for the question text in ask-question blocks."
  :group 'eca)

(defface eca-chat-question-option-face
  '((t :inherit font-lock-function-name-face :underline t))
  "Face for selectable option labels in ask-question blocks."
  :group 'eca)

(defface eca-chat-question-description-face
  '((t :inherit font-lock-comment-face))
  "Face for option descriptions in ask-question blocks."
  :group 'eca)

;; Internal

(defvar-local eca-chat--closed nil)
(defvar-local eca-chat--history '())
(defvar-local eca-chat--history-index -1)

(defvar-local eca-chat--welcome-shown nil
  "Non-nil when this buffer still displays the welcome banner.
Set when the welcome message and resume link are inserted in
`eca-chat-mode'; cleared by `eca-chat--clear'.  Used to decide
whether the first user prompt should erase the banner first.")
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
(defvar-local eca-chat--fontify-timer nil
  "Idle timer that defers `font-lock-ensure' during streaming.")
(defvar-local eca-chat--progress-text "")
(defvar-local eca-chat--last-user-message-pos nil)
(defvar-local eca-chat--last-response-copy-start nil
  "Buffer position where the latest assistant response starts.")
(defvar-local eca-chat--last-response-copy-kind nil
  "Kind of latest top-level assistant content for copy scoping.")
(defvar-local eca-chat--chat-loading nil)
(defvar-local eca-chat--session-cost nil)
(defvar-local eca-chat--message-cost nil)
(defvar-local eca-chat--message-input-tokens nil)
(defvar-local eca-chat--message-output-tokens nil)
(defvar-local eca-chat--session-tokens nil)
(defvar-local eca-chat--session-limit-context nil)
(defvar-local eca-chat--session-limit-output nil)
(defvar-local eca-chat--context-breakdown nil
  "Latest context-window usage breakdown plist for the current chat.
Keys: :categories (vector of :name/:tokens plists), :usedTokens,
:freeTokens and :contextLimit, as sent in the `usage' content.")
(defvar-local eca-chat--queued-prompt nil)
(defvar-local eca-chat--steered-prompt nil)
(defvar-local eca-chat--subagent-chat-id->tool-call-id (make-hash-table :test 'equal)
  "Hash table mapping subagent chatId to the parent tool call expandable block id.")
(defvar-local eca-chat--subagent-usage (make-hash-table :test 'equal)
  "Hash table mapping tool-call-id to a plist (:session-tokens N :context-limit N).
Stores the latest usage data received for each running subagent.")

(defvar-local eca-chat--history-before-cursor nil
  "Opaque cursor for loading the older history page, or nil at the start.")
(defvar-local eca-chat--history-after-cursor nil
  "Opaque cursor for loading the newer history page, or nil at the tail.")
(defvar-local eca-chat--history-compaction-cursor nil
  "Opaque cursor at the last compaction boundary, or nil when never compacted.")
(defvar-local eca-chat--history-total nil
  "Total number of messages in the full chat history, from pagination meta.")
(defvar-local eca-chat--history-loading nil
  "Non-nil while an older-history page request is in flight.")

(defvar-local eca-chat--server-version nil
  "Cached ECA server version string for mode-line display.")

(defvar-local eca-chat--task-state nil
  "Current task state plist with :goal and :tasks.
Each task is a plist with :id, :content, :status, :priority, etc.")

(defvar-local eca-chat--stopping-safety-timer nil
  "Safety timer to force-clear \='stopping state.
Used when server never responds to stop request.")

(defvar-local eca-chat--pending-question nil
  "When non-nil, holds the active question state.
A plist with :session :request :question :options :tool-call-id :allow-freeform.")

;; Buffer-local caches for singleton overlays.  The chat buffer
;; contains a fixed set of overlays that are created once at chat
;; setup (prompt-area, prompt-field, progress-area, context-area,
;; task-area) plus optionally one question-block.  Each lookup
;; historically scanned every overlay in the buffer via
;; `(overlays-in (point-min) (point-max))', which scales linearly
;; with chat length and is exercised on every streamed chunk.
;; Caching the overlay reference per buffer turns those lookups
;; into O(1).  The cache is invalidated automatically when the
;; cached overlay is deleted (its `overlay-buffer' becomes nil)
;; and explicitly in `eca-chat--clear'.
(defvar-local eca-chat--prompt-area-ov-cache nil)
(defvar-local eca-chat--prompt-field-ov-cache nil)
(defvar-local eca-chat--progress-area-ov-cache nil)
(defvar-local eca-chat--context-area-ov-cache nil)
(defvar-local eca-chat--task-area-ov-cache nil)
(defvar-local eca-chat--question-block-ov-cache nil)


(defvar eca-chat--new-chat-id 0)
(defvar eca-chat--last-known-model nil)
(defvar eca-chat--last-known-agent nil)
(defvar eca-chat--last-known-variant nil)
(defvar eca-chat--last-known-trust eca-chat-trust-enable
  "Session-wide trust default for new chats.
Seeded from `eca-chat-trust-enable' and kept in sync with the server's
`config/updated' selectTrust, so the server `chat.defaultTrust' config
applies to new chats too.")

(defvar eca--chat-init-session nil
  "Dynamically bound session during `eca-chat-mode' initialization.")

(defvar eca--chat-init-skip-welcome nil
  "When non-nil, skip welcome message during `eca-chat-mode' init.
Bound while creating a buffer for a server-initiated chat (resume,
fork) whose content will be streamed in next, so the welcome text
and resume link are not left behind under the replayed messages.")

(defun eca-chat-new-buffer-name (session)
  "Return the chat buffer name for SESSION."
  (format "<eca-chat[%s]:%s:%s>"
          (eca--session-project-name session)
          (eca--session-id session)
          eca-chat--new-chat-id))

(defvar eca-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map markdown-mode-map)
    (define-key map (kbd "S-<return>") #'eca-chat--key-pressed-newline)
    (define-key map (kbd "C-<return>") #'eca-chat--key-pressed-queue)
    (define-key map (kbd "C-<up>") #'eca-chat--key-pressed-previous-prompt-history)
    (define-key map (kbd "C-<down>") #'eca-chat--key-pressed-next-prompt-history)
    (define-key map (kbd "<return>") #'eca-chat--key-pressed-return)
    (define-key map (kbd "RET") #'eca-chat--key-pressed-return)
    (define-key map (kbd "C-c C-<return>") #'eca-chat-send-prompt-at-chat)
    (define-key map (kbd "<tab>") #'eca-chat--key-pressed-tab)
    (define-key map (kbd "TAB") #'eca-chat--key-pressed-tab)
    (define-key map (kbd "C-c C-k") #'eca-chat-reset)
    (define-key map (kbd "C-c C-S-k") #'eca-chat-delete)
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
    (define-key map (kbd "C-c C-S-h") #'eca-chat-timeline)
    (define-key map (kbd "C-c C-S-o") #'eca-chat-load-older-history)
    (define-key map (kbd "C-c C-a") #'eca-chat-tool-call-accept-all)
    (define-key map (kbd "C-c C-S-a") #'eca-chat-tool-call-accept-next)
    (define-key map (kbd "C-c C-y") #'eca-chat-tool-call-accept-all-and-remember)
    (define-key map (kbd "C-c C-r") #'eca-chat-tool-call-reject-next)
    (define-key map (kbd "C-c C-S-r") #'eca-chat-rename)
    (define-key map (kbd "C-c .") #'eca-transient-menu)
    (define-key map (kbd "C-c C-,") #'eca-settings)
    (define-key map (kbd "C-c C-<up>") #'eca-chat-go-to-prev-user-message)
    (define-key map (kbd "C-c C-<down>") #'eca-chat-go-to-next-user-message)
    (define-key map (kbd "C-c <up>") #'eca-chat-go-to-prev-expandable-block)
    (define-key map (kbd "C-c <down>") #'eca-chat-go-to-next-expandable-block)
    (define-key map (kbd "C-c <tab>") #'eca-chat-toggle-expandable-block)
    ;; Per-chat inline image zoom (browser-style).  Uses `C-c C-z' as
    ;; the prefix because `C-c <letter>' sequences are reserved for
    ;; users by the Emacs key binding conventions; `C-c' followed by
    ;; a control character is reserved for major modes.  `=' is a
    ;; no-Shift alias for `+' so users on US layouts don't need to
    ;; press Shift.
    (define-key map (kbd "C-c C-z +") #'eca-chat-image-zoom-in)
    (define-key map (kbd "C-c C-z =") #'eca-chat-image-zoom-in)
    (define-key map (kbd "C-c C-z -") #'eca-chat-image-zoom-out)
    (define-key map (kbd "C-c C-z 0") #'eca-chat-image-zoom-reset)
    (define-key map (kbd "C-c C-z s") #'eca-chat-save-image-at-point)
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
  "Get chat buffer for SESSION and CHAT-ID, or nil when none registered.
Since the client now generates the chat-id at buffer creation
time (see `eca-chat-open'), every known chat is registered under
its real id and there is no `'empty' placeholder to migrate from."
  (eca-get (eca--session-chats session) chat-id))

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
  (let ((inhibit-read-only t))
    (apply #'insert contents))
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

(defun eca-chat--update-bg-job-emoji (tool-call-id new-emoji)
  "Update background job status emoji in TOOL-CALL-ID.
Replace the job status emoji with NEW-EMOJI."
  (when-let* ((ov (eca-chat--get-expandable-content
                   tool-call-id))
              (ov-content (overlay-get ov 'eca-chat--expandable-content-ov-content)))
    (let ((start (overlay-start ov))
          (end (overlay-start ov-content))
          (inhibit-read-only t))
      (save-excursion
        (goto-char start)
        (when (re-search-forward "🟡\\|✅\\|🔴\\|⚫" end t)
          (replace-match new-emoji t t))))))

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
  eca-chat--selected-trust)

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
        (let ((result (concat (funcall propertize-fn failed 'error (or (> running 0) (> starting 0)))
                              (funcall propertize-fn starting 'warning (> running 0))
                              (funcall propertize-fn running 'success))))
          (if (string-empty-p result) "0" result))))))

(defcustom eca-chat-context-bar-width 16
  "Width in characters of the context-usage bar in the mode-line."
  :type 'integer
  :group 'eca)

(defconst eca-chat--context-category-faces
  '(("System prompt" . eca-chat-context-system-prompt-face)
    ("Rules" . eca-chat-context-rules-face)
    ("Skills" . eca-chat-context-skills-face)
    ("AGENTS.md" . eca-chat-context-agents-face)
    ("Tool definitions" . eca-chat-context-tool-definitions-face)
    ("Tool calls" . eca-chat-context-tool-calls-face)
    ("Conversation" . eca-chat-context-conversation-face))
  "Fallback alist mapping context category name to a bar face.
Only used when the server does not provide a per-category color.")

(defun eca-chat--context-category-face (name)
  "Return the bar face for category NAME."
  (or (cdr (assoc name eca-chat--context-category-faces))
      'eca-chat-context-conversation-face))

(defun eca-chat--context-category-face-spec (cat)
  "Return a face spec for category CAT, preferring its server color.
Falls back to the client face palette for older servers."
  (let ((color (plist-get cat :color)))
    (if (and (stringp color) (not (string-empty-p color)))
        (list :foreground color)
      (eca-chat--context-category-face (plist-get cat :name)))))

(defun eca-chat--context-category-color (cat)
  "Return the bar color (hex string) for category CAT.
Prefers the server color, falling back to the client face foreground."
  (let ((color (plist-get cat :color)))
    (or (and (stringp color) (not (string-empty-p color)) color)
        (face-foreground (eca-chat--context-category-face (plist-get cat :name))
                         nil t)
        "#888888")))

(defun eca-chat--context-free-face-spec (breakdown)
  "Return the face spec for the free portion from BREAKDOWN."
  (let ((color (plist-get breakdown :freeColor)))
    (if (and (stringp color) (not (string-empty-p color)))
        (list :foreground color)
      'eca-chat-context-free-face)))

(defun eca-chat--context-free-color (breakdown)
  "Return the free-portion color (hex string) from BREAKDOWN."
  (let ((color (plist-get breakdown :freeColor)))
    (or (and (stringp color) (not (string-empty-p color)) color)
        (face-foreground 'eca-chat-context-free-face nil t)
        "#5c6370")))

(defun eca-chat--context-bar-help (breakdown used free limit)
  "Build the help-echo legend for the context bar from BREAKDOWN.
USED, FREE and LIMIT are the aggregate token counts.  Each line is
prefixed with a colored swatch matching the bar so the colors map to
their category."
  (let* ((nf #'eca-chat--number->friendly-number)
         ;; Prefer the server-provided emoji swatch (consistent with the
         ;; /context text); fall back to a colored block for older servers.
         (swatch (lambda (emoji spec)
                   (or emoji (propertize "█" 'face spec))))
         (head (if (and limit (> limit 0))
                   (format "Context: %s / %s (%d%%)"
                           (funcall nf used) (funcall nf limit)
                           (round (* 100 (/ (float used) limit))))
                 (format "Context: %s used" (funcall nf used))))
         (rows (mapconcat
                (lambda (cat)
                  (format " %s %s: %s"
                          (funcall swatch (plist-get cat :emoji)
                                   (eca-chat--context-category-face-spec cat))
                          (plist-get cat :name)
                          (funcall nf (or (plist-get cat :tokens) 0))))
                (append (plist-get breakdown :categories) nil)
                "\n")))
    (concat head "\n" rows
            (when (and free (> free 0))
              (format "\n %s Free space: %s"
                      (funcall swatch (plist-get breakdown :freeEmoji)
                               (eca-chat--context-free-face-spec breakdown))
                      (funcall nf free)))
            "\n\nClick to run /context")))

(defvar eca-chat--context-bar-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'eca-chat-show-context)
    map)
  "Keymap for the modeline context-usage bar.")

(defconst eca-chat--context-bar-eighths
  ["" "▏" "▎" "▍" "▌" "▋" "▊" "▉" "█"]
  "Left-block glyphs indexed by eighths (0..8) for sub-cell precision.")

(defun eca-chat--context-bar-indices-by-tokens (tokens)
  "Return indices into TOKENS ordered by descending value."
  (mapcar #'car
          (sort (cl-loop for v in tokens for i from 0 collect (cons i v))
                (lambda (a b) (> (cdr a) (cdr b))))))

(defun eca-chat--context-bar-allocate (tokens cells)
  "Allocate CELLS whole cells across TOKENS by proportion.
Every positive entry gets at least one cell when CELLS allows it; the
result is a list of non-negative integers summing to CELLS (or to the
number of cells available when there are more categories than cells)."
  (let* ((n (length tokens))
         (positives (cl-count-if (lambda (tk) (> tk 0)) tokens)))
    (cond
     ((or (<= cells 0) (zerop positives)) (make-list n 0))
     ((<= cells positives)
      ;; Too tight for all: one cell each to the CELLS largest categories.
      (let ((out (make-list n 0)))
        (dolist (i (seq-take (eca-chat--context-bar-indices-by-tokens tokens) cells))
          (when (> (nth i tokens) 0) (setf (nth i out) 1)))
        out))
     (t
      ;; One cell per positive category, then the rest by proportion.
      (let* ((total (apply #'+ tokens))
             (remaining (- cells positives))
             (running 0) (prev 0)
             (extra (mapcar (lambda (tk)
                              (setq running (+ running tk))
                              (let ((cum (round (* remaining (/ (float running) total)))))
                                (prog1 (- cum prev) (setq prev cum))))
                            tokens)))
        (cl-mapcar (lambda (tk e) (+ (if (> tk 0) 1 0) e)) tokens extra))))))

(defun eca-chat--context-bar-edge-spec (categories alloc)
  "Return the face spec for the sub-cell edge given CATEGORIES and ALLOC."
  (let ((last nil))
    (cl-loop for cat in categories for n in alloc
             when (> n 0) do (setq last cat))
    (eca-chat--context-category-face-spec (or last (car categories)))))

(defun eca-chat--context-bar-chars (categories breakdown width frac)
  "Build the block-character context bar string for terminal frames.
CATEGORIES/BREAKDOWN carry the usage data, WIDTH is the cell count and
FRAC the used fraction (0..1).  The used/free edge uses a fractional
block for sub-cell precision."
  (let* ((colored-exact (* width frac))
         (colored-cells (min width (floor colored-exact)))
         (edge-eighths (if (< colored-cells width)
                           (min 7 (max 0 (round (* 8 (- colored-exact colored-cells)))))
                         0))
         (tokens (mapcar (lambda (c) (max 0 (or (plist-get c :tokens) 0))) categories))
         (alloc (eca-chat--context-bar-allocate tokens colored-cells))
         (bar ""))
    (cl-loop for cat in categories for n in alloc
             when (> n 0)
             do (setq bar (concat bar (propertize (make-string n ?█)
                                                  'face (eca-chat--context-category-face-spec cat)))))
    (when (> edge-eighths 0)
      (setq bar (concat bar (propertize (aref eca-chat--context-bar-eighths edge-eighths)
                                        'face (eca-chat--context-bar-edge-spec categories alloc)))))
    (let ((free-cells (- width (length bar))))
      (when (> free-cells 0)
        (setq bar (concat bar (propertize (make-string free-cells ?█)
                                          'face (eca-chat--context-free-face-spec breakdown))))))
    bar))

(defun eca-chat--context-bar-pixels (categories breakdown width frac)
  "Build the pixel-width thin-segment context bar for graphical frames.
CATEGORIES/BREAKDOWN carry the usage data; each category is a
`:background'-colored space whose pixel width is proportional to its
share, giving sub-character granularity so small percentages still
show as a thin sliver.  WIDTH is the footprint in characters and FRAC
the used fraction (0..1)."
  (let* ((char-px (max 1 (frame-char-width)))
         (total-px (* width char-px))
         (used-px (min total-px (round (* total-px frac))))
         (tokens (mapcar (lambda (c) (max 0 (or (plist-get c :tokens) 0))) categories))
         (alloc (eca-chat--context-bar-allocate tokens used-px))
         (seg (lambda (px color)
                (propertize " "
                            'display (list 'space :width (list px))
                            'face (list :background color))))
         (bar "")
         (drawn 0))
    (cl-loop for cat in categories for px in alloc
             when (> px 0)
             do (setq bar (concat bar (funcall seg px (eca-chat--context-category-color cat)))
                      drawn (+ drawn px)))
    (let ((free-px (- total-px drawn)))
      (when (> free-px 0)
        (setq bar (concat bar (funcall seg free-px (eca-chat--context-free-color breakdown))))))
    bar))

(defun eca-chat--context-bar ()
  "Return the propertized context-usage bar string, or nil.
Renders nothing until the server has sent a context breakdown.  In
graphical frames it draws pixel-width thin segments for high
granularity; terminals fall back to block characters.  The bar maps
the whole window (colored = used, dim = free); percentages and
per-category tokens are in the tooltip."
  (when-let* ((breakdown eca-chat--context-breakdown)
              (categories (append (plist-get breakdown :categories) nil)))
    (let* ((used (or (plist-get breakdown :usedTokens) 0))
           (free (plist-get breakdown :freeTokens))
           (limit (plist-get breakdown :contextLimit))
           (width (max 1 eca-chat-context-bar-width))
           (frac (if (and limit (> limit 0)) (min 1.0 (/ (float used) limit)) 1.0))
           (help (eca-chat--context-bar-help breakdown used free limit))
           (bar (if (display-graphic-p)
                    (eca-chat--context-bar-pixels categories breakdown width frac)
                  (eca-chat--context-bar-chars categories breakdown width frac))))
      (propertize bar
                  'help-echo help
                  'local-map eca-chat--context-bar-mode-line-map))))

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
  (let ((context-area-ov (make-overlay (line-beginning-position) (line-end-position) (current-buffer) nil t)))
    (overlay-put context-area-ov 'eca-chat-context-area t)
    (eca-chat--insert (propertize eca-chat-context-prefix 'font-lock-face 'eca-chat-context-unlinked-face))
    (eca-chat--insert "\n")
    (move-overlay context-area-ov (overlay-start context-area-ov) (1- (overlay-end context-area-ov))))
  (let ((prompt-field-ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)) (current-buffer))))
    (overlay-put prompt-field-ov 'eca-chat-prompt-field t)
    (overlay-put prompt-field-ov 'before-string (propertize eca-chat-prompt-prefix 'font-lock-face 'eca-chat-prompt-prefix-face)))
  (eca-chat--protect-non-prompt))

(defun eca-chat--clear (&optional new-prompt-content)
  "Clear the chat for SESSION and then insert NEW-PROMPT-CONTENT."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (remove-overlays (point-min) (point-max)))
  (eca-chat--invalidate-overlay-caches)
  (eca-chat-expandable--reset-id-table)
  (setq-local eca-chat--task-state nil)
  ;; Cancel loading-related timers and reset state
  (when eca-chat--stopping-safety-timer
    (cancel-timer eca-chat--stopping-safety-timer)
    (setq-local eca-chat--stopping-safety-timer nil))
  (when eca-chat--modeline-timer
    (cancel-timer eca-chat--modeline-timer)
    (setq-local eca-chat--modeline-timer nil))
  (setq-local eca-chat--chat-loading nil)
  (setq-local eca-chat--steered-prompt nil)
  (setq-local eca-chat--queued-prompt nil)
  (setq-local eca-chat--welcome-shown nil)
  (setq-local eca-chat--last-user-message-pos nil)
  (setq-local eca-chat--history-before-cursor nil)
  (setq-local eca-chat--history-after-cursor nil)
  (setq-local eca-chat--history-compaction-cursor nil)
  (setq-local eca-chat--history-total nil)
  (setq-local eca-chat--history-loading nil)
  (clrhash eca-chat--subagent-chat-id->tool-call-id)
  (clrhash eca-chat--subagent-usage)
  (eca-chat--insert "\n")
  (eca-chat--insert-prompt-string)
  (eca-chat--refresh-context)
  (when new-prompt-content
    (eca-chat--set-prompt new-prompt-content)))

(defun eca-chat--stop-prompt (session)
  "Stop the running chat prompt for SESSION.
A pending question keeps the turn active server-side, so allow
stopping while one is pending: cancel it, then notify the server."
  (when (or (eq eca-chat--chat-loading t)
            eca-chat--pending-question)
    (when eca-chat--pending-question
      (eca-chat--cancel-question))
    (eca-api-notify session
                    :method "chat/promptStop"
                    :params (list :chatId eca-chat--id))
    (eca-chat--set-chat-loading session 'stopping)))

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

(defun eca-chat--remove-flag (session content-id)
  "Remove a flag identified by CONTENT-ID from the chat via SESSION."
  (eca-api-request-sync session
                        :method "chat/removeFlag"
                        :params (list :chatId eca-chat--id
                                      :contentId content-id))
  (eca-chat--remove-expandable-content content-id))

(defun eca-chat--fork-from-flag (session content-id)
  "Fork the chat from a flag identified by CONTENT-ID via SESSION."
  (eca-api-request-sync session
                        :method "chat/fork"
                        :params (list :chatId eca-chat--id
                                      :contentId content-id)))

(defun eca-chat--set-chat-loading (session loading)
  "Set the SESSION chat loading state.
LOADING can be t (loading), \\='stopping (stop in progress), or nil (idle)."
  (unless (eq eca-chat--chat-loading loading)
    (setq-local eca-chat--chat-loading loading)
    (pcase loading
      ('t
       (setq-local eca-chat--prompt-start-time (current-time))
       (let ((buf (current-buffer))
             (timer nil))
         (setq timer
               (run-with-timer 1 1
                               (lambda ()
                                 (if (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (eca-chat--force-tab-line-update))
                                   (cancel-timer timer)))))
         (setq-local eca-chat--modeline-timer timer))
       ;; Cancel any leftover stopping safety timer
       (when eca-chat--stopping-safety-timer
         (cancel-timer eca-chat--stopping-safety-timer)
         (setq-local eca-chat--stopping-safety-timer nil)))
      ('stopping
       ;; Clear visual indicators (looks idle) but stay logically loading
       ;; so new prompts are queued until server confirms finish.
       (when eca-chat--modeline-timer
         (cancel-timer eca-chat--modeline-timer)
         (setq-local eca-chat--modeline-timer nil))
       (eca-chat--force-tab-line-update)
       ;; Safety timeout: force-clear if server never sends finished
       (let ((buf (current-buffer)))
         (setq-local eca-chat--stopping-safety-timer
                     (run-with-timer 10 nil
                                     (lambda ()
                                       (when (buffer-live-p buf)
                                         (with-current-buffer buf
                                           (when (eq eca-chat--chat-loading 'stopping)
                                             (eca-chat--set-chat-loading session nil)))))))))
      (_
       ;; nil — full stop
       (when eca-chat--stopping-safety-timer
         (cancel-timer eca-chat--stopping-safety-timer)
         (setq-local eca-chat--stopping-safety-timer nil))
       (when eca-chat--prompt-start-time
         (setq-local eca-chat--turn-duration-secs
                     (floor (float-time (time-subtract (current-time) eca-chat--prompt-start-time))))
         (setq-local eca-chat--prompt-start-time nil))
       (when eca-chat--modeline-timer
         (cancel-timer eca-chat--modeline-timer)
         (setq-local eca-chat--modeline-timer nil))
       (eca-chat--force-tab-line-update)))
    (eca-chat--refresh-transient-area)
    (eca-chat--notify-status-changed session)))

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

(defun eca-chat--key-pressed-queue ()
  "Queue the current prompt to be sent after the running prompt finishes."
  (interactive)
  (eca-chat--allow-write
   (let ((prompt (or (eca-chat--prompt-content) "")))
     (when (and (not (string-empty-p prompt))
                eca-chat--chat-loading)
       (eca-chat--queue-prompt prompt)))))

(defmacro eca-chat--cached-overlay (cache-var prop)
  "Return the singleton overlay tagged with PROP, memoized in CACHE-VAR.
Falls back to a single linear scan of overlays in the current
buffer when the cache is empty, holds a deleted overlay, or
points to an overlay in a different buffer.  CACHE-VAR must be a
buffer-local variable (defined with `defvar-local').

The returned overlay is the same one a plain
`(-first (-lambda (ov) (eq t (overlay-get ov PROP)))
         (overlays-in (point-min) (point-max)))'
would produce, but cached so that subsequent calls skip the scan."
  (declare (debug (symbolp form)))
  `(or (let ((ov ,cache-var))
         (when (and (overlayp ov)
                    (overlay-buffer ov)
                    (eq (overlay-buffer ov) (current-buffer))
                    (eq t (overlay-get ov ,prop)))
           ov))
       (setq ,cache-var
             (-first (-lambda (ov) (eq t (overlay-get ov ,prop)))
                     (overlays-in (point-min) (point-max))))))

(defun eca-chat--invalidate-overlay-caches ()
  "Clear all cached singleton-overlay references for the current buffer.
Should be called whenever overlays are wholesale removed, e.g. via
`erase-buffer' + `remove-overlays' in `eca-chat--clear'."
  (setq eca-chat--prompt-area-ov-cache nil
        eca-chat--prompt-field-ov-cache nil
        eca-chat--progress-area-ov-cache nil
        eca-chat--context-area-ov-cache nil
        eca-chat--task-area-ov-cache nil
        eca-chat--question-block-ov-cache nil))

(defun eca-chat--prompt-field-ov ()
  "Return the overlay for the prompt field."
  (eca-chat--cached-overlay eca-chat--prompt-field-ov-cache
                            'eca-chat-prompt-field))

(defun eca-chat--prompt-field-start-point ()
  "Return the metadata overlay for the prompt field start point."
  (-some-> (eca-chat--prompt-field-ov) (overlay-start)))

(defun eca-chat--prompt-progress-field-ov ()
  "Return the overlay for the progress field."
  (eca-chat--cached-overlay eca-chat--progress-area-ov-cache
                            'eca-chat-progress-area))

(defun eca-chat--prompt-context-field-ov ()
  "Return the overlay for the context field."
  (eca-chat--cached-overlay eca-chat--context-area-ov-cache
                            'eca-chat-context-area))

(defun eca-chat--prompt-area-ov ()
  "Return the overlay for the prompt area."
  (eca-chat--cached-overlay eca-chat--prompt-area-ov-cache
                            'eca-chat-prompt-area))

(defun eca-chat--task-area-ov ()
  "Return the overlay for the task area."
  (eca-chat--cached-overlay eca-chat--task-area-ov-cache
                            'eca-chat-task-area))

(defun eca-chat--prompt-area-start-point ()
  "Return the metadata overlay for the prompt area start point."
  (-some-> (eca-chat--prompt-area-ov)
    (overlay-start)))

(defconst eca-chat--task-block-id "eca-chat-task"
  "Fixed expandable block ID for the task area widget.")

(defvar eca-chat--insertion-point-override nil
  "When non-nil, a marker overriding `eca-chat--content-insertion-point'.
Bound while prepending an older history page so the shared content
renderer inserts above existing content instead of just above the
prompt area.")

(defun eca-chat--content-insertion-point ()
  "Return the point where new chat content should be inserted.
When `eca-chat--insertion-point-override' is set, returns its position
\(used to prepend older history above existing content); otherwise the
position just before the prompt area start."
  (or (and eca-chat--insertion-point-override
           (marker-position eca-chat--insertion-point-override))
      (1- (eca-chat--prompt-area-start-point))))

(defun eca-chat--load-older-control-region ()
  "Return (BEG . END) of the load-older control, or nil when absent.
END is the position just after the control (start of the message area)."
  (let ((beg (text-property-any (point-min) (point-max) 'eca-chat-load-older t)))
    (when beg
      (cons beg (or (text-property-not-all beg (point-max) 'eca-chat-load-older t)
                    (point-max))))))

(defun eca-chat--older-content-start ()
  "Return the position where an older history page should be inserted.
Just after the load-older control when present, else the top of the buffer."
  (or (cdr (eca-chat--load-older-control-region))
      (point-min)))

(defun eca-chat--read-only-end-point ()
  "Return the position where the read-only region ends.
This is the start of the progress area, so the history, the
separator and the task area are all read-only while the progress,
context and prompt input stay editable.  Falls back to the prompt
area start when the progress overlay does not exist yet."
  (or (-some-> (eca-chat--prompt-progress-field-ov) (overlay-start))
      (eca-chat--prompt-area-start-point)))

(defun eca-chat--protect-non-prompt (&optional from)
  "Make the chat history, separator and task area read-only.
Apply the `read-only' text property from FROM (default
`point-min') up to the progress area so the history, the prompt
separator and the task area are locked, while the progress,
context and prompt input stay editable.  The first protected char
is `front-sticky' and the last is `rear-nonsticky' for
`read-only', so the locked region rejects edits while typing in
the progress/context/prompt still works.  No-op when
`eca-chat-read-only-history' is nil or the boundary is absent."
  (when eca-chat-read-only-history
    (when-let* ((end (eca-chat--read-only-end-point)))
      (let ((inhibit-read-only t)
            (beg (max (point-min) (or from (point-min)))))
        (when (< beg end)
          (put-text-property beg end 'read-only t)
          (put-text-property (1- end) end 'rear-nonsticky '(read-only))
          (when (= beg (point-min))
            (put-text-property (point-min) (1+ (point-min))
                               'front-sticky '(read-only))))))))

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
             (prompt-ov (eca-chat--prompt-field-ov))
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
         ((and prompt-ov
               (or (<= (point) (overlay-start prompt-ov))
                   (and (eq 'backward-kill-word this-command)
                        (string-blank-p (buffer-substring-no-properties
                                         (overlay-start prompt-ov) (point))))))
          (ding))

         ;; in context area trying to remove a context space separator
         ((and cur-ov
               (overlay-get cur-ov 'eca-chat-context-area)
               (and (string= " " (string (char-before (point))))
                    (not (eolp)))))

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
  (let* ((type (plist-get context :type))
         (refined (pcase type
                    ("cursor" (-> context
                                  (plist-put :path (plist-get eca-chat--cursor-context :path))
                                  (plist-put :position (plist-get eca-chat--cursor-context :position))))
                    (_ context)))
         (path (plist-get refined :path)))
    (if path
        (plist-put (copy-sequence refined) :path (eca--path-local-to-remote path))
      refined)))

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
            (setq result (concat result
                                 (cond
                                  ((eq item-type 'filepath)
                                   (eca--path-local-to-remote (substring expanded-str 1)))
                                  ((eq item-type 'context)
                                   (concat "@" (eca--path-local-to-remote
                                                (substring expanded-str 1))))
                                  (t expanded-str))))
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
    (when eca-chat--welcome-shown (eca-chat--clear))
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
     ;; The chat-id is already set buffer-locally at chat creation
     ;; time, so the prompt response carries no information we need
     ;; to act on.  Pass `#'ignore' (rather than nil) so the response
     ;; dispatcher still removes the pending-handler entry.
     :success-callback #'ignore)))

(defun eca-chat--queued-prompt-display-string (text)
  "Return a display string for queued prompt TEXT, truncated to 40 chars."
  (let* ((single-line (replace-regexp-in-string "\n" " " text))
         (truncated (if (> (length single-line) 40)
                        (concat (substring single-line 0 40) "...")
                      single-line)))
    (propertize (concat "Queued: " truncated)
                'font-lock-face 'eca-chat-queued-prompt-face)))

(defun eca-chat--transient-segment-queued ()
  "Return the queued-prompt segment string, or nil when not queued."
  (when eca-chat--queued-prompt
    (concat (eca-chat--queued-prompt-display-string eca-chat--queued-prompt)
            " "
            (eca-buttonize
             eca-chat-mode-map
             (propertize "[-]" 'font-lock-face 'eca-chat-prompt-stop-face)
             #'eca-chat--remove-queued-prompt)
            "\n")))

(defun eca-chat--update-queued-area ()
  "Refresh the transient area to reflect `eca-chat--queued-prompt'."
  (eca-chat--refresh-transient-area))

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

(defun eca-chat--remove-queued-prompt ()
  "Discard the queued prompt without sending it."
  (setq-local eca-chat--queued-prompt nil)
  (eca-chat--update-queued-area))

(defun eca-chat--steered-prompt-display-string (text)
  "Return a display string for steered prompt TEXT, truncated to 40 chars."
  (let* ((single-line (replace-regexp-in-string "\n" " " text))
         (truncated (if (> (length single-line) 40)
                        (concat (substring single-line 0 40) "...")
                      single-line)))
    (propertize (concat "Steering: " truncated)
                'font-lock-face 'eca-chat-steer-prompt-face)))

(defun eca-chat--transient-segment-steered ()
  "Return the steered-prompt segment string, or nil when not steered."
  (when eca-chat--steered-prompt
    (concat (eca-chat--steered-prompt-display-string eca-chat--steered-prompt)
            " "
            (eca-buttonize
             eca-chat-mode-map
             (propertize "[-]" 'font-lock-face 'eca-chat-prompt-stop-face)
             (lambda () (eca-chat--remove-steered-prompt (eca-session))))
            "\n")))

(defun eca-chat--update-steer-area ()
  "Refresh the transient area to reflect `eca-chat--steered-prompt'."
  (eca-chat--refresh-transient-area))

(defun eca-chat--transient-segment-loading ()
  "Return the loading/stop segment, or nil when idle.
Also returned while a question is pending so the prompt can be
stopped even if the chat reports idle awaiting the answer."
  (when (or (eq eca-chat--chat-loading t)
            eca-chat--pending-question)
    (concat (propertize eca-chat-prompt-prefix-loading
                        'font-lock-face 'default)
            (eca-buttonize
             eca-chat-mode-map
             (propertize "stop" 'font-lock-face 'eca-chat-prompt-stop-face)
             (lambda () (eca-chat--stop-prompt (eca-session))))
            "\n")))

(defvar eca-chat-transient-area-segments
  '(eca-chat--transient-segment-queued
    eca-chat--transient-segment-steered
    eca-chat--transient-segment-loading)
  "Ordered list of zero-arg functions returning a propertized string or nil.
Each non-nil result is rendered on its own line, top to bottom, between
the context area and the prompt input.  Add a new function here to make
a new dynamic line appear in that region.")

(defun eca-chat--refresh-transient-area ()
  "Re-render the transient area between context and prompt input.
Iterates `eca-chat-transient-area-segments', concatenating each
segment's non-nil string result.  Uses `insert-before-markers' so
the prompt-field overlay's start advances past inserted content."
  (when-let* ((context-ov (eca-chat--prompt-context-field-ov))
              (prompt-ov (eca-chat--prompt-field-ov)))
    (save-excursion
      (let ((start (1+ (overlay-end context-ov)))
            (end (overlay-start prompt-ov)))
        (delete-region start end)
        (goto-char start)
        (dolist (seg eca-chat-transient-area-segments)
          (when-let* ((str (funcall seg)))
            (insert-before-markers str)))
        (setq-local buffer-undo-list nil)))))

(defun eca-chat--steer-prompt (session prompt)
  "Steer the running prompt for SESSION by injecting PROMPT at the next LLM turn."
  (setq-local eca-chat--steered-prompt (if eca-chat--steered-prompt
                                           (concat eca-chat--steered-prompt "\n" prompt)
                                         prompt))
  (eca-chat--update-steer-area)
  (eca-chat--set-prompt "")
  (eca-api-notify session
                  :method "chat/promptSteer"
                  :params (list :chatId eca-chat--id
                                :message (eca-chat--normalize-prompt prompt))))

(defun eca-chat--send-steered-prompt (session)
  "Merge any unconsumed steered prompt into the queued prompt for SESSION.
Does not send directly — `eca-chat--send-queued-prompt' handles sending."
  (ignore session)
  (when eca-chat--steered-prompt
    (setq-local eca-chat--queued-prompt
                (if eca-chat--queued-prompt
                    (concat eca-chat--steered-prompt "\n" eca-chat--queued-prompt)
                  eca-chat--steered-prompt))
    (setq-local eca-chat--steered-prompt nil)
    (eca-chat--update-steer-area)
    (eca-chat--update-queued-area)))

(defun eca-chat--remove-steered-prompt (session)
  "Discard the pending steer for SESSION and notify the server."
  (setq-local eca-chat--steered-prompt nil)
  (eca-chat--update-steer-area)
  (eca-api-notify session
                  :method "chat/promptSteerRemove"
                  :params (list :chatId eca-chat--id)))

(defun eca-chat--completion-active-p ()
  "Return non-nil if a completion popup is active."
  (or (and (bound-and-true-p completion-in-region-mode))
      (and (bound-and-true-p corfu--frame)
           (frame-live-p corfu--frame)
           (frame-visible-p corfu--frame))
      (bound-and-true-p company-candidates)))

(defun eca-chat--completion-accept ()
  "Accept the current completion candidate."
  (cond
   ((and (bound-and-true-p corfu--frame)
         (frame-visible-p corfu--frame)
         (fboundp 'corfu-insert))
    (corfu-insert))
   ((and (bound-and-true-p company-candidates)
         (fboundp 'company-complete-selection))
    (company-complete-selection))
   ((bound-and-true-p completion-in-region-mode)
    (completion-at-point))))

(defun eca-chat--face-at-point-member-p (faces)
  "Return non-nil when the `face' text property at point includes any of FACES.
The `face' property may be a single symbol or a list of faces: a bare
URL wrapped in emphasis such as **https://...** carries both
`markdown-plain-url-face' and `markdown-bold-face', so a plain `eq'
against a single symbol would miss it."
  (let ((prop (get-text-property (point) 'face)))
    (seq-some (lambda (f) (memq f faces))
              (if (listp prop) prop (list prop)))))

(defun eca-chat--follow-link-at-point ()
  "Open the markdown link or URL at point in the chat buffer.
Handles inline [text](url) links as well as bare URLs, including bare
URLs wrapped in markdown emphasis like **https://...** or _https://..._
where `thing-at-point' would otherwise capture the trailing markup
characters as part of the URL."
  (if (eca-chat--face-at-point-member-p '(markdown-plain-url-face))
      (when-let* ((url (thing-at-point 'url t)))
        (browse-url (string-trim url "[*_~`]+" "[*_~`]+")))
    (markdown-follow-thing-at-point nil)))

(defun eca-chat--key-pressed-return ()
  "Send the current prompt to eca process if in prompt."
  (interactive)
  (eca-chat--allow-write
   (let* ((session (eca-session))
          (prompt (eca-chat--prompt-content)))
     (cond
      ;; check if completion popup is active
      ((eca-chat--completion-active-p)
       (eca-chat--completion-accept))

      ;; check it's an actionable text
      ((-some->> (thing-at-point 'symbol) (get-text-property 0 'eca-button-on-action))
       (-some->> (thing-at-point 'symbol)
         (get-text-property 0 'eca-button-on-action)
         (funcall)))

      ;; check is inside a expandable text
      ((eca-chat--expandable-content-at-point)
       (let ((ov (eca-chat--expandable-content-at-point)))
         (eca-chat--expandable-content-toggle (overlay-get ov 'eca-chat--expandable-content-id))))

      ;; follow markdown link [text](url) or a bare URL, even when the URL
      ;; is wrapped in emphasis like **https://...** (face is then a list,
      ;; so check membership instead of `eq' against a single symbol).
      ((eca-chat--face-at-point-member-p '(markdown-link-face
                                           markdown-url-face
                                           markdown-plain-url-face))
       (eca-chat--follow-link-at-point))

      ;; pending question + freeform allowed — answer with prompt text
      ((and eca-chat--pending-question
            (plist-get eca-chat--pending-question :allow-freeform)
            (not (string-empty-p prompt)))
       (eca-chat--answer-question prompt))

      ;; pending question — block normal send/steer
      (eca-chat--pending-question nil)

      ;; check prompt
      ((and (not (string-empty-p prompt))
            (not eca-chat--chat-loading))
       (eca-chat--send-prompt session prompt))

      ((and (not (string-empty-p prompt))
            eca-chat--chat-loading)
       (eca-chat--steer-prompt session prompt))

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
          (mcp-keymap (make-sparse-keymap))
          (gear-keymap (make-sparse-keymap)))
      (define-key model-keymap (kbd "<header-line> <mouse-1>") #'eca-chat-select-model)
      (define-key agent-keymap (kbd "<header-line> <mouse-1>") #'eca-chat-select-agent)
      (define-key variant-keymap (kbd "<header-line> <mouse-1>") #'eca-chat-select-variant)
      (define-key mcp-keymap (kbd "<header-line> <mouse-1>") #'eca-mcp-details)
      (define-key gear-keymap (kbd "<header-line> <mouse-1>") #'eca-mcp-open-menu)
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
                         'keymap mcp-keymap)
             (propertize
              " "
              'display '(space :align-to (- right 2)))
             (propertize "⚙"
                         'font-lock-face 'eca-chat-option-key-face
                         'pointer 'hand
                         'keymap gear-keymap))))))

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
                   (:session-tokens-percentage (format "%.2f%%%%"
                                                       (* 100 (/ (float eca-chat--session-tokens) eca-chat--session-limit-context))))
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

(defun eca-chat--needs-attention-p (buffer)
  "Return non-nil when chat BUFFER is waiting on the user.
A chat needs attention when it has a pending tool call approval
or an unanswered question.  A still loading chat is busy, not
waiting, so it is not considered."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'eca-chat-mode)
              (or (eca-chat--has-pending-approvals-p)
                  (and eca-chat--pending-question t))))))

(defun eca-chat-session-status (session)
  "Return the aggregated status of all chats in SESSION.
Return the symbol `waiting-approval' when any chat waits on the
user (pending tool call approval or question), `running' when any
chat is actively loading, otherwise `idle'.  A chat in the
transient `stopping' state is treated as idle since it is winding
down and visually looks idle."
  (let ((buffers (-filter #'buffer-live-p
                          (eca-vals (eca--session-chats session)))))
    (cond
     ((-first #'eca-chat--needs-attention-p buffers) 'waiting-approval)
     ((-first (lambda (buffer)
                (eq (buffer-local-value 'eca-chat--chat-loading buffer) t))
              buffers)
      'running)
     (t 'idle))))

(defun eca-chat--notify-status-changed (session)
  "Run `eca-chat-session-status-changed-functions' for SESSION.
No-op when SESSION is nil.  Called on transitions that may change the
aggregated session status so integrations can refresh."
  (when session
    (run-hook-with-args 'eca-chat-session-status-changed-functions session)))

(defun eca-chat--maybe-notify-status-changed (session content)
  "Notify a status change for SESSION when CONTENT can alter attention.
Only tool-call lifecycle events add or clear a pending approval, so the
status recompute is limited to those content types to avoid scanning the
buffer on every streamed chunk."
  (when (member (plist-get content :type)
                '("toolCallRun" "toolCallRunning" "toolCalled" "toolCallRejected"))
    (eca-chat--notify-status-changed session)))

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
  "Return FACE for TAB styled by selection and activity.
Uses `eca-tab-inactive-face' for non-selected idle
tabs, `eca-chat-tab-active-face' for selected active
tabs, and `eca-chat-tab-inactive-active-face' for
non-selected active (loading/approval) tabs."
  (let* ((buf (cdr (assq 'buffer tab)))
         (selectedp (cdr (assq 'selected tab)))
         (activep (and buf (buffer-live-p buf)
                       (or (buffer-local-value
                            'eca-chat--chat-loading buf)
                           (with-current-buffer buf
                             (eca-chat--has-pending-approvals-p))))))
    (cond
     ((and activep (not selectedp))
      `(:inherit (eca-chat-tab-inactive-active-face ,face)))
     (activep
      `(:inherit (eca-chat-tab-active-face ,face)))
     ((not selectedp)
      `(:inherit (eca-tab-inactive-face ,face)))
     (t face))))

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

(defun eca-chat-remove-workspace-root ()
  "Prompt for a workspace folder to remove from the current session.
Refuses when only one folder remains.  In `merged' worktree mode, a
removed folder sharing its git-common-dir with another session folder
may be auto-re-added on the next buffer visit."
  (interactive)
  (when-let ((session (eca-session)))
    (let ((folders (eca--session-workspace-folders session)))
      (cond
       ((null folders)
        (user-error "No workspace folders to remove"))
       ((<= (length folders) 1)
        (user-error "Cannot remove the last workspace folder"))
       (t
        (let ((folder (completing-read "Remove workspace: " folders nil t)))
          (eca--session-remove-workspace-folder session folder)
          (force-mode-line-update)))))))

(defvar eca-chat--add-workspace-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      #'eca-chat-add-workspace-root)
    map)
  "Keymap for the modeline [+] workspace button.")

(defvar eca-chat--remove-workspace-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      #'eca-chat-remove-workspace-root)
    map)
  "Keymap for the modeline [-] workspace button.")

(defvar eca-chat--trust-toggle-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      #'eca-chat-toggle-trust)
    map)
  "Keymap for the modeline trust indicator.")

(defun eca-chat--init-progress-str (session)
  "Return init progress string for SESSION, or nil when done.
Shows \"⏳ finished/total · latest-title\" while tasks
are in progress."
  (when-let* ((tasks (eca--session-init-tasks session)))
    (let* ((total (length tasks))
           (finished (length (seq-filter
                              (lambda (entry)
                                (equal "finish" (plist-get (cdr entry) :type)))
                              tasks)))
           (active (seq-filter
                    (lambda (entry)
                      (equal "start" (plist-get (cdr entry) :type)))
                    tasks)))
      (when (> (length active) 0)
        (let ((latest-title (plist-get (cdr (car active)) :title)))
          (propertize (format "⏳ %d/%d · %s" finished total latest-title)
                      'face 'shadow))))))

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
    (:remove-workspace-button
     (when (> (length (eca--session-workspace-folders session)) 1)
       (propertize " [-]"
                   'face 'shadow
                   'mouse-face 'highlight
                   'help-echo "Remove workspace folder"
                   'local-map eca-chat--remove-workspace-map)))
    (:bg-jobs
     (when-let* ((jobs (eca--session-jobs session))
                 (running (seq-count (lambda (j) (string= "running" (plist-get j :status))) jobs)))
       (when (> running 0)
         (propertize (format " [%d %s]" running (if (= running 1) "job" "jobs"))
                     'face 'shadow))))
    (:title
     (eca-chat-title))
    (:elapsed-time
     (when-let* ((str (eca-chat--turn-duration-str)))
       (let ((icon (if (eca-chat--has-pending-approvals-p) "🚧" "⏱")))
         (propertize (concat icon " " str)
                     'font-lock-face 'eca-chat-elapsed-time-face))))
    (:context-bar
     (when-let* ((bar (eca-chat--context-bar)))
       (concat bar " ")))
    (:usage
     (eca-chat--usage-str))
    (:server-version
     (when eca-chat--server-version
       (concat "ECA " eca-chat--server-version)))
    (:init-progress
     (eca-chat--init-progress-str session))
    (:trust
     (let* ((trust? (eca-chat--trust))
            (graphic? (display-graphic-p))
            (face (if trust?
                      'eca-chat-trust-on-face
                    'eca-chat-trust-off-face))
            (help (if trust?
                      "Trust ON - auto-accepting tool calls"
                    "Trust OFF - not auto-accepting tool calls"))
            (symbol (if trust?
                        (if graphic?
                            eca-chat-trust-on-symbol
                          eca-chat-trust-on-symbol-tty)
                      (if graphic?
                          eca-chat-trust-off-symbol
                        eca-chat-trust-off-symbol-tty))))
       (propertize symbol
                   'face face
                   'mouse-face 'highlight
                   'help-echo help
                   'local-map eca-chat--trust-toggle-map)))
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
      (let ((result (concat left fill right)))
        (if (eca-chat--has-pending-approvals-p)
            (propertize result 'face
                        'eca-chat-approval-modeline-face)
          result)))))

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
                    (display-action
                     `((display-buffer-in-side-window)
                       (side . ,side)
                       (slot . ,slot)
                       (dedicated . side)
                       ,@(when (memq side '(left right))
                           `((window-width . ,eca-chat-window-width)))
                       ,@(when (memq side '(top bottom))
                           `((window-height . ,eca-chat-window-height)))
                       (window-parameters . ,window-parameters))))
               (display-buffer buffer display-action))
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

(defun eca-chat--switch-to-buffer (buffer session)
  "Switch to chat BUFFER for SESSION."
  (unless (buffer-live-p buffer)
    (user-error "Chat buffer no longer exists"))
  (if-let* ((window (get-buffer-window buffer)))
      (select-window window)
    (eca-chat--display-buffer buffer))
  (setf (eca--session-last-chat-buffer session) buffer)
  buffer)

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

(defun eca-chat--fontify-region (beg end &optional loudly)
  "Custom `font-lock-fontify-region-function' for chat buffers.
Walks BEG..END in `eca-no-fontify' property runs.  For each
untagged sub-range delegates to `font-lock-default-fontify-region'
forwarding LOUDLY; tagged sub-ranges are skipped entirely, which
avoids running gfm/markdown matchers (the dominant CPU cost
reported in #234) over still-streaming tool-call argument bodies
that have not yet stabilized.

Cleanup is automatic: the `toolCalled' arm replaces the body with
fresh, un-tagged content, so on the next redisplay jit-lock asks
this function to refontify, the property is gone and the default
fontifier runs normally.

Returns `(jit-lock-bounds BEG . END)' so jit-lock's bookkeeping
matches the region we considered."
  (let ((pos beg))
    (while (< pos end)
      (let ((skip (get-text-property pos 'eca-no-fontify))
            (next (or (next-single-property-change pos 'eca-no-fontify nil end)
                      end)))
        (unless skip
          (font-lock-default-fontify-region pos next loudly))
        (setq pos next))))
  `(jit-lock-bounds ,beg . ,end))

(defun eca-chat--schedule-fontify ()
  "Schedule a deferred `font-lock-ensure' for the current chat buffer.
Cancels any previously scheduled timer.  Does nothing when
`eca-chat-fontify-debounce-interval' is nil, letting jit-lock cover
visible-area fontification and relying on the final ensure at
end-of-stream for correctness.

The scoped region runs from `eca-chat--last-user-message-pos' (the
start of the current turn) to `point-max', so cost is bounded by
the new turn instead of the full chat history."
  (when (timerp eca-chat--fontify-timer)
    (cancel-timer eca-chat--fontify-timer))
  (setq eca-chat--fontify-timer nil)
  (when (numberp eca-chat-fontify-debounce-interval)
    (let ((buf (current-buffer)))
      (setq eca-chat--fontify-timer
            (run-with-idle-timer
             eca-chat-fontify-debounce-interval nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq eca-chat--fontify-timer nil)
                   (font-lock-ensure
                    (or eca-chat--last-user-message-pos (point-min))
                    (point-max))))))))))

(defun eca-chat--copy-region-text (beg end)
  "Return text between BEG and END without copy button text."
  (let ((pos beg)
        parts)
    (while (< pos end)
      (let ((next (or (next-single-property-change
                       pos 'eca-chat--copy-button-kind nil end)
                      end)))
        (unless (get-text-property pos 'eca-chat--copy-button-kind)
          (push (buffer-substring-no-properties pos next) parts))
        (setq pos next)))
    (apply #'concat (nreverse parts))))

(defun eca-chat--copy-region (start end description)
  "Copy text between START and END and show DESCRIPTION."
  (let ((buffer (or (and (markerp start) (marker-buffer start))
                    (current-buffer)))
        (beg (if (markerp start) (marker-position start) start))
        (finish (if (markerp end) (marker-position end) end)))
    (with-current-buffer buffer
      (kill-new (string-trim-right
                 (eca-chat--copy-region-text beg finish))))
    (message "Copied %s" description)))

(defun eca-chat--copy-button-text (text callback help)
  "Return actionable copy button TEXT using CALLBACK and HELP."
  (let* ((button (eca-buttonize eca-chat-mode-map text callback))
         (callback-int (lambda (&rest _)
                         (interactive)
                         (funcall callback)))
         (map (eca-chat--copy-button-keymap button)))
    (define-key map (kbd "<mouse-1>") callback-int)
    (add-text-properties
     0 (length button)
     `(font-lock-face eca-chat-copy-button-face
       mouse-face highlight
       help-echo ,help)
     button)
    button))

(defun eca-chat--copy-button-keymap (button)
  "Return the keymap from copy BUTTON."
  (or (get-text-property 0 'keymap button)
      (get-text-property 0 'local-map button)))

(defun eca-chat--decorate-copy-button-overlay (overlay button prop)
  "Decorate OVERLAY for copy BUTTON and mark it with PROP."
  (overlay-put overlay prop t)
  (overlay-put overlay 'keymap (eca-chat--copy-button-keymap button))
  (overlay-put overlay 'local-map (eca-chat--copy-button-keymap button))
  (overlay-put overlay 'mouse-face 'highlight)
  (overlay-put overlay 'pointer 'hand)
  (overlay-put overlay 'help-echo (get-text-property 0 'help-echo button)))

(defun eca-chat--remove-copy-buttons (start end kind)
  "Remove copy button text of KIND between START and END.
Return the adjusted end position after deletion."
  (let ((end-marker (copy-marker end)))
    (save-excursion
      (goto-char start)
      (while (< (point) (marker-position end-marker))
        (let ((next (or (next-single-property-change
                         (point) 'eca-chat--copy-button-kind
                         nil end-marker)
                        (marker-position end-marker))))
          (if (eq (get-text-property
                   (point) 'eca-chat--copy-button-kind)
                  kind)
              (let ((inhibit-read-only t))
                (delete-region (point) next))
            (goto-char next)))))
    (prog1 (marker-position end-marker)
      (set-marker end-marker nil))))

(defun eca-chat--insert-copy-button (pos button kind overlay-prop)
  "Insert copy BUTTON at POS and mark it as KIND with OVERLAY-PROP.
Return the position after the inserted button."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char pos)
      (add-text-properties
       0 (length button)
       `(eca-chat--copy-button-kind ,kind
         rear-nonsticky t)
       button)
      (let ((beg (point)))
        (insert button)
        (let ((ov (make-overlay beg (point) nil t t)))
          (eca-chat--decorate-copy-button-overlay
           ov button overlay-prop))
        (setq-local buffer-undo-list nil)
        (point)))))

(defun eca-chat--code-fence-close-regexp (fence)
  "Return regexp matching the closing FENCE line."
  (concat "^[ \t]*" (regexp-quote fence) "[ \t]*$"))

(defun eca-chat--refresh-code-copy-buttons (&optional from to)
  "Refresh copy buttons for fenced code blocks between FROM and TO."
  (let* ((start (or from (point-min)))
         (end (eca-chat--remove-copy-buttons
               start (or to (point-max)) 'code))
         (limit (copy-marker end)))
    (remove-overlays start end 'eca-chat--code-copy-button t)
    (when eca-chat-show-copy-buttons
      (save-excursion
        (goto-char start)
        (while (re-search-forward "^[ \t]*\\(``+\\|~~+\\).*$" limit t)
          (let* ((open-start (line-beginning-position))
                 (fence (match-string-no-properties 1))
                 (body-start (copy-marker (1+ (line-end-position)) t))
                 (close-regexp (eca-chat--code-fence-close-regexp fence)))
            (when (re-search-forward close-regexp limit t)
              (let* ((body-end (copy-marker (match-beginning 0)))
                     (button (eca-chat--copy-button-text
                              "[copy]\n"
                              (lambda ()
                                (interactive)
                                (eca-chat--copy-region
                                 body-start
                                 body-end
                                 "code block"))
                              "Copy code block")))
                (eca-chat--insert-copy-button
                 open-start button 'code 'eca-chat--code-copy-button)))))))
    (set-marker limit nil)))

(defun eca-chat--refresh-response-copy-button (&optional from to)
  "Refresh the copy button for assistant response between FROM and TO."
  (let* ((start (or from eca-chat--last-response-copy-start))
         (end (or to (eca-chat--content-insertion-point))))
    (when (and start end (< start end))
      (setq end (eca-chat--remove-copy-buttons start end 'response))
      (remove-overlays start end 'eca-chat--response-copy-button t)
      (when (and eca-chat-show-copy-buttons
                 (not (string-empty-p
                       (string-trim
                        (buffer-substring-no-properties start end)))))
        (let ((copy-start (make-marker))
              (copy-end (copy-marker end)))
          (let* ((button (eca-chat--copy-button-text
                          "[copy response]\n"
                          (lambda ()
                            (interactive)
                            (eca-chat--copy-region
                             copy-start
                             copy-end
                             "response"))
                          "Copy response"))
                 (button-end (eca-chat--insert-copy-button
                              start button 'response
                              'eca-chat--response-copy-button)))
            (set-marker copy-start button-end (current-buffer))))))))

(defun eca-chat--refresh-copy-buttons ()
  "Refresh copy affordances for the latest assistant response."
  (let ((start eca-chat--last-response-copy-start)
        (end (eca-chat--content-insertion-point)))
    (when (and start end (< start end))
      (eca-chat--refresh-response-copy-button start end)
      (eca-chat--refresh-code-copy-buttons
       start (eca-chat--content-insertion-point)))))

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
  "Navigate to overlay matching OV-KEY in RANGE-MIN..RANGE-MAX.
When FIRST? is non-nil pick the first match, otherwise the last.
Operates on the current buffer directly — callers pass range
values from their own buffer, so switching to
`eca-chat--get-last-buffer' would apply them to the wrong
buffer when multiple sessions exist."
  (let ((get-fn (if first? #'-first #'-last)))
    (when-let ((ov (funcall get-fn (-lambda (ov) (overlay-get ov ov-key))
                            (overlays-in range-min range-max))))
      (goto-char (overlay-start ov)))))

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

  ;; Skip font-lock on ranges tagged with `eca-no-fontify', currently
  ;; used to stop gfm/markdown matchers from re-fontifying streaming
  ;; tool-call argument bodies on every chunk (see #234).
  (setq-local font-lock-fontify-region-function #'eca-chat--fontify-region)

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
        (unless eca--chat-init-skip-welcome
          (eca-chat--insert (propertize (eca--session-chat-welcome-message session)
                                        'font-lock-face 'eca-chat-welcome-face))
          (eca-chat--insert "\n")
          (eca-chat--insert (eca-buttonize
                             eca-chat-mode-map
                             (propertize "Resume a previous session"
                                         'font-lock-face 'eca-chat-resume-link-face)
                             #'eca-chat-resume))
          (setq-local eca-chat--welcome-shown t))
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
           (when eca-chat-override-mode-line
             (setq-local mode-line-format
                         (if (functionp eca-chat-mode-line-format)
                             (funcall eca-chat-mode-line-format session)
                           `(t (:eval (eca-chat--mode-line-string ,session))))))

           ;; Tab-line: show a tab for each open chat
           (when eca-chat-tab-line
             (require 'tab-line)
             (setq-local tab-line-tabs-function #'eca-chat--tab-line-tabs)
             (setq-local tab-line-new-button-show t)
             (setq-local tab-line-close-button-show t)
             (setq-local tab-line-new-tab-function #'eca-chat-new)
             (setq-local tab-line-separator "")
             (setq-local tab-line-tab-face-functions '(eca-chat--tab-line-face))
             (face-remap-add-relative 'tab-line :height 0.9)
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

  ;; Ensure markdown links look clickable regardless of theme.
  (face-remap-add-relative 'markdown-link-face
                           '(:underline t))
  (face-remap-add-relative 'markdown-plain-url-face
                           '(:underline t))

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

(defun eca-chat--handle-init-progress (session)
  "Handle init progress update for SESSION."
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
    (force-mode-line-update)))

(defun eca-chat--set-agent (session new-agent &optional buffer)
  "Set new agent to NEW-AGENT notifying server for SESSION.
When BUFFER is provided, set the agent in that buffer instead of
the last chat buffer of SESSION."
  (let* ((target (or buffer (eca-chat--get-last-buffer session)))
         (chat-id (when (buffer-live-p target)
                    (buffer-local-value 'eca-chat--id target))))
    (eca-chat--with-current-buffer target
      (setq-local eca-chat--selected-agent new-agent)
      (setq eca-chat--last-known-agent new-agent))
    (eca-api-notify session
                    :method "chat/selectedAgentChanged"
                    :params (append (list :agent new-agent)
                                    (when chat-id (list :chatId chat-id))))))

(defun eca-chat--set-trust (session value &optional buffer)
  "Set trust mode to VALUE for SESSION.
When BUFFER is provided, set in that buffer instead of
the last chat buffer of SESSION."
  (eca-chat--with-current-buffer (or buffer (eca-chat--get-last-buffer session))
    (setq-local eca-chat--selected-trust value)
    (force-mode-line-update)))

(defun eca-chat--tool-call-file-change-details
    (content label approval-text time status _tool-call-next-line-spacing roots &optional parent-id)
  "Update tool call UI showing file change details.
CONTENT is the tool call content, LABEL is the label.
Can include optional APPROVAL-TEXT and TIME.
Append STATUS, ROOTS and optional PARENT-ID."
  (-let* (((&plist :name name :details details :id id) content)
          (path (eca--path-remote-to-local (plist-get details :path)))
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
             " " (eca-chat--file-change-details-label (plist-put (copy-sequence details) :path path))
             " " status time
             (when view-diff-btn
               (concat " " view-diff-btn))
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

(defun eca-chat--response-copy-break-content-p (type)
  "Return non-nil when TYPE should restart response copy scope."
  (member type
          '("flag" "image"
            "reasonStarted" "reasonText" "reasonFinished"
            "hookActionStarted" "hookActionFinished"
            "toolCallPrepare" "toolCallRun" "toolCallRunning"
            "toolCalled" "toolCallRejected")))

(defun eca-chat--mark-response-copy-break (type parent-tool-call-id)
  "Mark TYPE as a top-level break for response copy scope.
PARENT-TOOL-CALL-ID means content belongs to a tool block."
  (when (and (not parent-tool-call-id)
             (eca-chat--response-copy-break-content-p type))
    (setq-local eca-chat--last-response-copy-start nil)
    (setq-local eca-chat--last-response-copy-kind 'break)))

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
         (content-type (plist-get content :type))
         (tool-call-next-line-spacing (make-string (1+ (length eca-chat-expandable-block-open-symbol)) ?\s)))
    (pcase content-type
      ("metadata"
       (unless parent-tool-call-id
         (setq-local eca-chat--title (plist-get content :title))))
      ("text"
       (when-let* ((text (plist-get content :text)))
         (pcase role
           ("user"
            (unless parent-tool-call-id
              ;; Capture the insertion point before adding the user
              ;; expandable so we can scope `font-lock-ensure' to just
              ;; the newly-inserted region instead of the whole buffer.
              (let ((user-msg-start (eca-chat--content-insertion-point)))
                (when eca-chat--steered-prompt
                  (setq-local eca-chat--steered-prompt nil)
                  (eca-chat--update-steer-area))
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
                (setq-local eca-chat--last-response-copy-start nil)
                (setq-local eca-chat--last-response-copy-kind nil)
                (eca-chat--mark-header)
                (font-lock-ensure user-msg-start (point-max)))))
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
              (unless (eq eca-chat--last-response-copy-kind 'text)
                (setq-local eca-chat--last-response-copy-start
                            (eca-chat--content-insertion-point)))
              (setq-local eca-chat--last-response-copy-kind 'text)
              (eca-chat--add-text-content text)
              ;; Defer fontification: let jit-lock handle visible-area
              ;; updates and run a single final ensure in the
              ;; "finished" progress arm below.
              (eca-chat--schedule-fontify))))))
      ("url"
       (unless parent-tool-call-id
         (eca-chat--add-header
          (concat "🌐 "
                  (eca-buttonize
                   eca-chat-mode-map
                   (plist-get content :title)
                   (lambda () (browse-url (plist-get content :url))))
                  "\n\n"))))
      ("image"
       (eca-chat--render-image-content content parent-tool-call-id))
      ("flag"
       (let* ((flag-text (plist-get content :text))
              (flag-content-id (plist-get content :contentId))
              (flag-str (propertize (concat "🚩️️ " flag-text)
                                    'font-lock-face 'eca-chat-flag-face))
              (fork-btn (eca-buttonize
                         eca-chat-mode-map
                         (propertize "Fork from here" 'font-lock-face 'eca-chat-rollback-face)
                         (lambda () (eca-chat--fork-from-flag session flag-content-id))))
              (remove-btn (eca-buttonize
                           eca-chat-mode-map
                           (propertize "Remove flag" 'font-lock-face 'eca-chat-rollback-face)
                           (lambda () (eca-chat--remove-flag session flag-content-id))))
              (actions (concat fork-btn "\n" remove-btn)))
         (eca-chat--add-expandable-content flag-content-id flag-str actions)
         (when-let* ((ov (eca-chat--get-expandable-content flag-content-id)))
           (overlay-put ov 'eca-chat--flag-text flag-text)
           (overlay-put ov 'eca-chat--timestamp (float-time)))))
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
         (when-let* ((id (plist-get content :id))
                     (name (plist-get content :name))
                     (server (plist-get content :server)))
           (let* ((argsText (plist-get content :argumentsText))
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
                      ;; Tag body and accumulated content with `eca-no-fontify' so
                      ;; the custom `font-lock-fontify-region-function' skips them
                      ;; while the tool args are still streaming (#234).  The
                      ;; property travels through `eca-chat--insert' and
                      ;; `eca-chat--update-expandable-content' to every code
                      ;; path that lands the body in the buffer; the `toolCalled'
                      ;; arm later reinserts un-tagged final content, so jit-lock
                      ;; refontifies normally on the next redisplay.
                      (body (if subagent?
                                (eca-chat--content-table `())
                              (propertize
                               (eca-chat--content-table
                                `(("Tool" . ,name)
                                  ("Server" . ,server)
                                  ("Arguments" . ,new-content)))
                               'eca-no-fontify t)))
                      (update-content (if subagent?
                                          body
                                        (propertize new-content 'eca-no-fontify t))))
                 (if (eca-chat--get-expandable-content id)
                     ;; Update with accumulated content, not just this chunk
                     (eca-chat--update-expandable-content
                      id label update-content nil parent-tool-call-id)
                   (eca-chat--add-expandable-content
                    id label body parent-tool-call-id))))))))
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
                (bg? (plist-get details :background))
                (status (cond
                         ((plist-get content :error)
                          eca-chat-mcp-tool-call-error-symbol)
                         (bg? "🟡")
                         (t eca-chat-mcp-tool-call-success-symbol))))
           ;; Cleanup counters for this tool-call id to avoid unbounded growth
           (remhash id eca-chat--tool-call-prepare-counters)
           (remhash id eca-chat--tool-call-prepare-content-cache)
           ;; Stop elapsed-time tracking for this tool call
           (eca-chat--tool-call-elapsed-stop id)
           ;; Another client may have answered this ask_user question first,
           ;; resolving the tool call here; drop our now-stale prompt state.
           (eca-chat--dismiss-pending-question-for-tool-call id)
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
           ;; Another client may have cancelled this ask_user question first,
           ;; rejecting the tool call here; drop our now-stale prompt state.
           (eca-chat--dismiss-pending-question-for-tool-call id)
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
            (pcase eca-chat--chat-loading
              ('stopping
               ;; Stopped prompt confirmed — minimal cleanup, no trailing newline
               (setq-local eca-chat--progress-text "")
               (eca-chat--spinner-stop)
               (eca-chat--tool-call-elapsed-stop-all)
               (when (timerp eca-chat--fontify-timer)
                 (cancel-timer eca-chat--fontify-timer)
                 (setq eca-chat--fontify-timer nil))
               ;; Region-scoped fontify: only the current turn needs
               ;; (re)fontification at end-of-stream; full-buffer
               ;; fontify is O(buffer size) and was the dominant
               ;; cost on long chats.
               (font-lock-ensure (or eca-chat--last-user-message-pos (point-min))
                                 (point-max))
               (eca-chat--refresh-copy-buttons)
               (eca-chat--set-chat-loading session nil)
               (eca-chat--refresh-progress chat-buffer)
               (eca-chat--send-steered-prompt session)
               (eca-chat--send-queued-prompt session)
               (run-hooks 'eca-chat-finished-hook))
              ('t
               ;; Normal completion
               (setq-local eca-chat--progress-text "")
               (eca-chat--spinner-stop)
               (eca-chat--tool-call-elapsed-stop-all)
               (eca-chat--add-text-content "\n")
               (when (timerp eca-chat--fontify-timer)
                 (cancel-timer eca-chat--fontify-timer)
                 (setq eca-chat--fontify-timer nil))
               ;; Final guaranteed fontify before table alignment so
               ;; the beautifier sees fully-fontified text.  Scoped
               ;; to the current turn so cost does not grow with
               ;; chat history; previous turns were already
               ;; fontified at their own end-of-stream.
               (font-lock-ensure (or eca-chat--last-user-message-pos (point-min))
                                 (point-max))
               (eca-chat--refresh-copy-buttons)
               ;; Table align/beautify default to scanning from
               ;; `eca-chat--last-user-message-pos' when called with
               ;; no argument, scoping work to the current turn.
               (eca-chat--align-tables)
               (eca-chat--beautify-tables)
               (eca-chat--set-chat-loading session nil)
               (eca-chat--refresh-progress chat-buffer)
               (eca-chat--send-steered-prompt session)
               (eca-chat--send-queued-prompt session)
               (run-hooks 'eca-chat-finished-hook))
              (_
               ;; Idempotent UI cleanup for a `finished' arriving while
               ;; `chat-loading' is nil — e.g. server-driven progress not
               ;; started via `eca-chat--send-prompt', or after the 10s
               ;; stopping safety-timer already cleared the flag.  We still
               ;; clear the visible spinner / progress text and stop the
               ;; elapsed timers, but intentionally skip the trailing
               ;; newline, fontify and queued/steered-prompt dispatch so a
               ;; duplicate `finished' (the case 19aa392 guarded against)
               ;; still cannot insert a second newline or re-trigger a
               ;; queued prompt.
               (setq-local eca-chat--progress-text "")
               (eca-chat--spinner-stop)
               (eca-chat--tool-call-elapsed-stop-all)
               (eca-chat--refresh-progress chat-buffer)))))))
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
           (setq-local eca-chat--session-cost          (plist-get content :sessionCost))
           (setq-local eca-chat--context-breakdown     (plist-get content :contextBreakdown)))
         (force-mode-line-update)))
      (_ nil))
    (eca-chat--mark-response-copy-break
     content-type parent-tool-call-id)))

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
              (eca-chat--render-content session parent-buffer role content roots tool-call-id chat-id)
              (eca-chat--protect-non-prompt eca-chat--last-user-message-pos)
              (eca-chat--maybe-notify-status-changed session content))))
      ;; Normal content
      (when-let* ((chat-buffer (eca-chat--get-chat-buffer session chat-id))
                  ((buffer-live-p chat-buffer)))
        (eca-chat--with-current-buffer chat-buffer
          (eca-chat--render-content session chat-buffer role content roots)
          (eca-chat--protect-non-prompt eca-chat--last-user-message-pos)
          (eca-chat--maybe-notify-status-changed session content))))))

(defun eca-chat--render-history-contents (session chat-buffer contents)
  "Prepend CONTENTS above existing content in CHAT-BUFFER for SESSION.
CONTENTS is the list of `chat/history' items (each shaped like a
`chat/contentReceived' payload); they are rendered in order at the top
of the message area so older pages stack above newer ones.  Subagent
items (those carrying :parentChatId) are routed into their parent tool
call exactly as the streaming path does.

The live-turn marker `eca-chat--last-user-message-pos' is saved and
restored so prepending older content does not move the scope used by the
streaming renderer; fontification and table styling are applied to the
prepended region explicitly.  Callers are expected to re-apply
`eca-chat--protect-non-prompt' afterwards."
  (eca-chat--with-current-buffer chat-buffer
    (let* ((roots (eca--session-workspace-folders session))
           (start (eca-chat--older-content-start))
           (m (copy-marker start t))
           (saved-last-user-pos eca-chat--last-user-message-pos))
      (unwind-protect
          (let ((eca-chat--insertion-point-override m))
            (seq-do
             (lambda (item)
               (let ((role (plist-get item :role))
                     (content (plist-get item :content))
                     (parent-chat-id (plist-get item :parentChatId))
                     (item-chat-id (plist-get item :chatId)))
                 (if parent-chat-id
                     (when-let* ((tool-call-id (gethash item-chat-id eca-chat--subagent-chat-id->tool-call-id)))
                       (eca-chat--render-content session chat-buffer role content roots tool-call-id item-chat-id))
                   (eca-chat--render-content session chat-buffer role content roots))))
             contents))
        (setq-local eca-chat--last-user-message-pos saved-last-user-pos)
        ;; Separate the prepended block from the content below with a single
        ;; newline: historical content has no trailing turn-end newline, so the
        ;; last older line would otherwise glue to the first existing line.
        (let ((pos (marker-position m)))
          (when (and (> pos (point-min))
                     (< pos (point-max))
                     (not (eq (char-before pos) ?\n))
                     (not (eq (char-after pos) ?\n)))
            (save-excursion (goto-char pos) (insert "\n"))))
        (let ((end (marker-position m)))
          (font-lock-ensure start end)
          (eca-chat--align-tables start)
          (eca-chat--beautify-tables start))
        (set-marker m nil)))))

(defun eca-chat--apply-history-meta (meta)
  "Update buffer-local history pagination cursors from META plist."
  (when meta
    (setq-local eca-chat--history-before-cursor (plist-get meta :beforeCursor))
    (setq-local eca-chat--history-after-cursor (plist-get meta :afterCursor))
    (setq-local eca-chat--history-compaction-cursor (plist-get meta :compactionCursor))
    (setq-local eca-chat--history-total (plist-get meta :total))))

(defun eca-chat--refresh-load-older-control ()
  "Insert or remove the clickable \"Load older messages\" control.
Shown at the top of the buffer only when an older page is available
\(`eca-chat--history-before-cursor' is non-nil)."
  (let ((inhibit-read-only t))
    (save-excursion
      (when-let* ((region (eca-chat--load-older-control-region)))
        (delete-region (car region) (cdr region)))
      (when eca-chat--history-before-cursor
        (goto-char (point-min))
        (let ((beg (point)))
          (eca-chat--insert
           (eca-buttonize
            eca-chat-mode-map
            (propertize "Load older messages"
                        'font-lock-face 'eca-chat-load-more-face)
            #'eca-chat-load-older-history)
           "\n")
          (put-text-property beg (point) 'eca-chat-load-older t))))))

(defun eca-chat-load-older-history ()
  "Load and prepend the previous (older) page of this chat's history."
  (interactive)
  (let ((session (eca-session))
        (buffer (current-buffer))
        (chat-id eca-chat--id))
    (cond
     ((not eca-chat--history-before-cursor)
      (message "No older messages to load."))
     (eca-chat--history-loading
      (message "Already loading older messages…"))
     (t
      (setq-local eca-chat--history-loading t)
      (eca-api-request-async
       session
       :method "chat/history"
       :params (append (list :chatId chat-id
                             :before eca-chat--history-before-cursor)
                       (when eca-chat-history-page-size
                         (list :limit eca-chat-history-page-size)))
       :success-callback
       (lambda (res)
         (when (buffer-live-p buffer)
           (eca-chat--with-current-buffer buffer
             (setq-local eca-chat--history-loading nil)
             (if-let* ((err (plist-get res :error)))
                 (if (string= (plist-get err :code) "cursor_expired")
                     (message "Chat history changed; reopen the chat to continue paging.")
                   (message "Failed to load older messages: %s" (plist-get err :message)))
               (progn
                 (eca-chat--render-history-contents session buffer (append (plist-get res :contents) nil))
                 (eca-chat--apply-history-meta (plist-get res :meta))
                 (eca-chat--refresh-load-older-control)
                 (eca-chat--protect-non-prompt))))))
       :error-callback
       (lambda (err)
         (when (buffer-live-p buffer)
           (eca-chat--with-current-buffer buffer
             (setq-local eca-chat--history-loading nil)
             (message "Failed to load older messages: %s" err)))))))))

(defun eca-chat-cleared (session params)
  "Clear chat for SESSION and PARAMS requested by server."
  (-let* ((chat-id (plist-get params :chatId))
          (messages? (plist-get params :messages))
          (chat-buffer (eca-chat--get-chat-buffer session chat-id)))
    (when (buffer-live-p chat-buffer)
      (eca-chat--with-current-buffer chat-buffer
        (when messages?
          (eca-chat--clear))))))

(defun eca-chat--apply-per-chat-config (chat-config buffer)
  "Apply the per-chat fields of CHAT-CONFIG to BUFFER's local state.
Used by `eca-chat-config-updated' to drive a single chat's UI from
a `config/updated' broadcast."
  (with-current-buffer buffer
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
    ;; Server-driven trust restore on chat resume (eca #426): keep the
    ;; mode-line shield/flame indicator in sync with the persisted
    ;; per-chat trust state so it matches the server's auto-approval
    ;; behavior for subsequent tool calls.
    (when (plist-member chat-config :selectTrust)
      (let ((new-trust (eq t (plist-get chat-config :selectTrust))))
        (setq-local eca-chat--selected-trust new-trust)
        (setq eca-chat--last-known-trust new-trust)))
    (force-mode-line-update)))

(defun eca-chat-config-updated (session chat-config)
  "Update chat based on the CHAT-CONFIG for SESSION.

Session-level fields (welcomeMessage, models, agents, variants) are
always applied to the session record.  Per-chat fields (selectModel,
selectAgent, selectVariant, selectTrust) are scoped by `chatId':

- when CHAT-CONFIG contains a `chatId' the per-chat fields apply only
  to that chat's buffer (eca-emacs#231 - prevents one chat's model
  change from leaking into other chats);

- when no `chatId' is present the legacy session-wide path is used
  (per-chat fields broadcast to every chat buffer).  This path is still
  needed for the initial `config/updated' the server sends right after
  `initialize' which pushes session-default model/agent to all chats."
  (-some->> (plist-get chat-config :welcomeMessage)
    (setf (eca--session-chat-welcome-message session)))
  (-some->> (plist-get chat-config :models)
    (setf (eca--session-models session)))
  (-some->> (plist-get chat-config :agents)
    (setf (eca--session-chat-agents session)))
  (when (plist-member chat-config :variants)
    (setf (eca--session-chat-variants session)
          (append (plist-get chat-config :variants) nil)))
  (if-let* ((chat-id (plist-get chat-config :chatId)))
      (when-let* ((chat-buffer (eca-get (eca--session-chats session) chat-id))
                  ((buffer-live-p chat-buffer)))
        (eca-chat--apply-per-chat-config chat-config chat-buffer))
    (seq-doseq (chat-buffer (eca-vals (eca--session-chats session)))
      (when (buffer-live-p chat-buffer)
        (eca-chat--apply-per-chat-config chat-config chat-buffer)))))

(defun eca-chat-deleted (session params)
  "Handle chat deleted notification for SESSION with PARAMS."
  (let* ((chat-id (plist-get params :chatId))
         (chat-buffer (eca-get (eca--session-chats session) chat-id)))
    (when chat-buffer
      (setf (eca--session-chats session)
            (eca-dissoc (eca--session-chats session) chat-id))
      (when (buffer-live-p chat-buffer)
        (kill-buffer chat-buffer)))))

(defun eca-chat-opened (session params)
  "Handle chat/opened notification for SESSION with PARAMS.
Idempotent: if the chat-id is already known on the client side
\(e.g. a client-initiated chat where the client minted the id and
the server is just announcing the new chat to other observers\),
update the title and return without creating a duplicate buffer.
Otherwise, creates a new chat buffer for a server-initiated chat
\(e.g. /fork or replay via chat/open\) and registers it under the
real chat-id so subsequent `chat/contentReceived' notifications
render into it.

A registered buffer marked `eca-chat--closed' (left behind by
`eca-chat-exit' on restart) is treated as stale, not reused, so a
resumed chat gets a fresh writable buffer."
  (let* ((chat-id (plist-get params :chatId))
         (title (plist-get params :title))
         (existing (eca-get (eca--session-chats session) chat-id)))
    (cond
     ((and existing (buffer-live-p existing)
           (not (buffer-local-value 'eca-chat--closed existing)))
      ;; Already known: propagate title (if any) but do not duplicate.
      (when title
        (with-current-buffer existing
          (setq-local eca-chat--title title)))
      (eca-chat--force-tab-line-update))
     (t
      ;; Any live buffer reaching here is a stale closed one; kill it so
      ;; it doesn't linger as an orphan `:closed' buffer.
      (when (and existing (buffer-live-p existing))
        (kill-buffer existing))
      (cl-incf eca-chat--new-chat-id)
      (let ((new-buffer (eca-chat--create-buffer session)))
        (with-current-buffer new-buffer
          (let ((eca--chat-init-session session)
                (eca--chat-init-skip-welcome t))
            (eca-chat-mode))
          (setq-local eca-chat--id chat-id)
          (setq-local eca-chat--title title)
          (setq-local eca-chat--selected-agent eca-chat--last-known-agent)
          (setq-local eca-chat--selected-model eca-chat--last-known-model)
          (setq-local eca-chat--selected-variant eca-chat--last-known-variant)
          (setq-local eca-chat--selected-trust eca-chat--last-known-trust))
        (setf (eca--session-chats session)
              (eca-assoc (eca--session-chats session) chat-id new-buffer))
        (eca-chat--force-tab-line-update))))))

(defun eca-chat-status-changed (session params)
  "Handle chat status changed notification for SESSION with PARAMS.
Synthesizes progress content-received events to update the
spinner.  Subagent chats (which have no dedicated buffer) are
silently ignored."
  (let* ((chat-id (plist-get params :chatId))
         (status (plist-get params :status))
         (chat-buffer (eca-get (eca--session-chats session) chat-id)))
    (when (and chat-buffer (buffer-live-p chat-buffer))
      (eca-chat--with-current-buffer chat-buffer
        (pcase status
          ("running"
           (eca-chat--set-chat-loading session t)
           (eca-chat-content-received session
                                      (list :chatId chat-id :role "system"
                                            :content (list :type "progress" :state "running" :text "Running..."))))
          ("idle"
           (eca-chat-content-received session
                                      (list :chatId chat-id :role "system"
                                            :content (list :type "progress" :state "finished")))))))))

;;; Ask question

(defun eca-chat-handle-ask-question (session request params)
  "Handle chat/askQuestion REQUEST for SESSION with PARAMS.
Renders the question in the chat buffer and switches the prompt
to answer mode.  Returns :async — the response is sent later when
the user answers or cancels."
  (let* ((chat-id (plist-get params :chatId))
         (question (plist-get params :question))
         ;; Guard against a non-sequence `:options` (e.g. a malformed
         ;; string from a misbehaving server): `append' on a string would
         ;; split it into character integers that render as random numbers.
         (options (let ((raw (plist-get params :options)))
                    (when (or (listp raw) (vectorp raw))
                      (append raw nil))))
         (tool-call-id (plist-get params :toolCallId))
         (allow-freeform (plist-get params :allowFreeform))
         (chat-buffer (eca-chat--get-chat-buffer session chat-id)))
    (if (and chat-buffer (buffer-live-p chat-buffer))
        (progn
          (eca-chat--with-current-buffer chat-buffer
            (setq eca-chat--pending-question
                  (list :session session :request request
                        :question question :options options
                        :tool-call-id tool-call-id
                        :allow-freeform allow-freeform))
            (if tool-call-id
                (progn
                  (eca-chat--update-expandable-content
                   tool-call-id
                   (propertize (concat "Q: " question)
                               'font-lock-face 'eca-chat-question-face)
                   (eca-chat--build-question-options-content options))
                  (eca-chat--expandable-content-toggle tool-call-id t nil))
              (eca-chat--render-ask-question-standalone question options))
            (when allow-freeform
              (eca-chat--set-question-prompt-prefix t))
            ;; A pending question keeps the turn active server-side, so
            ;; surface the stop affordance even if the chat reports idle.
            (eca-chat--refresh-transient-area)
            (eca-chat--notify-status-changed session))
          :async)
      (list :answer nil :cancelled t))))

(defun eca-chat--normalize-question-option (opt)
  "Return a cons (LABEL . DESCRIPTION) for question OPT.
OPT may be a plist with :label/:description or a plain string.  LABEL is
always a non-nil string so option rendering never fails on bad data."
  (let* ((label (cond ((stringp opt) opt)
                      ((listp opt) (plist-get opt :label))))
         (desc (and (listp opt) (plist-get opt :description))))
    (cons (if (stringp label) label (format "%s" (or label opt)))
          (and (stringp desc) desc))))

(defun eca-chat--build-question-options-content (options)
  "Build expandable block content string with OPTIONS and a cancel button."
  (concat
   (when options
     (mapconcat
      (lambda (opt)
        (let* ((ld (eca-chat--normalize-question-option opt))
               (label (car ld))
               (desc (cdr ld))
               (btn (eca-buttonize
                     eca-chat-mode-map
                     (propertize label 'font-lock-face 'eca-chat-question-option-face)
                     (lambda ()
                       (eca-chat--answer-question label)))))
          (concat btn
                  (when desc
                    (concat "  " (propertize desc 'font-lock-face 'eca-chat-question-description-face)))
                  "\n")))
      options
      ""))
   (eca-buttonize
    eca-chat-mode-map
    (propertize "Cancel" 'font-lock-face '(error :underline t))
    #'eca-chat--cancel-question)
   "\n"))

(defun eca-chat--question-block-ov ()
  "Return the overlay marking the standalone question block."
  (eca-chat--cached-overlay eca-chat--question-block-ov-cache
                            'eca-chat--question-block))

(defun eca-chat--render-ask-question-standalone (question options)
  "Insert a standalone question block with QUESTION and OPTIONS.
Used as fallback when no toolCallId is available."
  (save-excursion
    (goto-char (eca-chat--content-insertion-point))
    (eca-chat--insert
     (concat "\n"
             (propertize (concat "Q: " question)
                         'font-lock-face 'eca-chat-question-face)
             "\n"))
    (let ((block-start (point)))
      (eca-chat--insert
       (concat
        (when options
          (concat
           "\n"
           (mapconcat
            (lambda (opt)
              (let* ((ld (eca-chat--normalize-question-option opt))
                     (label (car ld))
                     (desc (cdr ld))
                     (btn (eca-buttonize
                           eca-chat-mode-map
                           (propertize label 'font-lock-face 'eca-chat-question-option-face)
                           (lambda ()
                             (eca-chat--answer-question label)))))
                (concat "  " btn
                        (when desc
                          (concat "  " (propertize desc 'font-lock-face 'eca-chat-question-description-face)))
                        "\n")))
            options
            "")))
        "\n  "
        (eca-buttonize
         eca-chat-mode-map
         (propertize "Cancel" 'font-lock-face '(error :underline t))
         #'eca-chat--cancel-question)
        "\n\n"))
      (let ((ov (make-overlay block-start (point))))
        (overlay-put ov 'eca-chat--question-block t)))))

(defun eca-chat--collapse-question-block (text)
  "Replace the standalone question block with TEXT."
  (when-let* ((ov (eca-chat--question-block-ov)))
    (let ((start (overlay-start ov))
          (end (overlay-end ov))
          (inhibit-read-only t))
      (delete-overlay ov)
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (eca-chat--insert text)))))

(defun eca-chat--answer-question (answer)
  "Send ANSWER for the pending question and restore normal prompt."
  (when-let* ((pending eca-chat--pending-question))
    (let ((session (plist-get pending :session))
          (request (plist-get pending :request))
          (tool-call-id (plist-get pending :tool-call-id))
          (allow-freeform (plist-get pending :allow-freeform)))
      (setq eca-chat--pending-question nil)
      (when allow-freeform
        (eca-chat--set-question-prompt-prefix nil))
      (eca-chat--allow-write
       (if tool-call-id
           (eca-chat--update-expandable-content
            tool-call-id nil
            (concat (propertize (concat "→ " answer)
                                'font-lock-face 'eca-chat-question-option-face)
                    "\n"))
         (eca-chat--collapse-question-block
          (concat (propertize (concat "→ " answer)
                              'font-lock-face 'eca-chat-question-option-face)
                  "\n\n")))
       (eca-chat--set-prompt "")
       (eca-chat--refresh-transient-area))
      (eca-api-send-request-response
       session request
       (list :answer answer :cancelled :json-false))
      (eca-chat--notify-status-changed session))))

(defun eca-chat--cancel-question ()
  "Cancel the pending question and restore normal prompt."
  (when-let* ((pending eca-chat--pending-question))
    (let ((session (plist-get pending :session))
          (request (plist-get pending :request))
          (tool-call-id (plist-get pending :tool-call-id))
          (allow-freeform (plist-get pending :allow-freeform)))
      (setq eca-chat--pending-question nil)
      (when allow-freeform
        (eca-chat--set-question-prompt-prefix nil))
      (eca-chat--allow-write
       (if tool-call-id
           (eca-chat--update-expandable-content
            tool-call-id nil
            (concat (propertize "✗ Cancelled"
                                'font-lock-face 'font-lock-comment-face)
                    "\n"))
         (eca-chat--collapse-question-block
          (concat (propertize "✗ Cancelled"
                              'font-lock-face 'font-lock-comment-face)
                  "\n\n")))
       (eca-chat--refresh-transient-area))
      (eca-api-send-request-response
       session request
       (list :answer nil :cancelled t))
      (eca-chat--notify-status-changed session))))

(defun eca-chat--dismiss-pending-question-for-tool-call (tool-call-id)
  "Dismiss the pending question when it belongs to TOOL-CALL-ID.
Used when another client answers the same `ask_user' question first, so
the server resolves the tool call out from under us.  The tool output is
rendered by the caller; here we only restore the local question state
and prompt so this client does not stay stuck in answer mode."
  (when (and eca-chat--pending-question
             (equal tool-call-id
                    (plist-get eca-chat--pending-question :tool-call-id)))
    (let ((allow-freeform (plist-get eca-chat--pending-question :allow-freeform)))
      (setq eca-chat--pending-question nil)
      (when allow-freeform
        (eca-chat--set-question-prompt-prefix nil))
      (eca-chat--refresh-transient-area)
      (eca-chat--notify-status-changed (ignore-errors (eca-session))))))

(defun eca-chat--set-question-prompt-prefix (active)
  "Toggle the prompt prefix for question mode.
When ACTIVE is non-nil, show the question prefix; otherwise restore normal."
  (when-let* ((ov (eca-chat--prompt-field-ov)))
    (overlay-put ov 'before-string
                 (propertize (if active
                                 eca-chat-prompt-prefix-question
                               eca-chat-prompt-prefix)
                             'font-lock-face 'eca-chat-prompt-prefix-face))))

(defun eca-chat-open (session)
  "Open or create dedicated eca chat window for SESSION."
  (eca-assert-session-running session)
  (unless (buffer-live-p (eca-chat--get-last-buffer session))
    (eca-chat--create-buffer session))
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
    (unless (derived-mode-p 'eca-chat-mode)
      (let ((eca--chat-init-session session))
        (eca-chat-mode))
      ;; Generate the chat-id eagerly so every chat/* request (including
      ;; the very first chat/prompt) carries a real id.  The server
      ;; treats a previously-unknown id as a new empty chat.
      (setq-local eca-chat--id (eca-uuid))
      (setq-local eca-chat--selected-agent eca-chat--last-known-agent)
      (setq-local eca-chat--selected-model eca-chat--last-known-model)
      (setq-local eca-chat--selected-variant eca-chat--last-known-variant)
      (setq-local eca-chat--selected-trust eca-chat--last-known-trust)
      (eca-chat--track-cursor-position-schedule)
      (when eca-chat-auto-add-cursor
        (eca-chat--add-context (list :type "cursor")))
      (when eca-chat-auto-add-repomap
        (eca-chat--add-context (list :type "repoMap"))))
    (unless (member (current-buffer) (eca-vals (eca--session-chats session)))
      (cl-assert eca-chat--id nil "eca-chat--id must be set before registering buffer")
      (setf (eca--session-chats session)
            (eca-assoc (eca--session-chats session) eca-chat--id (current-buffer))))
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
                  ;; Cancel all timers if chat was still loading/stopping.
                  (eca-chat--spinner-stop)
                  (eca-chat--tool-call-elapsed-stop-all)
                  (when eca-chat--modeline-timer
                    (cancel-timer eca-chat--modeline-timer)
                    (setq-local eca-chat--modeline-timer nil))
                  (when eca-chat--stopping-safety-timer
                    (cancel-timer eca-chat--stopping-safety-timer)
                    (setq-local eca-chat--stopping-safety-timer nil))
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
    ;; Target the chat the user is interacting with (the current buffer
    ;; when invoked from inside a chat, otherwise the session's
    ;; last-chat-buffer fallback), not whichever chat was last globally
    ;; tracked.  This keeps the model selection from leaking across
    ;; chats in the session (eca-emacs#231).
    (let* ((target (if (derived-mode-p 'eca-chat-mode)
                       (current-buffer)
                     (eca-chat--get-last-buffer (eca-session))))
           (chat-id (when (buffer-live-p target)
                      (buffer-local-value 'eca-chat--id target)))
           (variant (when (buffer-live-p target)
                      (buffer-local-value 'eca-chat--selected-variant target))))
      (eca-chat--with-current-buffer target
        (setq-local eca-chat--selected-model model)
        (setq eca-chat--last-known-model model))
      (eca-api-notify (eca-session)
                      :method "chat/selectedModelChanged"
                      :params (append (list :model model :variant variant)
                                      (when chat-id (list :chatId chat-id)))))))

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
      ;; Target the active chat buffer (see `eca-chat-select-model').
      (let ((target (if (derived-mode-p 'eca-chat-mode)
                        (current-buffer)
                      (eca-chat--get-last-buffer (eca-session)))))
        (eca-chat--with-current-buffer target
          (setq-local eca-chat--selected-variant variant)
          (setq eca-chat--last-known-variant variant))))))

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
(defun eca-chat-add-flag ()
  "Add a named flag to the current chat.
The flag is placed after the nearest message block at or before
point.  Works with any message type (user, tool call, etc)."
  (interactive)
  (eca-assert-session-running (eca-session))
  (let ((nearest-id nil)
        (nearest-pos -1))
    (dolist (ov (overlays-in (point-min) (1+ (point))))
      (when-let* ((id (overlay-get ov 'eca-chat--expandable-content-id))
                  (pos (overlay-start ov)))
        (when (and (> pos nearest-pos)
                   (not (overlay-get ov 'eca-chat--flag-text)))
          (setq nearest-id id nearest-pos pos))))
    (if nearest-id
        (when-let* ((flag-text (read-string "Flag: ")))
          (unless (string-empty-p flag-text)
            (eca-api-request-sync (eca-session)
                                  :method "chat/addFlag"
                                  :params (list :chatId eca-chat--id
                                                :contentId nearest-id
                                                :text flag-text))))
      (message "No message found before point"))))

;;;###autoload
(defun eca-chat-toggle-trust ()
  "Toggle trust mode (auto-accept all tool call).
Sends chat/update to server so trust applies immediately to the next tool call."
  (interactive)
  (let ((new-value (not (eca-chat--trust))))
    (eca-chat--set-trust (eca-session) new-value (current-buffer))
    (when eca-chat--id
      (eca-api-request-sync (eca-session)
                            :method "chat/update"
                            :params (list :chatId eca-chat--id
                                          :trust (if new-value t :json-false))))
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

(defun eca-chat--session-chats-oldest-first (session)
  "Return SESSION's chat buffers ordered oldest-first."
  (nreverse (eca-vals (eca--session-chats session))))

(defun eca-chat--rotate-after (items pos)
  "Return ITEMS rotated to start right after position POS.
The element at POS is moved to the end so it is visited last.
When POS is nil, return ITEMS unchanged."
  (if pos
      (append (nthcdr (1+ pos) items)
              (cl-subseq items 0 (1+ pos)))
    items))

;;;###autoload
(defun eca-chat-go-to-next-attention-in-project ()
  "Go to the next chat of the current project needing attention.
A chat needs attention when it is waiting on you: a tool call
pending approval or an unanswered question.  Cycles through the
current session's chats, wrapping around, so repeated invocations
visit every chat that is waiting."
  (interactive)
  (let ((session (eca-session)))
    (eca-assert-session-running session)
    (let* ((chats (eca-chat--session-chats-oldest-first session))
           ;; Start searching after the current chat (wrapping) so each
           ;; call advances instead of re-selecting the current one.
           (ordered (eca-chat--rotate-after chats
                                            (cl-position (current-buffer)
                                                         chats)))
           (target (seq-find #'eca-chat--needs-attention-p ordered)))
      (if target
          (eca-chat--switch-to-buffer target session)
        (eca-info "No chat needs attention in this project")))))

;;;###autoload
(defun eca-chat-go-to-next-attention ()
  "Go to the next chat needing attention, cycling across projects.
A chat needs attention when it is waiting on you: a tool call
pending approval or an unanswered question.  Exhausts the current
project's chats first, then moves on to the next project (ECA
session), eventually visiting them all and wrapping around."
  (interactive)
  (let ((sessions (sort (copy-sequence (eca-vals eca--sessions))
                        (lambda (a b)
                          (< (eca--session-id a) (eca--session-id b))))))
    (eca-assert-session-running (car sessions))
    (let* ((entries (cl-loop
                     for session in sessions
                     append (cl-loop
                             for buffer in (eca-chat--session-chats-oldest-first
                                            session)
                             collect (cons session buffer))))
           ;; Anchor at the current chat; when not in a chat, anchor just
           ;; before the current session's entries so they are searched
           ;; first.
           (pos (or (cl-position (current-buffer) entries :key #'cdr)
                    (when-let* ((session (eca-session))
                                (idx (cl-position session entries
                                                  :key #'car)))
                      (1- idx))))
           (target (seq-find (lambda (entry)
                               (eca-chat--needs-attention-p (cdr entry)))
                             (eca-chat--rotate-after entries pos))))
      (if target
          (eca-chat--switch-to-buffer (cdr target) (car target))
        (eca-info "No chat needs attention in any project")))))

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
(defun eca-chat-show-context ()
  "Show the context-window usage breakdown via the /context command."
  (interactive)
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (let ((session (eca-session)))
      (when (and session (not eca-chat--chat-loading))
        (eca-chat--send-prompt session "/context")))))

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

(defun eca-chat--relative-time (ms)
  "Return a relative-time string for epoch MS (milliseconds).
E.g. \"just now\", \"5m ago\", \"3h ago\", \"2d ago\"."
  (when ms
    (let* ((delta (- (float-time) (/ (float ms) 1000.0))))
      (cond
       ((< delta 60)    "just now")
       ((< delta 3600)  (format "%dm ago" (floor (/ delta 60))))
       ((< delta 86400) (format "%dh ago" (floor (/ delta 3600))))
       (t               (format "%dd ago" (floor (/ delta 86400))))))))

(defun eca-chat--kill-empty-welcome-buffer (session buffer keep-buffer)
  "Kill BUFFER for SESSION if it's a fresh welcome chat.
Skips when BUFFER equals KEEP-BUFFER or when the welcome banner is
no longer present (i.e. the user already exchanged messages there).
Marks the chat as `eca-chat--closed' so the `kill-buffer' hook does
not prompt for server-side deletion, and dissocs it from the
session's chat registry.  Used by `eca-chat-resume' to clean up
the empty buffer that was used to trigger the resume."
  (when (and (buffer-live-p buffer)
             (not (eq buffer keep-buffer))
             (with-current-buffer buffer eca-chat--welcome-shown))
    (with-current-buffer buffer
      (setq-local eca-chat--closed t)
      (when-let* ((cid eca-chat--id))
        (setf (eca--session-chats session)
              (eca-dissoc (eca--session-chats session) cid))))
    (kill-buffer buffer)
    (eca-chat--force-tab-line-update)))

;;;###autoload
(defun eca-chat-resume ()
  "Select and resume a previous ECA session."
  (interactive)
  (let* ((session (eca-session))
         (from-buf (current-buffer)))
    (eca-assert-session-running session)
    (let* ((res (eca-api-request-sync session :method "chat/list"))
           ;; Drop entries the server can't actually re-open: nil ids show up
           ;; for legacy DB rows that pre-date the per-chat `:id` field, and
           ;; picking them would silently no-op because `chat/open {:chatId
           ;; nil}` returns `{:found? false}`.
           (chats (cl-remove-if-not (lambda (c) (plist-get c :id))
                                    (append (plist-get res :chats) nil))))
      (if (null chats)
          (message "No previous sessions to resume.")
        (let* ((id-by-label  (make-hash-table :test 'equal))
               (ann-by-label (make-hash-table :test 'equal))
               (labels       nil))
          (seq-do
           (lambda (chat)
             (let* ((id    (plist-get chat :id))
                    (title (or (plist-get chat :title) "Untitled"))
                    (model (plist-get chat :model))
                    (count (plist-get chat :messageCount))
                    (upd   (plist-get chat :updatedAt))
                    (label (if (gethash title id-by-label)
                               (concat title " (" id ")")
                             title))
                    (ann   (concat
                            (when model
                              (propertize (concat "  " model) 'face 'shadow))
                            (when count
                              (propertize (format "  %d msgs" count) 'face 'shadow))
                            (when-let* ((rel (eca-chat--relative-time upd)))
                              (propertize (concat "  " rel)
                                          'face 'eca-chat-elapsed-time-face)))))
               (puthash label id id-by-label)
               (puthash label ann ann-by-label)
               (push label labels)))
           chats)
          (setq labels (nreverse labels))
          (when-let* ((chosen (completing-read
                               "Resume session: "
                               (lambda (string pred action)
                                 (if (eq action 'metadata)
                                     `(metadata
                                       (display-sort-function . ,#'identity)
                                       (annotation-function
                                        . ,(lambda (candidate)
                                             (gethash candidate ann-by-label))))
                                   (complete-with-action action labels string pred)))
                               nil t))
                      (chat-id (gethash chosen id-by-label)))
            (eca-api-request-async
             session
             :method "chat/open"
             :params (append (list :chatId chat-id)
                             (when eca-chat-history-page-size
                               (list :limit eca-chat-history-page-size)))
             :success-callback
             (lambda (open-res)
               (cond
                ((not (plist-get open-res :found?))
                 (user-error "Server could not open chat %s" chat-id))
                ((not (buffer-live-p (eca-get (eca--session-chats session) chat-id)))
                 (user-error "Resume: no buffer was registered for chat %s" chat-id))
                (t
                 (let ((chat-buf (eca-get (eca--session-chats session) chat-id)))
                   (setf (eca--session-last-chat-buffer session) chat-buf)
                   (eca-chat--with-current-buffer chat-buf
                     (eca-chat--apply-history-meta (plist-get open-res :meta))
                     (eca-chat--refresh-load-older-control)
                     (eca-chat--protect-non-prompt))
                   (eca-chat-open session)
                   (eca-chat--kill-empty-welcome-buffer session from-buf chat-buf)))))
             :error-callback
             (lambda (err)
               (user-error "Failed to resume: %s" err)))))))))

;;;###autoload
(defun eca-chat-rename ()
  "Rename last visited chat to a custom NEW-NAME."
  (interactive)
  (let ((new-name (read-string "Inform the new chat title: ")))
    (eca-assert-session-running (eca-session))
    (with-current-buffer (eca-chat--get-last-buffer (eca-session))
      ;; Update local title immediately for responsiveness
      (setq-local eca-chat--title new-name)
      ;; Clear any custom title since we now have an official title
      (setq-local eca-chat--custom-title nil)
      ;; Request server to persist and broadcast to other clients
      (eca-api-request-sync (eca-session)
                            :method "chat/update"
                            :params (list :chatId eca-chat--id :title new-name)))))

;;;###autoload
(defun eca-chat-delete ()
  "Delete the active chat of the current session from the server.
Operates on the session's last visited chat buffer, so it can be
called from any buffer in the project, not only from the chat
buffer itself.  Unlike killing the chat buffer, this never
prompts; the chat is always removed server-side.  When the session
has other chats, any window showing the deleted chat switches to
another chat first."
  (interactive)
  (let ((session (eca-session)))
    (eca-assert-session-running session)
    (let* ((buffer (eca-chat--get-last-buffer session))
           (chat-id (and (buffer-live-p buffer)
                         (buffer-local-value 'eca-chat--id buffer))))
      (unless (and (buffer-live-p buffer) chat-id)
        (user-error "No active chat to delete"))
      (when-let ((other (-first (lambda (buf)
                                  (and (buffer-live-p buf) (not (eq buf buffer))))
                                (eca-vals (eca--session-chats session)))))
        (setf (eca--session-last-chat-buffer session) other)
        (dolist (win (get-buffer-window-list buffer nil t))
          (let ((dedicated (window-dedicated-p win)))
            (set-window-dedicated-p win nil)
            (set-window-buffer win other)
            (set-window-dedicated-p win dedicated))))
      ;; Mark closed so the kill-buffer hook neither prompts nor sends a
      ;; second chat/delete when BUFFER is killed below.
      (with-current-buffer buffer
        (setq-local eca-chat--closed t))
      (eca-api-request-sync session
                            :method "chat/delete"
                            :params (list :chatId chat-id))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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

(defun eca-chat--get-flags (&optional buffer)
  "Extract all flags from the chat BUFFER.
If BUFFER is nil, use the last chat buffer from current session.
Returns a list of plists ordered newest to oldest."
  (when-let* ((session (eca-session))
              (chat-buffer (or buffer (eca-chat--get-last-buffer session)))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (let ((flags '()))
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when-let* ((flag-text (overlay-get ov 'eca-chat--flag-text))
                      (start (overlay-start ov)))
            (push (list :text (concat "🚩️️ " flag-text)
                        :start start
                        :timestamp (or (overlay-get ov 'eca-chat--timestamp)
                                       (float-time)))
                  flags)))
        flags))))

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
  "Navigate to a user message or flag via completion."
  (interactive)
  (let* ((messages (or (eca-chat--get-user-messages) '()))
         (flags (or (eca-chat--get-flags) '()))
         (all-entries (append messages flags)))
    (if (null all-entries)
        (message "No user messages or flags found")
      (let ((table (make-hash-table :test 'equal)))
        (dolist (entry (sort all-entries
                             (lambda (a b) (< (plist-get a :start) (plist-get b :start)))))
          (puthash (eca-chat--format-message-for-completion entry) entry table))
        (when-let* ((choice (completing-read
                             "Timeline: "
                             (lambda (string pred action)
                               (if (eq action 'metadata)
                                   `(metadata (display-sort-function . identity))
                                 (complete-with-action action (hash-table-keys table) string pred)))
                             nil t))
                    (selected (gethash choice table))
                    (pos (plist-get selected :start))
                    (chat-buffer (eca-chat--get-last-buffer (eca-session))))
          (eca-chat--display-buffer chat-buffer)
          (with-current-buffer chat-buffer
            (goto-char pos)
            (recenter)))))))

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

(defun eca-chat--doctor-find-buffer ()
  "Find an ECA chat buffer to inspect for diagnostics.
Tries, in order: a chat buffer for the current buffer's session, then
the most recent chat of any running session.  Returns the buffer or
nil if no chat buffer can be located."
  (let ((session (ignore-errors (eca-session))))
    (or (and session
             (let ((b (ignore-errors (eca-chat--get-last-buffer session))))
               (and b (buffer-live-p b) b)))
        (-some (lambda (s)
                 (let ((b (ignore-errors (eca-chat--get-last-buffer s))))
                   (and b (buffer-live-p b) b)))
               (eca-vals eca--sessions)))))

(defun eca-chat--doctor-self-check (src-buf)
  "Probe loaded ECA chat code and SRC-BUF for fix-presence indicators.
Return a plist describing the state of mode-map bindings, the
cond ordering inside `eca-chat--key-pressed-return', the first
expandable label's keymap, and whether the loaded `.elc' is stale
relative to the on-disk `.el'."
  (let* ((mode-map-ret (ignore-errors
                         (lookup-key eca-chat-mode-map (kbd "RET"))))
         (mode-map-ret-ok (eq mode-map-ret 'eca-chat--key-pressed-return))
         (kpr-body (ignore-errors
                     (prin1-to-string
                      (symbol-function 'eca-chat--key-pressed-return))))
         (kpr-exp-pos (and kpr-body
                           (string-match-p "expandable-content-at-point"
                                           kpr-body)))
         (kpr-md-pos (and kpr-body
                          (string-match-p "markdown-link-face" kpr-body)))
         (cond-ok (cond ((and kpr-exp-pos kpr-md-pos)
                         (< kpr-exp-pos kpr-md-pos))
                        (t 'unknown)))
         (label-ov (with-current-buffer src-buf
                     (-first (lambda (o)
                               (overlay-get o 'eca-chat--expandable-content-id))
                             (overlays-in (point-min) (point-max)))))
         (label-ret
          (and label-ov
               (with-current-buffer src-buf
                 (save-excursion
                   (goto-char (overlay-start label-ov))
                   (let ((line-end (line-end-position))
                         (km nil)
                         (pos (point)))
                     (while (and (< pos line-end) (not km))
                       (setq km (get-text-property pos 'keymap))
                       (unless km
                         (setq pos (or (next-single-property-change
                                        pos 'keymap nil line-end)
                                       line-end))))
                     (when km
                       (or (lookup-key km (kbd "RET"))
                           (lookup-key km (kbd "<return>")))))))))
         (label-ret-ok (and label-ret (functionp label-ret)))
         (loaded-file (ignore-errors
                        (symbol-file 'eca-chat--insert-expandable-block
                                     'defun)))
         (source-file (ignore-errors
                        (find-library-name "eca-chat-expandable")))
         (stale-elc-p (and loaded-file source-file
                           (string-suffix-p ".elc" loaded-file)
                           (file-newer-than-file-p source-file
                                                   loaded-file))))
    (list :mode-map-ret mode-map-ret
          :mode-map-ret-ok mode-map-ret-ok
          :cond-ok cond-ok
          :has-label (not (null label-ov))
          :label-ret label-ret
          :label-ret-ok label-ret-ok
          :loaded-file loaded-file
          :source-file source-file
          :stale-elc-p stale-elc-p)))

(defun eca-chat--doctor-format (src-buf)
  "Return a markdown diagnostic string describing chat buffer SRC-BUF.
Captures `major-mode' derivation, RET binding, shadowing minor modes,
ECA chat state, prompt overlays, and rule-based hints."
  (let* ((mm (buffer-local-value 'major-mode src-buf))
         (in-chat-p (with-current-buffer src-buf
                      (derived-mode-p 'eca-chat-mode)))
         (parent-mode (and (boundp 'eca-chat-parent-mode)
                           eca-chat-parent-mode))
         (def-dir (buffer-local-value 'default-directory src-buf))
         (ret-cmd (with-current-buffer src-buf
                    (key-binding (kbd "RET"))))
         (return-cmd (with-current-buffer src-buf
                       (key-binding (kbd "<return>"))))
         (cm-cmd (with-current-buffer src-buf
                   (key-binding (kbd "C-m"))))
         (input-method (buffer-local-value 'current-input-method src-buf))
         (e-indent (and (boundp 'electric-indent-mode)
                        (buffer-local-value 'electric-indent-mode src-buf)))
         (e-pair (and (boundp 'electric-pair-mode)
                      (buffer-local-value 'electric-pair-mode src-buf)))
         (e-quote (and (boundp 'electric-quote-mode)
                       (buffer-local-value 'electric-quote-mode src-buf)))
         (evil-on (and (boundp 'evil-mode) (symbol-value 'evil-mode)))
         (evil-state-val (and (boundp 'evil-state)
                              (buffer-local-value 'evil-state src-buf)))
         (viper-on (and (boundp 'viper-mode) (symbol-value 'viper-mode)))
         (god-on (and (boundp 'god-local-mode)
                      (buffer-local-value 'god-local-mode src-buf)))
         (sp-on (and (boundp 'smartparens-mode)
                     (buffer-local-value 'smartparens-mode src-buf)))
         (paredit-on (and (boundp 'paredit-mode)
                          (buffer-local-value 'paredit-mode src-buf)))
         (corfu-on (and (boundp 'corfu-mode)
                        (buffer-local-value 'corfu-mode src-buf)))
         (company-on (and (boundp 'company-mode)
                          (buffer-local-value 'company-mode src-buf)))
         (chat-id (buffer-local-value 'eca-chat--id src-buf))
         (closed (buffer-local-value 'eca-chat--closed src-buf))
         (loading (buffer-local-value 'eca-chat--chat-loading src-buf))
         (pending (buffer-local-value 'eca-chat--pending-question src-buf))
         (session (with-current-buffer src-buf
                    (ignore-errors (eca-session))))
         (prompt-area (with-current-buffer src-buf
                        (ignore-errors (eca-chat--prompt-area-ov))))
         (prompt-field (with-current-buffer src-buf
                         (ignore-errors (eca-chat--prompt-field-ov))))
         (prompt-content (with-current-buffer src-buf
                           (ignore-errors (eca-chat--prompt-content))))
         (pt (with-current-buffer src-buf (point)))
         (self (eca-chat--doctor-self-check src-buf))
         (hints nil))
    ;; --- compute hints ---
    (unless in-chat-p
      (push (format "Inspected buffer's `major-mode' is `%s' and is not \
derived from `eca-chat-mode'.  `eca-chat-mode' likely failed to activate." mm)
            hints))
    (when (and in-chat-p
               (not (eq ret-cmd 'eca-chat--key-pressed-return))
               (not (eq return-cmd 'eca-chat--key-pressed-return)))
      (push (format "RET is bound to `%s' (and <return> to `%s'), not \
`eca-chat--key-pressed-return'.  Another keymap is intercepting RET — see \
\"Likely shadowing\" below." ret-cmd return-cmd)
            hints))
    (when (and in-chat-p (null session))
      (push "(eca-session) returned nil from inside the chat buffer — \
the buffer is not registered to any ECA workspace." hints))
    (when (and in-chat-p (null prompt-area))
      (push "The `eca-chat-prompt-area' overlay is missing — chat buffer is \
in an inconsistent state.  Try `M-x eca-chat-reset'." hints))
    (when (and in-chat-p evil-on (eq evil-state-val 'insert))
      (push "evil-mode is in insert state.  `evil-insert-state-map' (an \
emulation map) beats major-mode maps.  If RET above is `evil-ret' / \
`newline', that is the cause." hints))
    (when (and in-chat-p pending)
      (push "An unanswered question is pending \
(`eca-chat--pending-question' non-nil).  RET without freeform input is \
intentionally a no-op until you answer the question." hints))
    (when (and in-chat-p closed)
      (push "This chat is marked closed (`eca-chat--closed' non-nil); \
sending will raise a user-error." hints))
    (when (and in-chat-p (not (plist-get self :mode-map-ret-ok)))
      (push (format "`eca-chat-mode-map' RET is `%s', expected \
`eca-chat--key-pressed-return'.  Some code has clobbered the mode-map \
after `eca-chat' was loaded; check your user config."
                    (plist-get self :mode-map-ret))
            hints))
    (when (and in-chat-p (eq (plist-get self :cond-ok) nil))
      (push "`eca-chat--key-pressed-return' references the markdown-link \
clause before the expandable clause.  Loaded code predates commit \
`dac33d8'; pull master, restart Emacs, and `M-x byte-recompile-directory' \
if the warning persists." hints))
    (when (and in-chat-p (plist-get self :has-label)
               (not (plist-get self :label-ret-ok)))
      (push "The first expandable block label has no RET binding in its \
`keymap' text-property.  Either the loaded `eca-chat-expandable.el' \
predates commit `dac33d8' or this block was rendered before the fix was \
reloaded; start a new chat or wait for a fresh tool call to be rendered."
            hints))
    (when (and in-chat-p (plist-get self :stale-elc-p))
      (push (format "`%s' is newer than the loaded `%s'.  Recompile \
(`M-x byte-recompile-directory') or delete the stale `.elc' and restart \
Emacs." (plist-get self :source-file) (plist-get self :loaded-file))
            hints))
    ;; --- render report into temp buffer and return as string ---
    (with-temp-buffer
      (insert "### Inspected chat buffer\n\n")
      (insert (format "- buffer:            %s\n" (buffer-name src-buf)))
      (insert (format "- major-mode:        %s\n" mm))
      (insert (format "- derived from `eca-chat-mode': %s\n"
                      (if in-chat-p "yes" "NO")))
      (insert (format "- `eca-chat-parent-mode': %s\n" parent-mode))
      (insert (format "- default-directory: %s\n\n" def-dir))
      (insert "### Key bindings in chat buffer\n\n")
      (insert (format "- RET        → %s\n" (or ret-cmd "<unbound>")))
      (insert (format "- <return>   → %s\n" (or return-cmd "<unbound>")))
      (insert (format "- C-m        → %s\n\n" (or cm-cmd "<unbound>")))
      (insert "### Likely shadowing / input translation\n\n")
      (insert (format "- current-input-method:  %s\n" (or input-method "nil")))
      (insert (format "- electric-indent-mode:  %s\n" e-indent))
      (insert (format "- electric-pair-mode:    %s\n" e-pair))
      (insert (format "- electric-quote-mode:   %s\n" e-quote))
      (insert (format "- evil-mode:             %s (state=%s)\n"
                      evil-on evil-state-val))
      (insert (format "- viper-mode:            %s\n" viper-on))
      (insert (format "- god-local-mode:        %s\n" god-on))
      (insert (format "- smartparens-mode:      %s\n" sp-on))
      (insert (format "- paredit-mode:          %s\n" paredit-on))
      (insert (format "- corfu-mode:            %s\n" corfu-on))
      (insert (format "- company-mode:          %s\n\n" company-on))
      (insert "### ECA chat state\n\n")
      (insert (format "- eca-chat--id:               %s\n" chat-id))
      (insert (format "- eca-chat--closed:           %s\n" closed))
      (insert (format "- eca-chat--chat-loading:     %s\n" loading))
      (insert (format "- eca-chat--pending-question: %s\n"
                      (if pending "set" "nil")))
      (insert "- (eca-session): ")
      (if session
          (insert (format "found (id=%s, roots=%S)\n\n"
                          (eca--session-id session)
                          (eca--session-workspace-folders session)))
        (insert "NIL — buffer is not registered to a workspace\n\n"))
      (insert "### Self-check\n\n")
      (insert (format "- `eca-chat-mode-map' RET → %s  %s\n"
                      (or (plist-get self :mode-map-ret) "<unbound>")
                      (if (plist-get self :mode-map-ret-ok) "✓" "✗")))
      (insert (format
               "- `eca-chat--key-pressed-return': expandable before \
markdown-link  %s\n"
               (let ((status (plist-get self :cond-ok)))
                 (cond ((eq status t) "✓")
                       ((null status) "✗")
                       (t "? (could not introspect)")))))
      (if (plist-get self :has-label)
          (insert (format "- nearest expandable label RET → %s  %s\n"
                          (let ((b (plist-get self :label-ret)))
                            (cond ((null b) "<unbound>")
                                  ((functionp b) "<toggle lambda>")
                                  (t (format "%s" b))))
                          (if (plist-get self :label-ret-ok) "✓" "✗")))
        (insert "- nearest expandable label RET: n.a. \
(no blocks rendered yet)\n"))
      (insert (format "- loaded from: %s\n"
                      (or (plist-get self :loaded-file) "<unknown>")))
      (when (plist-get self :stale-elc-p)
        (insert (format "  ⚠ source `%s' is newer than the loaded `.elc'\n"
                        (plist-get self :source-file))))
      (insert "\n")
      (insert "### Prompt overlays\n\n")
      (insert (format "- prompt-area overlay:  %s\n"
                      (if prompt-area
                          (format "%d..%d"
                                  (overlay-start prompt-area)
                                  (overlay-end prompt-area))
                        "MISSING")))
      (insert (format "- prompt-field overlay: %s\n"
                      (if prompt-field
                          (format "%d..%d"
                                  (overlay-start prompt-field)
                                  (overlay-end prompt-field))
                        "MISSING")))
      (insert (format "- point:                %d\n" pt))
      (insert (format "- (eca-chat--prompt-content): %S\n\n" prompt-content))
      (insert "### Hints\n\n")
      (if hints
          (dolist (h (nreverse hints))
            (insert "- " h "\n"))
        (insert "- No obvious issue detected by the static checks.\n"))
      (buffer-string))))

(defun eca-chat--doctor-section ()
  "Return a markdown-formatted diagnostic string for the active chat.
Auto-finds the chat buffer via `eca-chat--doctor-find-buffer'.  When
no chat buffer is available, returns a single-line notice instead.
Used by `eca-doctor'."
  (if-let ((src-buf (eca-chat--doctor-find-buffer)))
      (eca-chat--doctor-format src-buf)
    "No chat buffer found in any running session.\n"))

(provide 'eca-chat)
;;; eca-chat.el ends here
