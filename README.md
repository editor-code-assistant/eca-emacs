# ECA Emacs

[![MELPA](https://melpa.org/packages/eca-badge.svg)](https://melpa.org/#/eca)
[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](./LICENSE)

![demo](./demo.gif)

ECA (Editor Code Assistant) Emacs is an AI-powered pair-programming client for Emacs.
Inspired by lsp-mode’s JSONRPC handling, it connects to an external `eca` server process to provide interactive chat, code suggestions, context management and more.
It's everything automatic and smooth as UX and re-usability across editors is the main goal of ECA.

For more details about ECA, check [ECA server](https://github.com/editor-code-assistant/eca).

## Requirements

- Emacs 28.1 or later

### Optional

- Custom `eca` server binary
  - Server is already automatically downloaded for UX reasons unless you set `eca-custom-command`
- [whisper.el](https://github.com/natrys/whisper.el/blob/master/whisper.el) for Speech-to-Text support (optional)

## Installation

### Melpa

```
M-x package-install eca
```

### Use Package

```
(use-package eca
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs" :rev :newest))
```

### Doom Emacs:

```elisp
(package! eca :recipe (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))
```

## Quickstart

1. Run `M-x eca` to start the eca process and initialize the workspace.
  - eca-emacs will check for `eca-custom-command`;
  - if not set, will check for a `eca` on `$PATH`;
  - if not found, will download `eca` automatically and cache it.
2. The dedicated chat window `<eca-chat>` pops up.
3. Type your prompt after the `> ` and press RET.
4. Attach more context auto completing after the `@`.

## Usage

### Commands

Server / process

- `eca`: Starts eca server/session + open chat
- `eca-stop`: Stop eca server/session
- `eca-restart`: Restart eca server/session
- `eca-workspaces`: show a view with all worskpaces and chats
- `eca-open-global-config`: Open ECA global config file

Chat

- `eca-chat-toggle-window`: Open/close chat
- `eca-chat-select`: Select a chat for existing session
- `eca-chat-new`: Start a new chat for current session/workspace folders.
- `eca-chat-rename`: Rename current chat
- `eca-chat-clear`: Clear chat content visually only
- `eca-chat-reset`: Deletes current chat and start a new one
- `eca-chat-select-model`: Change chat model
- `eca-chat-select-agent`: Change chat agent
- `eca-chat-cycle-agent`: Change chat agent to next available
- `eca-chat-add-context-to-system-prompt`: Add file/dirs to system prompt checking multiple modes with range support
- `eca-chat-add-context-to-user-prompt`: Add file/dirs to user prompt checking multiple modes with range support
- `eca-chat-add-filepath-to-user-prompt`: Add filepath mention only to user prompt checking multiple modes with range support
- `eca-chat-drop-context-from-system-prompt`: Drop a context from system prompt
- `eca-chat-send-prompt`: Send a prompt in chat interactively
- `eca-chat-send-prompt-at-chat` Open chat and send any prompt written there
- `eca-chat-clear-prompt`: Clear written prompt in chat
- `eca-chat-repeat-prompt`: Repeat a previously sent prompt
- `eca-chat-stop-prompt`: Stop a running prompt in chat
- `eca-chat-tool-call-accept-all`: Accept all pending tool calls in chat
- `eca-chat-tool-call-accept-all-and-remember`: Accept all pending tool calls in chat and remember for session
- `eca-chat-tool-call-accept-next`: Accept next pending tool call in chat
- `eca-chat-tool-call-reject-next`: Reject next pending tool call in chat
- `eca-chat-go-to-prev-user-message`: Go to the previous user message from point
- `eca-chat-go-to-next-user-message`: Go to the next user message from point
- `eca-chat-go-to-prev-expandable-block`: Go to the previous expandable block from point
- `eca-chat-go-to-next-expandable-block`: Go to the next expandable block from point
- `eca-chat-toggle-expandable-block`: Toggle current expandable block at point
- `eca-chat-expand-all-blocks`: Expand all expandable blocks in current chat
- `eca-chat-collapse-all-blocks`: Collapse all expandable blocks in current chat
- `eca-chat-timeline`: Show user prompt history as a timeline
- `eca-chat-talk`: Use whisper.el to send a prompt via voice.
- `eca-chat-save-to-file`: Save chat to a file.

### Variables

Server / process

- `eca-custom-command`: The `eca` server command; when nil ECA auto-downloads or uses `eca` from `$PATH`.
- `eca-server-download-method`: Method to download server (`curl` or `url-retrieve`, Emacs built-in way).
- `eca-server-download-url`: Custom URL to download the ECA server archive.
- `eca-server-install-path`: Path where the downloaded ECA server binary is installed.
- `eca-server-version-file-path`: Path to the file storing the downloaded ECA server version.
- `eca-unzip-script`: Script/command template used to unzip the downloaded ECA server archive.
- `eca-extra-args`: Extra args to pass to the ECA server, e.g. `("--verbose")` or `("--log-level" "debug")`.
- `eca-min-gc-cons-threshold`: Temporary GC threshold used while processing heavy server messages.

Core / session

- `eca-before-initialize-hook`: Functions called before an ECA session is initialized.
- `eca-after-initialize-hook`: Functions called after an ECA session is initialized.
- `eca-find-root-for-buffer-function`: Function used to determine the workspace root for the current buffer.

Chat

- `eca-chat-parent-mode`: Set major-mode of chat parent, can be `markdown-mode`, `markdown-view-mode` or `gfm-view-mode` (default)
- `eca-chat-mode-hook`: Hooks to run after entering `eca-chat-mode`.
- `eca-chat-finished-hook`: Hooks to run after finishing a chat prompt.
- `eca-chat-use-side-window`: Whether the chat buffer is displayed in a side window or a normal window.
- `eca-chat-window-side`: On which side (`left`, `right`, `top`, `bottom`) the chat side window appears.
- `eca-chat-window-width`: Width of the chat side window when on the left or right.
- `eca-chat-window-height`: Height of the chat side window when on the top or bottom.
- `eca-chat-focus-on-open`: Whether to focus the chat window when it opens.
- `eca-chat-auto-add-repomap`: Whether to automatically include repoMap context when opening ECA.
- `eca-chat-auto-add-cursor`: Whether to automatically track the cursor position and add it as context.
- `eca-chat-cursor-context-debounce`: Seconds to debounce updates when tracking cursor context.
- `eca-chat-prompt-separator`: Separator string between the chat content and the prompt area.
- `eca-chat-prompt-prefix`: Prompt prefix string shown before user input.
- `eca-chat-prompt-prefix-loading`: Prompt prefix string while a request is in progress.
- `eca-chat-context-prefix`: Prefix used for context references in the chat buffer (default `@`).
- `eca-chat-filepath-prefix`: Prefix used for file path references in the chat buffer (default `#`).
- `eca-chat-expandable-block-open-symbol`: Symbol used for expandable blocks in open state.
- `eca-chat-expandable-block-close-symbol`: Symbol used for expandable blocks in closed state.
- `eca-chat-mcp-tool-call-loading-symbol`: Symbol used for MCP tool calls while loading.
- `eca-chat-mcp-tool-call-error-symbol`: Symbol used for MCP tool calls when they fail.
- `eca-chat-mcp-tool-call-success-symbol`: Symbol used for MCP tool calls when they succeed.
- `eca-chat-expand-pending-approval-tools`: Whether to auto-expand tool calls that are pending approval.
- `eca-chat-shrink-called-tools`: Whether to auto-shrink tool calls after they have been executed.
- `eca-chat-custom-model`: Override the model used for chat (nil = server default).
- `eca-chat-custom-agent`: Override the chat agent (nil = server default).
- `eca-chat-usage-string-format`: Controls what usage information (tokens/costs/limits) is shown in the mode-line.
- `eca-chat-diff-tool`: How to show file diffs from chat (`smerge` or `ediff`).
- `eca-chat-tool-call-prepare-throttle`: Throttle strategy for `toolCallPrepare` events (`all` or `smart`).
- `eca-chat-tool-call-prepare-update-interval`: When using `smart` throttle, process every Nth `toolCallPrepare` update.
- `eca-chat-tool-call-approval-content-size`: Face height used for tool call approval UI text.
- `eca-chat-save-chat-initial-path`: Default initial path to save chats.

Completion

- `eca-completion-idle-delay`: Idle delay before triggering inline completion (0 = immediate, nil = disabled).

Rewrite

- `eca-rewrite-prompt-prefix`: Text automatically prefixed to rewrite prompts.
- `eca-rewrite-finish-prefix`: Prefix text shown in the buffer when a rewrite finishes.
- `eca-rewrite-diff-tool`: Diff tool for rewrite overlays (`simple-diff` or `ediff`).
- `eca-rewrite-finished-action`: Action to take when a rewrite finishes (`show-overlay-actions`, `accept`, `diff`, `merge`).
- `eca-rewrite-on-finished-hook`: Hook run after a rewrite finishes, receiving the overlay as argument.

MCP

- `eca-mcp-details-position-params`: Display parameters for the MCP details side window.

API / requests

- `eca-api-response-timeout`: Maximum time to wait (seconds) for synchronous API responses.
- `eca-api-request-while-no-input-may-block`: If non-nil, `eca-api-request-while-no-input` may block even when `non-essential` is set.

UI / misc

- `eca-buttons-allow-mouse`: Whether ECA buttons can be clicked with the mouse.

### Keybindings

You can access the transient menu with common commands via `M-x eca-transient-menu` or by pressing `C-c .` in ECA windows.

#### Manual keybindings

| Feature                                        | key                                |
|-----------------------------------------------|------------------------------------|
| Chat: clear                                   | <kbd>C-c</kbd> <kbd>C-l</kbd>      |
| Chat: reset                                   | <kbd>C-c</kbd> <kbd>C-k</kbd>      |
| Chat: talk                                    | <kbd>C-c</kbd> <kbd>C-t</kbd>      |
| Chat: select agent                            | <kbd>C-c</kbd> <kbd>C-S-b</kbd>    |
| Chat: cycle agent                             | <kbd>C-c</kbd> <kbd>C-b</kbd>      |
| Chat: select model                            | <kbd>C-c</kbd> <kbd>C-m</kbd>      |
| Chat: new chat                                | <kbd>C-c</kbd> <kbd>C-n</kbd>      |
| Chat: select chat                             | <kbd>C-c</kbd> <kbd>C-f</kbd>      |
| Chat: repeat last prompt                      | <kbd>C-c</kbd> <kbd>C-p</kbd>      |
| Chat: clear prompt                            | <kbd>C-c</kbd> <kbd>C-d</kbd>      |
| Chat: timeline                                | <kbd>C-c</kbd> <kbd>C-h</kbd>      |
| Chat: send prompt at chat buffer              | <kbd>C-c</kbd> <kbd>C-RET</kbd>    |
| Chat: accept all pending tool calls           | <kbd>C-c</kbd> <kbd>C-a</kbd>      |
| Chat: accept next pending tool call           | <kbd>C-c</kbd> <kbd>C-S-a</kbd>    |
| Chat: accept all tool calls and remember      | <kbd>C-c</kbd> <kbd>C-s</kbd>      |
| Chat: reject next pending tool call           | <kbd>C-c</kbd> <kbd>C-r</kbd>      |
| Chat: rename chat                             | <kbd>C-c</kbd> <kbd>C-S-r</kbd>    |
| Chat: prev prompt history                     | <kbd>C-↑</kbd>                     |
| Chat: next prompt history                     | <kbd>C-↓</kbd>                     |
| Chat: go to prev tool / diff / reason block   | <kbd>C-c</kbd> <kbd>↑</kbd>        |
| Chat: go to next tool / diff / reason block   | <kbd>C-c</kbd> <kbd>↓</kbd>        |
| Chat: go to prev user message                 | <kbd>C-c</kbd> <kbd>C-↑</kbd>      |
| Chat: go to next user message                 | <kbd>C-c</kbd> <kbd>C-↓</kbd>      |
| Chat: toggle expandable content at point      | <kbd>C-c</kbd> <kbd>Tab</kbd>      |
| Chat: open transient menu                     | <kbd>C-c</kbd> <kbd>.</kbd>        |
| Chat: go to MCP details                       | <kbd>C-c</kbd> <kbd>C-,</kbd>      |
| MCP: go to chat (from MCP details buffer)     | <kbd>C-c</kbd> <kbd>C-,</kbd>      |
| MCP: open transient menu (from MCP details)   | <kbd>C-c</kbd> <kbd>.</kbd>        |

## Features

Check detailed features [here](https://eca.dev/features/).

### Rewrite

Select a text and call `eca-rewrite`, after rewrite is finish, call any action on the overlay.

### Code completion

Enable `eca-completion-mode` and call `eca-complete`.

### Speech-to-Text support (Talk)

If you have [whisper.el](https://github.com/natrys/whisper.el/blob/master/whisper.el) installed you can use the `eca-chat-talk`
command (or use the `C-t` keybinding) to talk to the Editor Code
Assistant. This will record audio until you press `RET`. Then, the
recorded audio will be transcribed to text and placed into the chat
buffer.

We recommend to use the `small`, it is a good trade-off between
accuracy and transcription speed.

```elisp
(use-package whisper
  :custom
  (whisper-model "small"))
```

### Custom workspaces

Calling `M-x eca` with prefix `C-u` will ask for what workspaces to start the process.

## Troubleshooting

Check before the [server troubleshooting](https://eca.dev/troubleshooting/).

### Debugging Steps

1. **Verify environment**: Check what environment variables are available to Emacs:
   ```elisp
   M-x eval-expression RET process-environment RET
   ```

2. **Test ECA manually**: Try running ECA from terminal to verify it works:
   ```bash
   eca --help
   ```
4. **Reset ECA**: Clear the workspace and restart:
   ```
   M-x eca-chat-reset
   M-x eca  ; Start fresh
   ```

### ECA Server Connection Issues

#### Problem: ECA server fails to start or connect

1. **Check ECA installation**: Verify ECA is available on your PATH or set `eca-custom-command`:
   ```elisp
   (setq eca-custom-command '("/path/to/your/eca/binary" "server"))
   ```

2. **Enable debug logging**: Add extra arguments for debugging:
   ```elisp
   (setq eca-extra-args '("--verbose" "--log-level" "debug"))
   ```

3. **Check environment variables**: Test if your API keys are available in Emacs:
   ```elisp
   M-x eval-expression RET (getenv "ANTHROPIC_API_KEY") RET
   ```

### Env vars not available

#### Solution: Use exec-path-from-shell

Install and configure `exec-path-from-shell` to import your shell environment into Emacs:

```elisp
(use-package exec-path-from-shell
  :init
  ;; Specify the environment variables ECA needs
  (setq exec-path-from-shell-variables
        '("ANTHROPIC_API_KEY"
          "OPENAI_API_KEY"
          "OLLAMA_API_BASE"
          "OPENAI_API_URL"
          "ANTHROPIC_API_URL"
          "ECA_CONFIG"
          "XDG_CONFIG_HOME"
          "PATH"
          "MANPATH"))
  ;; For macOS and Linux GUI environments
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
```

### Performance

#### Flyspell Performance in ECA Chat

see - this [comment](https://github.com/editor-code-assistant/eca-emacs/pull/42#issuecomment-3292134511)

If Flyspell is causing slowdowns during LLM streaming, you can enable spell-checking only while typing and disable it on submit by adding this to your personal Emacs config:


``` emacs-lisp

(defun my/eca-chat-flyspell-setup ()
  "Enable Flyspell during typing and disable on submit in `eca-chat-mode`."
  (when (derived-mode-p 'eca-chat-mode)
    ;; Disable Flyspell when submitting prompts
    (add-hook 'pre-command-hook
              (lambda ()
                (when (and (memq this-command '(eca-chat--key-pressed-return
                                                eca-chat-send-prompt-at-chat))
                           flyspell-mode)
                  (flyspell-mode -1)))
              nil t)
    ;; Re-enable Flyspell when typing
    (add-hook 'pre-command-hook
              (lambda ()
                (when (and (eq this-command 'self-insert-command)
                           (not flyspell-mode))
                  (flyspell-mode 1)))
              nil t)))

(add-hook 'eca-chat-mode-hook #'my/eca-chat-flyspell-setup)
```

How it works:

Submit (Enter/Return): Disables Flyspell just before sending your prompt or programmatic send, preventing spell-checking overhead during streaming.

Typing: Re-enables Flyspell on any character insertion (self-insert-command), giving you real-time spell checking while composing.

## Contributing 💙

Contributions are very welcome, please open a issue for discussion or pull request.
