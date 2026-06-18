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
- `eca-settings`: Open the centralized settings panel (MCP servers, and more in the future)
- `eca-open-global-config`: Open ECA global config file

Chat

- `eca-chat-toggle-window`: Open/close chat
- `eca-switch-to-chat`: Switch to an active chat across sessions
- `eca-switch-to-project-chat`: Switch to a chat for the current session
- `eca-chat-select`: Select a chat for existing session
- `eca-chat-new`: Start a new chat for current session/workspace folders.
- `eca-chat-rename`: Rename current chat
- `eca-chat-clear`: Clear chat messages both on server and local.
- `eca-chat-reset`: Deletes current chat and start a new one
- `eca-chat-delete`: Delete the active chat from the server without prompting (works from any buffer in the project)
- `eca-chat-select-model`: Change chat model
- `eca-chat-select-agent`: Change chat agent
- `eca-chat-cycle-agent`: Change chat agent to next available
- `eca-chat-add-context-to-system-prompt`: Add file/dirs to system prompt checking multiple modes with range support
- `eca-chat-add-context-to-user-prompt`: Add file/dirs to user prompt checking multiple modes with range support, jumping to the chat window (with prefix arg, keep point where it was)
- `eca-chat-add-filepath-to-user-prompt`: Add filepath mention only to user prompt checking multiple modes with range support, jumping to the chat window (with prefix arg, keep point where it was)
- `eca-chat-drop-context-from-system-prompt`: Drop a context from system prompt
- `eca-chat-add-flag`: Add a named flag after the nearest message block at or before point
- `eca-chat-send-prompt`: Send a prompt in chat interactively
- `eca-chat-send-prompt-at-chat` Open chat and send any prompt written there
- `eca-chat-clear-prompt`: Clear written prompt in chat
- `eca-chat-repeat-prompt`: Repeat a previously sent prompt
- `eca-chat-copy-at-point`: Copy the code block or assistant response at point
- `eca-chat-stop-prompt`: Stop a running prompt in chat
- `eca-chat-tool-call-accept-all`: Accept all pending tool calls in chat
- `eca-chat-tool-call-accept-all-and-remember`: Accept all pending tool calls in chat and remember for session
- `eca-chat-tool-call-accept-next`: Accept next pending tool call in chat
- `eca-chat-tool-call-reject-next`: Reject next pending tool call in chat
- `eca-chat-go-to-prev-user-message`: Go to the previous user message from point
- `eca-chat-go-to-next-user-message`: Go to the next user message from point
- `eca-chat-go-to-prev-expandable-block`: Go to the previous expandable block from point
- `eca-chat-go-to-next-expandable-block`: Go to the next expandable block from point
- `eca-chat-go-to-next-attention`: Go to the next chat waiting on you (pending tool call approval or question), cycling across sessions
- `eca-chat-go-to-next-attention-in-project`: Same as `eca-chat-go-to-next-attention` but cycling only the current session chats
- `eca-chat-toggle-expandable-block`: Toggle current expandable block at point
- `eca-chat-expand-all-blocks`: Expand all expandable blocks in current chat
- `eca-chat-collapse-all-blocks`: Collapse all expandable blocks in current chat
- `eca-chat-timeline`: Show user prompt history as a timeline
- `eca-chat-load-older-history`: Load and prepend the previous (older) page of the chat's history, also reachable via the "Load older messages" link at the top of the chat. Bound to `C-c C-S-o`.
- `eca-chat-talk`: Use whisper.el to send a prompt via voice.
- `eca-table-open`: Open the markdown table at point in a dedicated `*eca-table*` buffer where horizontal scrolling keeps the header aligned with the body, so wide tables can be read in full. Press `o` with point on any table to open it (the binding is scoped to the table, so it overrides `o` only there and works under evil/Doom too), or click `[o] open` on a wide table's action bar.
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
- `eca-worktree-mode`: How ECA handles git worktrees of the same repository (`merged` or `isolated`).

Chat

- `eca-chat-parent-mode`: Set major-mode of chat parent, can be `markdown-mode`, `markdown-view-mode` or `gfm-view-mode` (default)
- `eca-chat-mode-hook`: Hooks to run after entering `eca-chat-mode`.
- `eca-chat-finished-hook`: Hooks to run after finishing a chat prompt.
- `eca-chat-use-side-window`: Whether the chat buffer is displayed in a dedicated side window or a normal window. Either way it is placed on `eca-chat-window-side`.
- `eca-chat-window-side`: On which side (`left`, `right`, `top`, `bottom`) the chat window appears (respected for both side and normal windows).
- `eca-chat-window-width`: Width of the chat window when on the left or right.
- `eca-chat-window-height`: Height of the chat window when on the top or bottom.
- `eca-chat-focus-on-open`: Whether to focus the chat window when it opens.
- `eca-chat-auto-add-repomap`: Whether to automatically include repoMap context when opening ECA.
- `eca-chat-auto-add-cursor`: Whether to automatically track the cursor position and add it as context.
- `eca-chat-cursor-context-debounce`: Seconds to debounce updates when tracking cursor context.
- `eca-chat-prompt-separator`: Separator string between the chat content and the prompt area.
- `eca-chat-prompt-prefix`: Prompt prefix string shown before user input.
- `eca-chat-prompt-prefix-loading`: Prompt prefix string while a request is in progress.
- `eca-chat-read-only-history`: Whether the chat history/output, the `---` separator and the task area are read-only so only the progress, `@`-context and prompt input lines stay editable (default `t`). Set to `nil` to keep the whole buffer writable.
- `eca-chat-history-page-size`: Number of newest messages to load when opening a persisted chat (default `50`). When non-nil, `eca-chat-resume` opens chats with a bounded window and shows a "Load older messages" control to page through earlier history on demand; set to `nil` to replay the entire history on open.
- `eca-chat-context-prefix`: Prefix used for context references in the chat buffer (default `@`).
- `eca-chat-filepath-prefix`: Prefix used for file path references in the chat buffer (default `#`).
- `eca-chat-expandable-block-open-symbol`: Symbol used for expandable blocks in open state.
- `eca-chat-expandable-block-close-symbol`: Symbol used for expandable blocks in closed state.
- `eca-chat-mcp-tool-call-loading-symbol`: Symbol used for MCP tool calls while loading.
- `eca-chat-mcp-tool-call-error-symbol`: Symbol used for MCP tool calls when they fail.
- `eca-chat-mcp-tool-call-success-symbol`: Symbol used for MCP tool calls when they succeed.
- `eca-chat-expand-pending-approval-tools`: Whether to auto-expand tool calls that are pending approval.
- `eca-chat-shrink-called-tools`: Whether to auto-shrink tool calls after they have been executed.
- `eca-chat-tab-line`: Whether to show a tab line with chat tabs at the top of each chat window (default `t`). Each tab shows the chat status (pending approval, loading) and title.
- `eca-chat-custom-model`: Override the model used for chat (nil = server default).
- `eca-chat-custom-agent`: Override the chat agent (nil = server default).
- `eca-chat-usage-string-format`: Controls what usage information (tokens/costs/limits) is shown in the mode-line.
- `eca-chat-mode-line-format`: Controls the layout of the chat mode line. Can be a list of module keywords and literal strings, or a function for full control.
- `eca-chat-diff-tool`: How to show file diffs from chat (`smerge` or `ediff`).
- `eca-chat-tool-call-prepare-throttle`: Throttle strategy for `toolCallPrepare` events (`all` or `smart`).
- `eca-chat-tool-call-prepare-update-interval`: When using `smart` throttle, process every Nth `toolCallPrepare` update.
- `eca-chat-tool-call-approval-content-size`: Face height used for tool call approval UI text.
- `eca-chat-save-chat-initial-path`: Default initial path to save chats.

Completion

- `eca-completion-idle-delay`: Idle delay before triggering inline completion (0 = immediate, nil = disabled).
- `eca-completion-syntax-highlight`: Whether to syntax-highlight the inline ghost-text suggestion using the buffer's `major-mode` (default `t`).
- `eca-completion-overlay-dim-ratio`: Float in `[0.0, 1.0]` controlling how far each fontified span's foreground is blended toward the default background; lower values yield a more dimmed "ghost" look (default `0.5`, `nil` disables dimming). Effective only when `eca-completion-syntax-highlight` is non-nil.

Rewrite

- `eca-rewrite-prompt-prefix`: Text automatically prefixed to rewrite prompts.
- `eca-rewrite-finish-prefix`: Prefix text shown in the buffer when a rewrite finishes.
- `eca-rewrite-diff-tool`: Diff tool for rewrite overlays (`simple-diff` or `ediff`).
- `eca-rewrite-finished-action`: Action to take when a rewrite finishes (`show-overlay-actions`, `accept`, `diff`, `merge`).
- `eca-rewrite-on-finished-hook`: Hook run after a rewrite finishes, receiving the overlay as argument.

Settings

- `eca-settings-tab-line`: Whether to show a tab line in settings buffers (default `t`).
- `eca-settings-display-params`: Display parameters for the settings panel side window.

Doom Emacs

- `eca-doom-workspace-tabs`: Whether to decorate the Doom workspace tabline with the ECA session status of each workspace (default `t`).

MCP

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
| Chat: copy at point                           | <kbd>C-c</kbd> <kbd>C-w</kbd>      |
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
| Chat: go to settings                          | <kbd>C-c</kbd> <kbd>C-,</kbd>      |
| Settings: go to chat                          | <kbd>C-c</kbd> <kbd>C-,</kbd>      |
| Settings: refresh current tab                 | <kbd>g</kbd>                        |
| Settings: quit                                | <kbd>q</kbd>                        |
| Settings: open transient menu                 | <kbd>C-c</kbd> <kbd>.</kbd>        |

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

### Doom Emacs workspace tabs

On Doom Emacs with the `:ui workspaces` module, the workspace tabline is
colored with the ECA session status of each workspace: orange when a chat
waits on you (pending approval or question), dim yellow while a chat is
running. Customize the colors via the `eca-doom-workspace-tab-attention-face`
and `eca-doom-workspace-tab-running-face` faces, or disable with:

```elisp
(setq eca-doom-workspace-tabs nil)
```

## Sandboxing

You can run the eca server under any sandbox tool that wraps a command
(firejail, bubblewrap, jai, docker, etc.) without writing a separate
launcher script. Configure `eca-process-wrapper-function` to prepend the
sandbox invocation in elisp. The function is called with the resolved
command (already including `eca-extra-args`) and the list of absolute
workspace folder paths that ECA will operate on, so it can dynamically
whitelist the directories the server needs.

When the sandbox tool hides or remaps the host PID, also set
`eca-send-process-id` to `nil`; otherwise the server's parent-process
watchdog would see an invalid PID and shut down right after startup.

<details>
<summary><strong>Example: jai</strong></summary>

[jai](https://jai.scs.stanford.edu/) is a lightweight Linux sandbox that
hides processes, environment variables, and any directory not explicitly
whitelisted with `-d`.

```elisp
(defun my/eca-jail-wrapper (command roots)
  "Wrap the eca server COMMAND with `jai', exposing ROOTS."
  (append (list "jai" "-j" "eca"
                "-D"                       ; do not expose $CWD
                "--unsetenv=*"             ; hide all environment vars
                "--setenv" "TERM")
          ;; Workspace roots picked at session startup.
          (cl-loop for d in roots
                   append (list "-d" (expand-file-name d)))
          ;; Always-needed directories for ECA itself.
          (list "-d" (expand-file-name "~/.config/eca/")
                "-d" (expand-file-name "~/.cache/eca/")
                "-d" (expand-file-name "~/.emacs.d/"))
          command))

(setq eca-process-wrapper-function #'my/eca-jail-wrapper)
(setq eca-send-process-id nil)
```

</details>

<details>
<summary><strong>Example: firejail</strong></summary>

The same shape works for any wrapper. Replace the `jai` prefix and flags:

```elisp
(defun my/eca-firejail-wrapper (command roots)
  (append (list "firejail" "--quiet"
                (format "--whitelist=%s" (expand-file-name "~/.config/eca"))
                (format "--whitelist=%s" (expand-file-name "~/.cache/eca")))
          (cl-loop for d in roots
                   append (list (format "--whitelist=%s"
                                        (expand-file-name d))))
          command))

(setq eca-process-wrapper-function #'my/eca-firejail-wrapper)
(setq eca-send-process-id nil)
```

</details>

### Limitations

- Sandbox tools that seal their directory whitelist at startup (jai,
  firejail's `--whitelist`, etc.) won't honor a workspace root added
  later in the same session. eca-emacs still sends
  `workspace/didChangeWorkspaceFolders` to the server when a root is
  added via `eca-chat-add-workspace-root` or the `[+]` mode-line button,
  but the sandbox itself blocks the new path. Workaround: launch ECA
  with `C-u M-x eca` and pre-select every root you intend to use.
- The wrapper function runs once per server start. Restart the eca
  session (`M-x eca-process-stop` then `M-x eca`) after editing it.

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
