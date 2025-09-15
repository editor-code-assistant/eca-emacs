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

- Custom `eca` server binary, it's automatically downloaded if `eca-custom-command` is `nil`
  - Place it on your `$PATH` or customize `eca-custom-command`
- [whisper.el](https://github.com/natrys/whisper.el/blob/master/whisper.el) for Speech-to-Text support (optional)

## Installation

### Melpa

```
M-x package-install eca
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

## Tips

### Functions

- `eca-chat-add-context-at-point`: Add the current function or selected lines to chat as context.
- `eca-chat-add-file-context`: Add the current file to chat as context.
- `eca-keep-all-suggested-changes`: Accept planned edit changes on file.
- `eca-discard-all-suggested-changes`: Reject planned edit changes on file.
- `eca-chat-send-prompt`: In case you wanna send prompts programatically in elisp.
- `eca-chat-toggle-window`: toggle chat window.

### Variables

- `eca-extra-args`: customize extra args to server to help debug like `("--verbose")` or `("--log-level" "debug")`.
- `eca-chat-usage-string-format`: to customize what shows on mode-line for usage like costs and tokens.
- `eca-chat-use-side-window`: customize whether chat window is a side-window or a normal buffer.
- `eca-chat-window-side`: customize the chat window side.
- `eca-chat-window-width`: customize the chat window width.
- `eca-chat-window-height`: customize the chat window height.
- `eca-chat-diff-tool`: customize viewing diffs, defaults to ediff.


### Keybindings

You can access __transient___ menu with commonly commands via `M-x eca-transient-menu` or by pressing `C-c .` in eca's windows.

#### Manual keybindings

| Feature                         | key                                |
|---------------------------------|------------------------------------|
| Chat: clear                     | <kbd>C-c</kbd> <kbd>C-l</kbd>      |
| Chat: reset                     | <kbd>C-c</kbd> <kbd>C-k</kbd>      |
| Chat: talk                      | <kbd>C-c</kbd> <kbd>C-t</kbd>      |
| Chat: Select behavior           | <kbd>C-c</kbd> <kbd>C-b</kbd>      |
| Chat: Select model              | <kbd>C-c</kbd> <kbd>C-m</kbd>      |
| Chat: Go to MCP details         | <kbd>C-c</kbd> <kbd>C-,</kbd>      |
| Chat: prev prompt history       | <kbd>C-&uarr;</kbd>                |
| Chat: next prompt history       | <kbd>C-&darr;</kbd>                |
| Chat: go to prev block          | <kbd>C-c</kbd> <kbd>&uarr;</kbd>   |
| Chat: go to next block          | <kbd>C-c</kbd> <kbd>&darr;</kbd>   |
| Chat: go to prev user msg       | <kbd>C-c</kbd> <kbd>C-&uarr;</kbd> |
| Chat: go to next user msg       | <kbd>C-c</kbd> <kbd>C-&darr;</kbd> |
| Chat: toggle expandable content | <kbd>C-c</kbd> <kbd>Tab</kbd>      |
| MCP: Go to chat                 | <kbd>C-c</kbd> <kbd>C-,</kbd>      |

## Advanced Usage

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

##### Solution: Use exec-path-from-shell

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

##### Flyspell Performance in ECA Chat

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


### ECA Server Connection Issues

#### Problem: ECA server fails to start or connect

1. **Check ECA installation**: Verify ECA is available on your PATH or set `eca-custom-command`:
   ```elisp
   (setq eca-custom-command "/path/to/your/eca/binary")
   ```

2. **Enable debug logging**: Add extra arguments for debugging:
   ```elisp
   (setq eca-extra-args '("--verbose" "--log-level" "debug"))
   ```

3. **Check environment variables**: Test if your API keys are available in Emacs:
   ```elisp
   M-x eval-expression RET (getenv "ANTHROPIC_API_KEY") RET
   ```

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

## Contributing

Contributions are very welcome, please open a issue for discussion or pull request.
