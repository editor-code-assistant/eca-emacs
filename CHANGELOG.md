# Changelog

## Unreleased

- Add `eca-chat-remove-workspace-root` command and `[-]` mode-line button to remove a workspace folder from a running session. Both 'add' and 'remove' is in the transient menu (`W a` / `W r`).
- Add `eca-chat-mode-line-format` for customizable chat mode line layout. #184
- Fix top-level `(require 'tab-line)` causing side effects when `eca-chat-tab-line` is nil. #195
- Add support for `chat/opened` server notification, enabling the `/fork` command to open forked chats as new tabs.
- Fix `eca--uri-to-path` returning wrong path on Windows (leading slash before drive letter). #200
- Add `eca-settings-tab-line` option to disable `tab-line-mode` in settings buffers. #201
- Replace `helpful-heading` face dependency with `eca-settings-heading` defface. #201
- Fix settings tab re-registration reordering tabs. #201
- Add support for `chat/askQuestion` server request, enabling tools to ask users questions with selectable options in the chat buffer. #338

## 0.6.0

- Add `eca-chat-save-to-file` command. #95 
- Fix chat not being closed. #89
- Fix: check existing eca sessions when opening chat. #88
- Add rewrite feature.
- Fix issue with `view_ediff` displaying empty buffers in ediff. #86
- Fix user message contexts and file paths not being sent. #112

# Started tracking since 03/11/25
