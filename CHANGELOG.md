# Changelog

## Unreleased

- Syntax-highlight the inline `eca-complete` ghost-text overlay using the buffer's `major-mode`, with optional foreground dimming for a "ghost" look similar to Cursor / Copilot. Configurable via `eca-completion-syntax-highlight` (default `t`) and `eca-completion-overlay-dim-ratio` (default `0.5`, `nil` disables dimming). `eca-completion-overlay-face` now composes with APPEND priority on top of font-lock faces instead of overriding them. #227
- Sync the mode-line trust indicator on chat resume by honoring the new `selectTrust` field on `config/updated`: the shield/flame icon now matches the server's persisted trust state for the resumed chat instead of always defaulting to off (eca #426).
- Defer fontification during chat streaming: replace the per-chunk `font-lock-ensure` with a debounced idle-timer scheduler plus a single guaranteed `font-lock-ensure` at end-of-stream, configurable via `eca-chat-fontify-debounce-interval` (default 0.15s, `nil` disables intermediate fontify). Cuts font-lock CPU by ~99% on fast-streaming models and ~84% on slow-streaming ones.
- Improve mode-line trust indicator: 🔥 (fire) when ON, 🛡 (shield) when OFF. Customizable via `eca-chat-trust-on-symbol` / `eca-chat-trust-off-symbol`.
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
