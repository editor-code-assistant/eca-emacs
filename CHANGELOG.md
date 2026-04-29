# Changelog

## Unreleased

- Render assistant-emitted `ChatImageContent` from `chat/contentReceived` inline in the chat buffer (e.g. images returned by the OpenAI `image_generation` tool). Decodes the base64 payload and displays it via an overlay carrying `create-image` data (overlay rather than `display` text-property so `markdown-mode`/`gfm-mode` font-lock cannot strip it on `font-lock-ensure`), falling back to a `[Image: <mediaType>, <N> bytes]` placeholder on TTY frames or when the image type is unavailable in the current Emacs build. Sizing is configurable via `eca-chat-image-max-width` (default `fit-window` — scales to the chat window body width, hard-capped by `eca-chat-image-window-fit-cap`, default 800px) and `eca-chat-image-max-height` (default `nil`); a fixed integer or `nil` (unconstrained) are also accepted for `eca-chat-image-max-width`. Per-chat zoom: `eca-chat-image-zoom-in` (`C-c i +` / `C-c i =`), `eca-chat-image-zoom-out` (`C-c i -`), and `eca-chat-image-zoom-reset` (`C-c i 0`) mutate the buffer-local `eca-chat-image-scale` (multiplier on the resolved width cap, default `1.0`) by `eca-chat-image-scale-step` (default `1.2`) and re-rasterize already-rendered images via `image-flush` so existing images snap to the new size without re-fetching. Save inline images to disk via `eca-chat-save-image-at-point` (`C-c i s`), also reachable via `RET` or `mouse-2` directly on the image overlay (with a tooltip describing media type / byte count), with the destination customizable via `eca-chat-save-image-directory` (default `workspace-root`, resolving to `<root>/.eca/images/`; also accepts a literal path string or `nil` for `default-directory`) and `eca-chat-save-image-filename-format` (default `eca-image-<YYYYMMDD-HHMMSS>.<ext>`); saved bytes are pulled directly from the existing overlay via `image-property :data`, so display zoom does not affect the file and no server round-trip is needed.
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
