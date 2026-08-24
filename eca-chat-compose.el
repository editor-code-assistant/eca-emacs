;;; eca-chat-compose.el --- ECA chat prompt compose buffer -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Compose a chat prompt in a dedicated markdown buffer instead of
;;  the inline chat prompt field, convenient for long or multi-line
;;  prompts.  `eca-chat-compose' opens a buffer targeting the current
;;  chat; `C-c C-c' sends the buffer content as a prompt to that chat
;;  and `C-c C-k' discards it.  @context and #filepath mentions
;;  complete against the ECA server just like in the chat prompt, and
;;  yanking a clipboard image inserts an @file mention pointing at a
;;  saved screenshot, mirroring the chat buffer behavior.
;;
;;; Code:

(require 'eca-util)
(require 'eca-chat)

;; Variables

(defvar-local eca-chat-compose--target-buffer nil
  "The chat buffer the composed prompt will be sent to.")

;; Internal

(defun eca-chat-compose--yank-image-handler (type data)
  "Save clipboard image DATA of mime TYPE and insert an @file mention.
Writes the image to a temporary eca-screenshot file, like the eca
chat buffer does, and inserts \"@/path/to/file \" at point so the
server picks it up as a file context when the prompt is sent."
  (when-let* ((output-path (eca-chat-media--save-clipboard-image type data)))
    (insert eca-chat-context-prefix output-path " ")
    (eca-info "Image added, size: %s"
              (file-size-human-readable (file-attribute-size (file-attributes output-path))))))

;; Public

(defun eca-chat-compose-tab ()
  "Complete the @/# mention at point, else do markdown cycling.
With point after an @context or #filepath prefix this triggers
`completion-at-point'; anywhere else it behaves like TAB in
`markdown-mode'."
  (interactive)
  (if (eca-chat--completion-type-at-point)
      (completion-at-point)
    (call-interactively #'markdown-cycle)))

(defun eca-chat-compose-yank ()
  "Yank into the compose buffer, routing images through `yank-media'.
A clipboard image is saved to a temporary file and inserted as an
@file mention; anything else falls back to a plain `yank'."
  (interactive)
  (if (and (fboundp 'yank-media)
           (boundp 'yank-media--registered-handlers)
           yank-media--registered-handlers
           (eca-chat--clipboard-image-p))
      (call-interactively #'yank-media)
    (call-interactively #'yank)))

(eca-chat-define-derived-mode eca-chat-compose-mode "eca-chat-compose"
  "Major mode for composing a prompt destined for an ECA chat.
The target chat is captured when the buffer is created by
`eca-chat-compose'.  Yanking a clipboard image inserts an @file
mention pointing at a temporary screenshot file, mirroring the
eca chat buffer behavior.

\\{eca-chat-compose-mode-map}"
  (setq header-line-format
        (substitute-command-keys
         "Compose prompt: \\[eca-chat-compose-send] to send, \
\\[eca-chat-compose-cancel] to cancel"))
  ;; ECA server completion for @contexts and #filepaths, mirroring the
  ;; chat prompt setup including its completion-style overrides.
  (setq-local completion-at-point-functions (list #'eca-chat-completion-at-point))
  (setq-local completion-category-defaults
              (cons '(eca-capf (styles basic substring))
                    completion-category-defaults))
  (setq-local completion-ignore-case t)
  ;; Paste image from clipboard support, mirroring eca-chat-mode: drop
  ;; the handlers inherited from markdown-mode (which insert markdown
  ;; image links) and register the @file mention handler.
  (when (fboundp 'yank-media-handler)
    (setq-local yank-media--registered-handlers nil)
    (yank-media-handler "image/png" #'eca-chat-compose--yank-image-handler)
    (yank-media-handler "image/jpeg" #'eca-chat-compose--yank-image-handler)
    (yank-media-handler "image/jpg" #'eca-chat-compose--yank-image-handler)
    (yank-media-handler "image/gif" #'eca-chat-compose--yank-image-handler)
    (yank-media-handler "image/webp" #'eca-chat-compose--yank-image-handler)))

(define-key eca-chat-compose-mode-map (kbd "C-c C-c") #'eca-chat-compose-send)
(define-key eca-chat-compose-mode-map (kbd "C-c C-k") #'eca-chat-compose-cancel)
(define-key eca-chat-compose-mode-map [remap yank] #'eca-chat-compose-yank)
(define-key eca-chat-compose-mode-map (kbd "TAB") #'eca-chat-compose-tab)
(define-key eca-chat-compose-mode-map (kbd "<tab>") #'eca-chat-compose-tab)

;;;###autoload
(defun eca-chat-compose ()
  "Compose a prompt for the current chat in a dedicated buffer.
When called from a chat buffer the prompt targets that chat,
otherwise it targets the session's last used chat.
\\<eca-chat-compose-mode-map>\\[eca-chat-compose-send] sends the \
buffer content as a prompt to that chat;
\\[eca-chat-compose-cancel] discards it."
  (interactive)
  (let ((session (eca-session)))
    (eca-assert-session-running session)
    (let ((target (if (derived-mode-p 'eca-chat-mode)
                      (current-buffer)
                    (eca-chat--get-last-buffer session))))
      (unless (buffer-live-p target)
        (user-error "No chat buffer found to compose for"))
      (let ((buffer (generate-new-buffer
                     (format "*eca-compose:%s*" (buffer-name target)))))
        (with-current-buffer buffer
          (eca-chat-compose-mode)
          (setq eca-chat-compose--target-buffer target)
          ;; Make `eca-session' resolve to the target's session from
          ;; this buffer, so completion and the send path work
          ;; regardless of where the compose window ends up.
          (setq-local eca--session-id-cache (eca--session-id session))
          (when-let* ((dir (car (eca--session-workspace-folders session))))
            (setq-local default-directory dir))
          ;; Carry the target's chat id so @/# completion queries
          ;; (chat/queryContext, chat/queryFiles) run against that chat.
          (setq-local eca-chat--id (buffer-local-value 'eca-chat--id target)))
        (pop-to-buffer buffer)))))

(defun eca-chat-compose-send ()
  "Send the composed prompt to the captured target chat.
Kills the compose buffer afterwards."
  (interactive)
  (let ((text (string-trim
               (buffer-substring-no-properties (point-min) (point-max))))
        (target eca-chat-compose--target-buffer)
        (session (eca-session)))
    (eca-assert-session-running session)
    (when (string-empty-p text)
      (user-error "Nothing to send"))
    (unless (buffer-live-p target)
      (user-error "The target chat buffer no longer exists"))
    (setf (eca--session-last-chat-buffer session) target)
    (eca-chat--with-current-buffer target
      (eca-chat--send-prompt session text))
    (quit-window t)
    (eca-info "Prompt sent to %s" (buffer-name target))))

(defun eca-chat-compose-cancel ()
  "Discard the composed prompt and kill the compose buffer."
  (interactive)
  (quit-window t)
  (eca-info "Compose cancelled"))

(provide 'eca-chat-compose)
;;; eca-chat-compose.el ends here
