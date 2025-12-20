;;; eca-chat-media.el --- Media handling for ECA chat -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Media handling for ECA chat: saving screenshots, mime type mapping,
;; and yank-media handlers.
;;
;;; Code:

(require 'f)
(require 'eca-util)

(eval-when-compile
  (require 'eca-chat))

(declare-function eca-session "eca")
(declare-function eca-assert-session-running "eca")
(declare-function eca-chat--get-last-buffer "eca-chat")
(declare-function eca-chat--context->str "eca-chat")
(declare-function eca-chat--insert-prompt "eca-chat")
(declare-function eca-chat--select-window "eca-chat")

(defconst eca-chat-media--mime-extension-map
  '(("image/png" . "png")
    ("image/x-png" . "png")
    ("image/jpeg" . "jpg")
    ("image/jpg" . "jpg")
    ("image/gif" . "gif")
    ("image/webp" . "webp")
    ("image/heic" . "heic")
    ("image/heif" . "heif")
    ("image/svg+xml" . "svg"))
  "Mapping of mime types to screenshot file extensions.")

(defun eca-chat-media--extension-for-type (type)
  "Return file extension (without dot) for mime TYPE.
TYPE can be a string or symbol."
  (let* ((type-str (if (symbolp type) (symbol-name type) type))
         (clean (and type-str (string-trim type-str))))
    (or (cdr (assoc-string clean eca-chat-media--mime-extension-map t))
        (when clean
          (let* ((parts (split-string clean "/"))
                 (raw-subtype (cadr parts))
                 (subtype (car (split-string (or raw-subtype "") "\\+"))))
            (unless (string-empty-p subtype)
              subtype)))
        "png")))

(defun eca-chat-media--yank-image-handler (type data)
  "Handler for yank-media to insert images from clipboard.
TYPE is the MIME type (e.g., 'image/png').
DATA is the binary image data as a string."
  (when-let* ((session (eca-session))
              (chat-buffer (eca-chat--get-last-buffer session))
              (extension (eca-chat-media--extension-for-type type))
              (output-path (make-temp-file "eca-screenshot-" nil (concat "." extension))))
    (condition-case err
        (progn
          (with-temp-file output-path
            (set-buffer-multibyte nil)
            (insert data))
          (when (f-exists? output-path)
            (eca-chat--with-current-buffer chat-buffer
              (let ((context (list :type "file" :path output-path)))
                (eca-chat--insert-prompt (concat (eca-chat--context->str context 'static) " "))
                (eca-chat--select-window)
                (goto-char (line-end-position)))
              (eca-info "Image yanked and added to prompt: %s"
                        (file-size-human-readable (file-attribute-size (file-attributes output-path)))))))
      (error
       (eca-error "Failed to save yanked image: %s" (error-message-string err))))))

;;;###autoload
(defun eca-chat-media-yank-screenshot ()
  "Yank image from clipboard and add to context.
Uses native `yank-media'. Requires Emacs 29+."
  (interactive)
  (unless (fboundp 'yank-media)
    (user-error "Screenshot yanking requires Emacs 29+ with yank-media support"))
  (let* ((session (eca-session))
         (chat-buffer (eca-chat--get-last-buffer session)))
    (eca-assert-session-running session)
    (unless chat-buffer
      (user-error "Open an ECA chat buffer before yanking a screenshot"))
    (eca-chat--select-window)
    (yank-media)))

(provide 'eca-chat-media)
;;; eca-chat-media.el ends here
