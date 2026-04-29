;;; eca-chat-image.el --- ECA chat inline image rendering and saving -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Inline image support for ECA chat: rendering assistant-emitted
;;  `ChatImageContent' as overlays, configurable sizing and per-chat
;;  zoom, and saving the original bytes to disk via a keybinding or
;;  the image overlay's own RET / mouse-2 handler.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'eca-util)
(require 'eca-chat-expandable)

;; Forward declarations for eca-chat.el core
(declare-function eca-chat--insert "eca-chat")
(declare-function eca-chat--content-insertion-point "eca-chat")
(declare-function eca-chat--add-text-content "eca-chat")

;;;; Customization

(defcustom eca-chat-image-max-width 'fit-window
  "Maximum width for inline images rendered in chat.
Applied via the `:max-width' property of `create-image' so the image
is scaled down (preserving aspect ratio) when larger than this value.
The value can be:
- `fit-window' (default) — scale to the chat window body width in
  pixels, capped at `eca-chat-image-window-fit-cap' so very wide
  frames don't produce huge images.
- An integer — a fixed pixel cap.
- nil — no width constraint (image's native pixel width)."
  :type '(choice (const :tag "Fit chat window (default)" fit-window)
                 (const :tag "Unconstrained" nil)
                 (integer :tag "Pixels"))
  :group 'eca)

(defcustom eca-chat-image-window-fit-cap 800
  "Hard pixel cap applied when `eca-chat-image-max-width' is `fit-window'.
Prevents inline images from becoming oversized on very wide chat
windows.  Has no effect for other values of `eca-chat-image-max-width'."
  :type 'integer
  :group 'eca)

(defcustom eca-chat-image-max-height nil
  "Maximum height in pixels for inline images rendered in chat.
Applied via the `:max-height' property of `create-image'.  When nil,
no height constraint is applied; usually `eca-chat-image-max-width'
is enough since chat images are typically wider than tall."
  :type '(choice (const :tag "Unconstrained" nil)
                 (integer :tag "Pixels"))
  :group 'eca)

(defcustom eca-chat-image-scale-step 1.2
  "Multiplicative step for the per-chat image zoom commands.
Applied to the buffer-local `eca-chat-image-scale' on each call to
`eca-chat-image-zoom-in' (multiply) or `eca-chat-image-zoom-out'
\(divide).  A value of 1.0 disables zooming."
  :type 'number
  :group 'eca)

(defvar-local eca-chat-image-scale 1.0
  "Per-chat zoom multiplier for inline images.
Multiplied into the resolved width cap from `eca-chat-image-max-width'
so the user can interactively scale all images in the current chat
buffer up or down via `eca-chat-image-zoom-in', `eca-chat-image-zoom-out',
and `eca-chat-image-zoom-reset'.  Buffer-local so each chat keeps its
own zoom level.")

(defcustom eca-chat-save-image-directory 'workspace-root
  "Default directory for `eca-chat-save-image-at-point'.
The value can be:
- `workspace-root' (default) — save under the current workspace
  root in a `.eca/images/' subdirectory, mirroring the idiom used
  by `eca-chat-save-chat-initial-path'.
- A string — used verbatim as the initial save directory.
- nil — fall back to `default-directory' at the time of saving."
  :type '(choice
          (const :tag "Workspace root (.eca/images/)" workspace-root)
          (string :tag "Custom directory")
          (const :tag "default-directory" nil))
  :group 'eca)

(defcustom eca-chat-save-image-filename-format "eca-image-%s.%s"
  "Format string for default filenames in `eca-chat-save-image-at-point'.
Receives two `%s' arguments via `format': a timestamp string built
from `format-time-string' with %Y%m%d-%H%M%S, and the file
extension derived from the image media type (e.g. `png', `jpg')."
  :type 'string
  :group 'eca)

;;;; Media-type tables

(defconst eca-chat--media-type->image-type
  '(("image/png" . png)
    ("image/x-png" . png)
    ("image/jpeg" . jpeg)
    ("image/jpg" . jpeg)
    ("image/gif" . gif)
    ("image/webp" . webp)
    ("image/svg+xml" . svg))
  "Mapping of MIME types to Emacs `image-type' symbols.
Formats that Emacs does not natively support (e.g. HEIC/HEIF) are
intentionally absent so the renderer falls back to text.")

(defconst eca-chat--media-type->extension
  '(("image/png" . "png")
    ("image/x-png" . "png")
    ("image/jpeg" . "jpg")
    ("image/jpg" . "jpg")
    ("image/gif" . "gif")
    ("image/webp" . "webp")
    ("image/svg+xml" . "svg"))
  "Mapping of MIME types to file extensions for saved images.
Used by `eca-chat--default-save-image-filename' so the suggested
file name carries a recognizable extension.")

;;;; Image building / sizing

(defun eca-chat--image-type-from-media-type (media-type)
  "Return the Emacs `image-type' symbol for MEDIA-TYPE, or nil.
Returns nil when MEDIA-TYPE is unknown or the resolved type is not
available in this Emacs build (see `image-type-available-p')."
  (when-let* ((sym (alist-get media-type eca-chat--media-type->image-type
                              nil nil #'string=)))
    (and (image-type-available-p sym) sym)))

(defun eca-chat--resolve-image-max-width ()
  "Resolve `eca-chat-image-max-width' to a pixel value or nil.
Returns nil when no width cap should be applied.  When the user
selected `fit-window' the base is the chat window body width in
pixels (or the frame width when no window is currently displayed),
clamped to `eca-chat-image-window-fit-cap' from above.  The base is
then multiplied by the buffer-local `eca-chat-image-scale' zoom
factor and clamped to a 50-pixel floor."
  (let ((base (pcase eca-chat-image-max-width
                ('nil nil)
                ('fit-window
                 (let* ((win (get-buffer-window (current-buffer)))
                        (avail (if win
                                   (window-body-width win t)
                                 (frame-pixel-width))))
                   (max 100 (min eca-chat-image-window-fit-cap avail))))
                ((and (pred integerp) n) n)
                (_ nil))))
    (when base
      (max 50 (round (* base (or eca-chat-image-scale 1.0)))))))

(defun eca-chat--build-inline-image (image-content)
  "Return an Emacs image object for IMAGE-CONTENT, or nil.
Returns nil on TTY frames or when the media type is unsupported.
IMAGE-CONTENT is a plist with `:mediaType' and `:base64' keys."
  (when-let* (((display-graphic-p))
              (b64 (plist-get image-content :base64))
              (binary (base64-decode-string b64))
              (image-type (eca-chat--image-type-from-media-type
                           (plist-get image-content :mediaType))))
    (let ((max-w (eca-chat--resolve-image-max-width)))
      (apply #'create-image binary image-type t
             (append
              (when max-w (list :max-width max-w))
              (when eca-chat-image-max-height
                (list :max-height eca-chat-image-max-height)))))))

(defun eca-chat--image-fallback-string (image-content)
  "Return the textual fallback string for IMAGE-CONTENT.
Used on TTY frames, for unsupported image formats, and inside
subagent expandable blocks where overlays cannot ride along."
  (let* ((media-type (plist-get image-content :mediaType))
         (b64 (plist-get image-content :base64))
         (bytes (if b64 (length (base64-decode-string b64)) 0)))
    (concat "\n"
            (propertize (format "[Image: %s, %d bytes]"
                                (or media-type "unknown") bytes)
                        'font-lock-face 'eca-chat-system-messages-face)
            "\n")))

;;;; Per-chat zoom

(defun eca-chat--refresh-rendered-images ()
  "Re-rasterize inline images in the current chat at the current scale.
Walks overlays carrying the `eca-chat-image' property, updates each
underlying image object's `:max-width' and `:max-height' via
`image-property', then calls `image-flush' so Emacs re-rasterizes on
the next redisplay.  Used by the per-chat zoom commands."
  (let ((max-w (eca-chat--resolve-image-max-width)))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'eca-chat-image)
        (let ((image (overlay-get ov 'display)))
          (when (and (consp image) (eq (car image) 'image))
            (when max-w
              (setf (image-property image :max-width) max-w))
            (when eca-chat-image-max-height
              (setf (image-property image :max-height)
                    eca-chat-image-max-height))
            (image-flush image)))))))

(defun eca-chat-image-zoom-in ()
  "Increase the inline-image zoom in this chat by one step.
The buffer-local `eca-chat-image-scale' is multiplied by
`eca-chat-image-scale-step' and existing inline images are
re-rasterized at the new size."
  (interactive)
  (setq-local eca-chat-image-scale
              (* eca-chat-image-scale eca-chat-image-scale-step))
  (eca-chat--refresh-rendered-images)
  (eca-info "Image zoom: %d%%" (round (* 100 eca-chat-image-scale))))

(defun eca-chat-image-zoom-out ()
  "Decrease the inline-image zoom in this chat by one step.
The buffer-local `eca-chat-image-scale' is divided by
`eca-chat-image-scale-step' and existing inline images are
re-rasterized at the new size."
  (interactive)
  (setq-local eca-chat-image-scale
              (/ eca-chat-image-scale eca-chat-image-scale-step))
  (eca-chat--refresh-rendered-images)
  (eca-info "Image zoom: %d%%" (round (* 100 eca-chat-image-scale))))

(defun eca-chat-image-zoom-reset ()
  "Reset the inline-image zoom in this chat to 100%."
  (interactive)
  (setq-local eca-chat-image-scale 1.0)
  (eca-chat--refresh-rendered-images)
  (eca-info "Image zoom: 100%%"))

;;;; Save to disk

(defun eca-chat--image-overlay-at-point ()
  "Return the inline-image overlay at point, or nil.
Looks at overlays at point, then at the position just before point
\(so point right after an image still finds it), then any image
overlay on the current line.  Filters by `eca-chat-image'."
  (cl-flet ((image-ov (ovs) (seq-find (lambda (ov)
                                        (overlay-get ov 'eca-chat-image))
                                      ovs)))
    (or (image-ov (overlays-at (point)))
        (image-ov (overlays-at (max (point-min) (1- (point)))))
        (image-ov (overlays-in (line-beginning-position)
                               (line-end-position))))))

(defun eca-chat--last-image-overlay ()
  "Return the rightmost inline-image overlay in this buffer, or nil.
Used by `eca-chat-save-image-at-point' as a fallback when point is
not on any image overlay."
  (let (best)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (and (overlay-get ov 'eca-chat-image)
                 (or (null best)
                     (> (overlay-start ov) (overlay-start best))))
        (setq best ov)))
    best))

(defun eca-chat--default-save-image-dir ()
  "Resolve `eca-chat-save-image-directory' to a directory string.
For `workspace-root' returns the workspace root joined with
`.eca/images/'.  Strings are returned verbatim (as a directory).
nil falls back to `default-directory'."
  (file-name-as-directory
   (pcase eca-chat-save-image-directory
     ('workspace-root
      (expand-file-name ".eca/images/" (eca-find-root-for-buffer)))
     ((and (pred stringp) s) s)
     (_ default-directory))))

(defun eca-chat--default-save-image-filename (media-type)
  "Build the default filename for an image of MEDIA-TYPE.
Combines a timestamp from `format-time-string' and the extension
derived from MEDIA-TYPE via `eca-chat--media-type->extension',
formatted with `eca-chat-save-image-filename-format'.  Falls back
to a `bin' extension when MEDIA-TYPE is unknown."
  (let* ((ts (format-time-string "%Y%m%d-%H%M%S"))
         (ext (or (cdr (assoc media-type
                              eca-chat--media-type->extension))
                  "bin")))
    (format eca-chat-save-image-filename-format ts ext)))

(defun eca-chat-save-image-at-point (&optional overlay)
  "Save the inline image at point (or last image) to a file.
Looks up the image overlay at point and falls back to the last
image overlay in the buffer when none is found.  When called with
non-nil OVERLAY, save that overlay's image directly (used by the
per-overlay keymap so a click saves the clicked image regardless
of where point currently sits).

Prompts for a destination using `eca-chat-save-image-directory'
and `eca-chat-save-image-filename-format' for the default path.
Creates parent directories as needed.  Writes the original bytes
verbatim — display zoom does not affect the saved file."
  (interactive)
  (let ((ov (or overlay
                (eca-chat--image-overlay-at-point)
                (eca-chat--last-image-overlay))))
    (cond
     ((null ov)
      (eca-warn "No image at point or in this chat"))
     (t
      (let* ((image (overlay-get ov 'display))
             (data (and (consp image) (eq (car image) 'image)
                        (image-property image :data)))
             (media-type (overlay-get ov 'eca-chat-image-media-type)))
        (cond
         ((not data)
          (eca-warn "Image overlay has no inline data to save"))
         (t
          (let* ((dir (eca-chat--default-save-image-dir))
                 (default-name
                  (eca-chat--default-save-image-filename media-type))
                 (target (read-file-name "Save image to: "
                                         dir nil nil default-name)))
            (when (and target (not (string-empty-p target)))
              (let ((parent (file-name-directory
                             (expand-file-name target))))
                (when (and parent (not (file-directory-p parent)))
                  (make-directory parent t)))
              (let ((coding-system-for-write 'no-conversion))
                (write-region data nil target nil 'nomessage))
              (eca-info "Saved image to %s" target))))))))))

;;;; Render entry point (called by eca-chat--render-content)

(defun eca-chat--render-image-content (image-content parent-tool-call-id)
  "Render IMAGE-CONTENT into the current chat buffer.
On a graphical frame with a supported media type the image is
inserted via an overlay so that `font-lock-ensure' (which manages
the `display' text-property in `markdown-mode'/`gfm-mode') cannot
strip it.  Otherwise a textual placeholder is shown.

When PARENT-TOOL-CALL-ID is non-nil the content is appended into
that expandable tool-call block; expandable content stores its body
as a plain string, so overlays would not survive — for that path we
always use the textual fallback."
  (let ((image (and (not parent-tool-call-id)
                    (eca-chat--build-inline-image image-content))))
    (cond
     (image
      (save-excursion
        (goto-char (eca-chat--content-insertion-point))
        (let ((start (point)))
          (eca-chat--insert "\n \n")
          (let ((ov (make-overlay (1+ start) (+ 2 start) (current-buffer))))
            (overlay-put ov 'display image)
            (overlay-put ov 'eca-chat-image t)
            (overlay-put ov 'eca-chat-image-media-type
                         (plist-get image-content :mediaType))
            (overlay-put ov 'mouse-face 'highlight)
            (overlay-put
             ov 'help-echo
             (format "Image (%s, %d bytes) — RET or mouse-2 to save"
                     (or (plist-get image-content :mediaType) "?")
                     (length (image-property image :data))))
            ;; Bind both `RET' (terminal) and `<return>' (graphical
            ;; frame function-key event) so RET works regardless of
            ;; whether the chat mode-map has the function-key bound;
            ;; otherwise `eca-chat--key-pressed-return' shadows ours.
            (let* ((save-fn (lambda ()
                              (interactive)
                              (eca-chat-save-image-at-point ov)))
                   (map (make-sparse-keymap)))
              (define-key map (kbd "RET") save-fn)
              (define-key map (kbd "<return>") save-fn)
              (define-key map [mouse-2] save-fn)
              (overlay-put ov 'keymap map))))))
     (parent-tool-call-id
      (eca-chat--update-expandable-content
       parent-tool-call-id nil
       (eca-chat--image-fallback-string image-content) t))
     (t
      (eca-chat--add-text-content
       (eca-chat--image-fallback-string image-content))))))

(provide 'eca-chat-image)
;;; eca-chat-image.el ends here
