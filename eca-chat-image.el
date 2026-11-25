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
;;  `ChatImageContent' as overlays, compact until expanded with RET,
;;  under a header line of actions to zoom, expand and save each
;;  image, saving the original bytes to disk, and thumbnails toggled
;;  with RET on image mentions, like screenshots pasted in the prompt.
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
(declare-function eca-chat--action-button "eca-chat")
(declare-function eca-chat--protect-non-prompt "eca-chat")

;;;; Customization

(defcustom eca-chat-image-compact-height 300
  "Max height in pixels of inline images until they are expanded.
Images the assistant sends, like generated ones, start compact so
they don't take over the chat, unless `eca-chat-image-start-expanded'
is non-nil.  RET on an image, or its expand button, expands it to
the width `eca-chat-image-max-width' allows, then shrinks it back."
  :type 'integer
  :group 'eca)

(defcustom eca-chat-image-start-expanded nil
  "Whether inline images start expanded instead of compact.
Compact images are at most `eca-chat-image-compact-height' pixels
tall, expanded ones as wide as `eca-chat-image-max-width' allows.
Either way, RET on an image switches it to the other size."
  :type 'boolean
  :group 'eca)

(defcustom eca-chat-image-max-width 'fit-window
  "Maximum width for inline images rendered in chat.
Applied via the `:max-width' property of `create-image' so the image
is scaled down (preserving aspect ratio) when larger than this value.
It limits expanded images, and compact ones too, which are also at
most `eca-chat-image-compact-height' pixels tall.
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
Applied via the `:max-height' property of `create-image', to expanded
images too.  When nil, expanded images are only limited by
`eca-chat-image-max-width', while compact ones are at most
`eca-chat-image-compact-height' pixels tall either way."
  :type '(choice (const :tag "Unconstrained" nil)
                 (integer :tag "Pixels"))
  :group 'eca)

(defcustom eca-chat-image-thumbnail-size 200
  "Maximum width and height in pixels of image mention thumbnails.
RET on the mention of an image, like a screenshot pasted in the
prompt, toggles its thumbnail, see `eca-chat-toggle-image-thumbnail'."
  :type 'integer
  :group 'eca)

(defcustom eca-chat-image-show-thumbnails t
  "Whether image mentions show their thumbnail right away.
Image mentions are screenshots pasted in the prompt, image files
added as context and images mentioned in sent messages.  When nil
they show as text until RET toggles their thumbnail.  Mentions of
remote or missing files always start as text."
  :type 'boolean
  :group 'eca)

(defvar eca-chat--inhibit-auto-thumbnails nil
  "When non-nil, inserted image mentions don't show their thumbnail.")

(defcustom eca-chat-image-scale-step 1.2
  "Factor by which the image zoom commands grow or shrink an image.
`eca-chat-image-zoom-in' multiplies the height of an inline image by
it, and `eca-chat-image-zoom-out' divides it.  A value of 1.0
disables zooming."
  :type 'number
  :group 'eca)

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

(defface eca-chat-image-label-face
  '((t :inherit font-lock-comment-face :slant normal))
  "Face for the format and size of inline images, in their header."
  :group 'eca)

(defface eca-chat-image-action-face
  '((t :inherit font-lock-comment-face :slant normal))
  "Face for the actions in the header of inline images, like zoom.
In the color of comments, like the actions of flags, see
`eca-chat-flag-action-face'."
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

(defconst eca-chat--image-file-extensions
  '("png" "jpg" "jpeg" "gif" "webp" "svg" "bmp" "tif" "tiff" "heic" "heif")
  "Extensions of the image files whose mentions toggle a thumbnail.")

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
selected `fit-window' the value is the chat window body width in
pixels (or the frame width when no window is currently displayed),
clamped to `eca-chat-image-window-fit-cap' from above."
  (pcase eca-chat-image-max-width
    ('nil nil)
    ('fit-window
     (let* ((win (get-buffer-window (current-buffer)))
            (avail (if win
                       (window-body-width win t)
                     (frame-pixel-width))))
       (max 100 (min eca-chat-image-window-fit-cap avail))))
    ((and (pred integerp) n) n)
    (_ nil)))

(defun eca-chat--image-max-height (height)
  "Return the max height in pixels of an image shown HEIGHT tall.
HEIGHT nil means expanded, only capped by `eca-chat-image-max-height'."
  (if (and height eca-chat-image-max-height)
      (min height eca-chat-image-max-height)
    (or height eca-chat-image-max-height)))

(defun eca-chat--create-inline-image (data type height)
  "Return an image of DATA, of image TYPE, at most HEIGHT pixels tall.
HEIGHT nil shows it expanded.  Either way it is no wider than
`eca-chat-image-max-width' allows."
  (let ((max-w (eca-chat--resolve-image-max-width))
        (max-h (eca-chat--image-max-height height)))
    (apply #'create-image data type t
           (append (when max-w (list :max-width max-w))
                   (when max-h (list :max-height max-h))))))

(defun eca-chat--image-initial-height ()
  "Return the height inline images start at, nil for expanded.
See `eca-chat-image-start-expanded'."
  (unless eca-chat-image-start-expanded
    eca-chat-image-compact-height))

(defun eca-chat--build-inline-image (image-content &optional height)
  "Return an Emacs image object for IMAGE-CONTENT, or nil.
The image is at most HEIGHT pixels tall, expanded when nil.  Returns
nil on TTY frames or when the media type is unsupported.
IMAGE-CONTENT is a plist with `:mediaType' and `:base64' keys."
  (when-let* (((display-graphic-p))
              (b64 (plist-get image-content :base64))
              (binary (base64-decode-string b64))
              (image-type (eca-chat--image-type-from-media-type
                           (plist-get image-content :mediaType))))
    (eca-chat--create-inline-image binary image-type height)))

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

;;;; Finding inline images

(defun eca-chat--image-overlay-in (ovs)
  "Return the first inline-image overlay of OVS, or nil."
  (seq-find (lambda (ov) (overlay-get ov 'eca-chat-image)) ovs))

(defun eca-chat--image-overlay-near (pos)
  "Return the inline-image overlay at POS or right before it, or nil.
So that point right after an image still finds it."
  (eca-chat--image-overlay-in
   (append (overlays-at pos)
           (overlays-at (max (point-min) (1- pos))))))

(defun eca-chat--image-overlay-at-point ()
  "Return the inline-image overlay at point, or nil.
Looks at overlays at point, then at the position just before point
\(so point right after an image still finds it), then any image
overlay on the current line.  On the header line of an image, that
is the image right below.  Filters by `eca-chat-image'."
  (or (eca-chat--image-overlay-near (point))
      (eca-chat--image-overlay-in
       (overlays-in (line-beginning-position) (line-end-position)))
      (when (get-text-property (line-beginning-position)
                               'eca-chat-image-header)
        (let ((below (min (point-max) (1+ (line-end-position)))))
          (eca-chat--image-overlay-in
           (overlays-in below (min (point-max) (1+ below))))))))

(defun eca-chat--last-image-overlay ()
  "Return the rightmost inline-image overlay in this buffer, or nil.
Used by the image commands as a fallback when point is not on any
image overlay."
  (let (best)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (and (overlay-get ov 'eca-chat-image)
                 (or (null best)
                     (> (overlay-start ov) (overlay-start best))))
        (setq best ov)))
    best))

(defun eca-chat--image-target (overlay)
  "Return the overlay of the inline image to act on, or nil.
That is OVERLAY when it is live, else the image at point, else the
last image of the chat.  Warn and return nil when there is none."
  (or (and overlay (overlay-buffer overlay) overlay)
      (eca-chat--image-overlay-at-point)
      (eca-chat--last-image-overlay)
      (ignore (eca-warn "No image at point or in this chat"))))

;;;; Size of inline images

(defconst eca-chat--image-min-height 50
  "Height in pixels below which zooming out no longer shrinks images.")

(defun eca-chat--image-expanded-p (ov)
  "Return non-nil when the inline image of OV is expanded."
  (null (overlay-get ov 'eca-chat-image-height)))

(defun eca-chat--image-variant (ov height)
  "Return the image of OV at most HEIGHT pixels tall, nil for expanded."
  (let ((image (overlay-get ov 'display)))
    (eca-chat--create-inline-image (image-property image :data)
                                   (image-property image :type)
                                   height)))

(defun eca-chat--image-displayed-height (image)
  "Return how many pixels tall IMAGE shows, or nil when unknown.
It is unknown when the selected frame can't display images."
  (when (display-images-p)
    (ignore-errors (cdr (image-size image t)))))

(defun eca-chat--image-set-height (ov height)
  "Show the inline image of OV at most HEIGHT pixels tall.
HEIGHT nil expands it.  A fresh image replaces the shown one, which
makes redisplay draw it at its new size, and the expand/shrink
button of its header tells what it does next."
  (with-current-buffer (overlay-buffer ov)
    (let ((image (eca-chat--image-variant ov height)))
      (overlay-put ov 'eca-chat-image-height height)
      (overlay-put ov 'display image))
    (eca-chat--image-update-toggle ov)))

(defun eca-chat--image-report-zoom (ov &optional full)
  "Tell how big the inline image of OV shows, as a part of its full size.
FULL is the height in pixels of the image expanded, computed when nil."
  (when-let* ((full (or full (eca-chat--image-displayed-height
                              (eca-chat--image-variant ov nil))))
              ((> full 0))
              (height (eca-chat--image-displayed-height
                       (overlay-get ov 'display))))
    (eca-info "Image zoom: %d%%" (round (* 100.0 (/ (float height) full))))))

(defun eca-chat-image-toggle-size (&optional overlay)
  "Expand the inline image at point, or shrink it back to compact.
Compact images are at most `eca-chat-image-compact-height' pixels
tall, expanded ones as wide as `eca-chat-image-max-width' allows.
Without an image at point, act on the last image of the chat.
OVERLAY is the image overlay to act on instead."
  (interactive)
  (when-let* ((ov (eca-chat--image-target overlay)))
    (eca-chat--image-set-height
     ov (and (eca-chat--image-expanded-p ov) eca-chat-image-compact-height))))

(defun eca-chat-image-zoom-in (&optional overlay)
  "Grow the inline image at point by `eca-chat-image-scale-step'.
It grows up to its expanded size.  Without an image at point, act
on the last image of the chat.  OVERLAY is the image overlay to act
on instead."
  (interactive)
  (when-let* ((ov (eca-chat--image-target overlay)))
    (with-current-buffer (overlay-buffer ov)
      (if (eca-chat--image-expanded-p ov)
          (eca-info "Image already at its full size")
        (let* ((height (or (eca-chat--image-displayed-height
                            (overlay-get ov 'display))
                           (overlay-get ov 'eca-chat-image-height)))
               (target (round (* height eca-chat-image-scale-step)))
               (full (eca-chat--image-displayed-height
                      (eca-chat--image-variant ov nil))))
          (eca-chat--image-set-height ov (unless (and full (>= target full))
                                           target))
          (eca-chat--image-report-zoom ov full))))))

(defun eca-chat-image-zoom-out (&optional overlay)
  "Shrink the inline image at point by `eca-chat-image-scale-step'.
It shrinks down to `eca-chat--image-min-height' pixels tall.
Without an image at point, act on the last image of the chat.
OVERLAY is the image overlay to act on instead."
  (interactive)
  (when-let* ((ov (eca-chat--image-target overlay)))
    (with-current-buffer (overlay-buffer ov)
      (let* ((height (or (eca-chat--image-displayed-height
                          (overlay-get ov 'display))
                         (overlay-get ov 'eca-chat-image-height)
                         eca-chat-image-compact-height))
             (target (max eca-chat--image-min-height
                          (round (/ height
                                    (float eca-chat-image-scale-step))))))
        (if (>= target height)
            (eca-info "Image already at its smallest size")
          (eca-chat--image-set-height ov target)
          (eca-chat--image-report-zoom ov))))))

(defun eca-chat-image-zoom-reset (&optional overlay)
  "Show the inline image at point at its initial size again.
That is compact, unless `eca-chat-image-start-expanded' is non-nil.
Without an image at point, act on the last image of the chat.
OVERLAY is the image overlay to act on instead."
  (interactive)
  (when-let* ((ov (eca-chat--image-target overlay)))
    (eca-chat--image-set-height ov (eca-chat--image-initial-height))))

;;;; Header line of inline images

(defun eca-chat--image-header-part (string)
  "Return STRING marked as a part of the header line of an image.
Markdown leaves the header alone, so that it doesn't fontify it."
  (propertize string 'eca-chat-image-header t 'eca-no-fontify t))

(defun eca-chat--image-action (ov label help command)
  "Return the LABEL action in the header of the inline image of OV.
RET or a click on it runs COMMAND on the image, HELP tells what it
does."
  (eca-chat--action-button 'eca-chat-image-action-face label help
                           (lambda () (funcall command ov))))

(defun eca-chat--image-toggle-button (ov)
  "Return the button expanding or shrinking the inline image of OV.
Its label tells which it does, depending on the size of the image."
  (propertize
   (if (eca-chat--image-expanded-p ov)
       (eca-chat--image-action ov "shrink" "Shrink the image back to compact"
                               #'eca-chat-image-toggle-size)
     (eca-chat--image-action ov "expand" "Expand the image to the chat width"
                             #'eca-chat-image-toggle-size))
   'eca-chat-image-toggle t))

(defun eca-chat--image-header (ov)
  "Return the header line of the inline image of OV.
It tells the format and size of the image, then the actions on it:
zoom out, zoom in, expand or shrink, and save."
  (let ((ext (alist-get (overlay-get ov 'eca-chat-image-media-type)
                        eca-chat--media-type->extension nil nil #'string=)))
    (eca-chat--image-header-part
     (concat
      (propertize (format "Image · %s · %s"
                          (upcase (or ext "?"))
                          (file-size-human-readable
                           (overlay-get ov 'eca-chat-image-bytes) 'si " " "B"))
                  'font-lock-face 'eca-chat-image-label-face
                  'help-echo "RET on the image expands or shrinks it")
      "   "
      (eca-chat--image-action ov "−" "Zoom out the image"
                              #'eca-chat-image-zoom-out)
      " "
      (eca-chat--image-action ov "+" "Zoom in the image"
                              #'eca-chat-image-zoom-in)
      "  "
      (eca-chat--image-toggle-button ov)
      "  "
      (eca-chat--image-action ov "save" "Save the image to a file"
                              #'eca-chat-save-image-at-point)))))

(defun eca-chat--image-header-bounds (ov)
  "Return (START . END) of the header line above the image of OV.
Return nil when the image has no header."
  (let ((end (1- (overlay-start ov))))
    (when (and (> end (point-min))
               (get-text-property (1- end) 'eca-chat-image-header))
      (cons (previous-single-property-change end 'eca-chat-image-header
                                             nil (point-min))
            end))))

(defun eca-chat--image-update-toggle (ov)
  "Make the expand/shrink button above the image of OV fit its size.
Point on the button stays on it, and the button stays read-only."
  (when-let* ((bounds (eca-chat--image-header-bounds ov))
              (start (text-property-any (car bounds) (cdr bounds)
                                        'eca-chat-image-toggle t))
              (end (next-single-property-change
                    start 'eca-chat-image-toggle nil (cdr bounds))))
    (let* ((button (eca-chat--image-header-part
                    (eca-chat--image-toggle-button ov)))
           (len (length button))
           (pt (point)))
      (unless (string= button (buffer-substring-no-properties start end))
        (let ((inhibit-read-only t))
          (delete-region start end))
        (goto-char start)
        (eca-chat--insert button)
        (goto-char (cond ((< pt start) pt)
                         ((< pt end) (+ start (min (- pt start) (1- len))))
                         (t (+ pt (- len (- end start))))))
        (eca-chat--protect-non-prompt start (+ start len))))))

;;;; Save to disk

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
save button above each image, so a click saves that image
regardless of where point currently sits).

Prompts for a destination using `eca-chat-save-image-directory'
and `eca-chat-save-image-filename-format' for the default path.
Creates parent directories as needed.  Writes the original bytes
verbatim — display size does not affect the saved file."
  (interactive)
  (when-let* ((ov (eca-chat--image-target overlay)))
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
            (eca-info "Saved image to %s" target))))))))

;;;; Render entry point (called by eca-chat--render-content)

(defun eca-chat--image-help-echo (_window ov _pos)
  "Return the help shown when hovering the inline image of OV."
  (format "%s: %s, +/-: zoom, s: save"
          (if eca-buttons-allow-mouse "RET or click" "RET or mouse-2")
          (if (eca-chat--image-expanded-p ov) "shrink" "expand")))

(defun eca-chat--inline-image-keymap (ov)
  "Return the keymap of the inline image of OV.
RET and a middle click expand or shrink it, see
`eca-chat--image-keymap', and keys zoom and save it."
  (let ((map (eca-chat--image-keymap
              (lambda () (interactive) (eca-chat-image-toggle-size ov)))))
    (define-key map "+" #'eca-chat-image-zoom-in)
    (define-key map "=" #'eca-chat-image-zoom-in)
    (define-key map "-" #'eca-chat-image-zoom-out)
    (define-key map "0" #'eca-chat-image-zoom-reset)
    (define-key map "s" #'eca-chat-save-image-at-point)
    map))

(defun eca-chat--render-image-content (image-content parent-tool-call-id)
  "Render IMAGE-CONTENT into the current chat buffer.
On a graphical frame with a supported media type the image is
inserted via an overlay so that `font-lock-ensure' (which manages
the `display' text-property in `markdown-mode'/`gfm-mode') cannot
strip it.  It starts compact, see `eca-chat-image-compact-height',
under a header line of actions on it, see `eca-chat--image-header'.
Otherwise a textual placeholder is shown.

When PARENT-TOOL-CALL-ID is non-nil the content is appended into
that expandable tool-call block; expandable content stores its body
as a plain string, so overlays would not survive — for that path we
always use the textual fallback."
  (let* ((height (eca-chat--image-initial-height))
         (image (and (not parent-tool-call-id)
                     (eca-chat--build-inline-image image-content height))))
    (cond
     (image
      (save-excursion
        (goto-char (eca-chat--content-insertion-point))
        ;; Empty until its image char is inserted, under the header
        ;; whose buttons need it.
        (let ((ov (make-overlay (point) (point))))
          (overlay-put ov 'eca-chat-image t)
          (overlay-put ov 'eca-chat-image-media-type
                       (plist-get image-content :mediaType))
          (overlay-put ov 'eca-chat-image-bytes
                       (length (image-property image :data)))
          (overlay-put ov 'eca-chat-image-height height)
          (overlay-put ov 'display image)
          (eca-chat--insert "\n" (eca-chat--image-header ov) "\n")
          (let ((image-start (point)))
            (eca-chat--insert " \n")
            (move-overlay ov image-start (1+ image-start)))
          ;; Gone with its image char, e.g. on a rollback.
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'mouse-face 'highlight)
          (overlay-put ov 'help-echo #'eca-chat--image-help-echo)
          (overlay-put ov 'keymap (eca-chat--inline-image-keymap ov)))))
     (parent-tool-call-id
      (eca-chat--update-expandable-content
       parent-tool-call-id nil
       (eca-chat--image-fallback-string image-content) t))
     (t
      (eca-chat--add-text-content
       (eca-chat--image-fallback-string image-content))))))

(defun eca-chat--image-file-p (path)
  "Return non-nil when PATH has an image file extension."
  (when-let* ((ext (and (stringp path) (file-name-extension path))))
    (and (member (downcase ext) eca-chat--image-file-extensions) t)))

(defun eca-chat--propertize-image-mention (str path)
  "Mark STR as a mention of the image at PATH, then return STR.
STR is left as is when PATH is not an image file.  The mark is the
`eca-chat-image-path' text property, which font-lock leaves alone
and which travels with the text, e.g. through the prompt history."
  (when (eca-chat--image-file-p path)
    (put-text-property 0 (length str) 'eca-chat-image-path path str))
  str)

(defun eca-chat--image-link-at (pos)
  "Return (START END PATH) of the image mention at POS, or nil.
Only the char after POS counts, so a position right after a
mention is not on it."
  (when-let* ((bounds (eca--property-run-bounds pos 'eca-chat-image-path)))
    (list (car bounds) (cdr bounds)
          (get-text-property pos 'eca-chat-image-path))))

(defun eca-chat--image-thumbnail-at (start end)
  "Return the thumbnail overlay shown from START to END, or nil."
  (seq-find (lambda (ov)
              (and (overlay-get ov 'eca-chat-image-thumbnail)
                   (= start (overlay-start ov))
                   (= end (overlay-end ov))))
            (overlays-in start end)))

(defun eca-chat--image-thumbnail (path)
  "Return a thumbnail image of the file at PATH, or nil.
Return nil when Emacs can't decode it.  Remote files are read
through their file handler, which image loading bypasses."
  (let ((props (list :max-width eca-chat-image-thumbnail-size
                     :max-height eca-chat-image-thumbnail-size
                     :ascent 'center)))
    (ignore-errors
      (if (file-remote-p path)
          (apply #'create-image
                 (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally path)
                   (buffer-string))
                 nil t props)
        (apply #'create-image path nil nil props)))))

(defun eca-chat--image-keymap (command)
  "Return a keymap running COMMAND on RET and a middle click.
Also on a left click when `eca-buttons-allow-mouse' is non-nil."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") command)
    (define-key map [mouse-2] command)
    (when eca-buttons-allow-mouse
      (define-key map [mouse-1] command))
    map))

(defun eca-chat--show-image-thumbnail (start end image)
  "Display IMAGE in place of the image mention from START to END.
Only the display changes: the overlay goes away with the mention
text, and RET or a middle click on the thumbnail hides it again.
Text inserted at either end stays out of the overlay.  Return it."
  (let* ((ov (make-overlay start end nil t nil))
         (hide (lambda () (interactive) (delete-overlay ov))))
    (overlay-put ov 'eca-chat-image-thumbnail t)
    (overlay-put ov 'display image)
    (overlay-put ov 'keymap (eca-chat--image-keymap hide))
    (overlay-put ov 'mouse-face 'highlight)
    (overlay-put ov 'help-echo "RET or mouse-2: hide the thumbnail")
    (overlay-put ov 'evaporate t)
    ov))

(defun eca-chat--toggle-image-thumbnail (link)
  "Toggle the thumbnail of LINK, a (START END PATH) image mention."
  (pcase-let ((`(,start ,end ,path) link))
    (if-let* ((ov (eca-chat--image-thumbnail-at start end)))
        (delete-overlay ov)
      (let (image)
        (cond
         ((not (display-images-p))
          (eca-warn "Images can't be displayed in this frame"))
         ((not (file-readable-p path))
          (eca-warn "Image not found: %s" path))
         ((not (setq image (eca-chat--image-thumbnail path)))
          (eca-warn "Can't display image: %s" path))
         (t (eca-chat--show-image-thumbnail start end image)))))))

(defun eca-chat-toggle-image-thumbnail ()
  "Toggle the thumbnail of the image mention at point.
Image mentions are the image files added to the prompt, like pasted
screenshots, and their mentions in sent messages.  The thumbnail,
sized by `eca-chat-image-thumbnail-size', is displayed in place of
the mention; the text is unchanged, and so is the prompt sent."
  (interactive)
  (if-let* ((link (or (eca-chat--image-link-at (point))
                      (and (> (point) (point-min))
                           (eca-chat--image-link-at (1- (point)))))))
      (eca-chat--toggle-image-thumbnail link)
    (eca-warn "No image mention at point")))

(defun eca-chat--auto-show-image-thumbnails (start end)
  "Show the thumbnails of the image mentions between START and END.
Per `eca-chat-image-show-thumbnails', silently skipping frames that
can't display images, mentions already showing one and those of
missing or remote files, which would be fetched over the network."
  (when (and eca-chat-image-show-thumbnails
             (not eca-chat--inhibit-auto-thumbnails)
             (display-images-p))
    (let ((pos start))
      (while (< pos end)
        (when-let* ((link (eca-chat--image-link-at pos))
                    (path (nth 2 link))
                    ((not (eca-chat--image-thumbnail-at (car link) (cadr link))))
                    ((not (file-remote-p path)))
                    ((file-readable-p path))
                    (image (eca-chat--image-thumbnail path)))
          (eca-chat--show-image-thumbnail (car link) (cadr link) image))
        (setq pos (next-single-property-change pos 'eca-chat-image-path
                                               nil end))))))

(defun eca-chat--auto-show-image-thumbnails-after-change (beg end _len)
  "Show the thumbnails of the image mentions inserted from BEG to END.
Meant for `after-change-functions', so image mentions show their
thumbnail however they land: pasted, completed, recalled from the
prompt history, yanked or undone."
  (when (and (< beg end)
             (text-property-not-all beg end 'eca-chat-image-path nil))
    (with-demoted-errors "eca-chat image thumbnails: %S"
      (eca-chat--auto-show-image-thumbnails beg end))))

(defun eca-chat--add-image-link-overlay (start end)
  "Make RET and a middle click toggle the thumbnail of a mention.
The mention goes from START to END and carries `eca-chat-image-path'.
For buffers whose RET doesn't handle image mentions, like the
compose one; the overlay goes away with the mention text."
  (let* ((ov (make-overlay start end nil t nil))
         (toggle (lambda ()
                   (interactive)
                   (when-let* ((buffer (overlay-buffer ov)))
                     (with-current-buffer buffer
                       (when-let* ((link (eca-chat--image-link-at
                                          (overlay-start ov))))
                         (eca-chat--toggle-image-thumbnail link)))))))
    (overlay-put ov 'keymap (eca-chat--image-keymap toggle))
    (overlay-put ov 'mouse-face 'highlight)
    (overlay-put ov 'help-echo "RET or mouse-2: toggle the thumbnail")
    (overlay-put ov 'evaporate t)
    ov))

(defun eca-chat--mention-image-path (token roots)
  "Return the local path of the image mentioned by TOKEN, or nil.
TOKEN is a mention path as sent to the server: absolute, maybe a
remote one, or relative to one of ROOTS.  Local paths must exist;
remote ones are not checked, which would be slow."
  (when (eca-chat--image-file-p token)
    (let ((local (eca--path-remote-to-local token)))
      (cond
       ((file-remote-p local) local)
       ((file-name-absolute-p local)
        (let ((path (expand-file-name local)))
          (and (file-exists-p path) path)))
       (t (seq-some (lambda (root)
                      (let ((path (expand-file-name local root)))
                        (and (not (file-remote-p path))
                             (file-exists-p path)
                             path)))
                    roots))))))

(defun eca-chat--linkify-image-mentions (start end roots)
  "Make the image mentions between START and END toggle a thumbnail.
Mentions are the @path tokens of sent messages, resolved against
ROOTS by `eca-chat--mention-image-path'.  Their `keymap' is dropped
so RET on them reaches `eca-chat--key-pressed-return', and they
show their thumbnail per `eca-chat-image-show-thumbnails'."
  (with-silent-modifications
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\(?:^\\|[^[:alnum:]]\\)@\\([^[:space:]]+\\)"
                                end t)
        (let* ((mention-start (1- (match-beginning 1)))
               ;; Punctuation right after a mention is not part of it.
               (token (replace-regexp-in-string
                       "[]),.;:!?'\"]+\\'" "" (match-string-no-properties 1)))
               (mention-end (+ mention-start 1 (length token))))
          (when-let* ((path (eca-chat--mention-image-path token roots)))
            (remove-text-properties mention-start mention-end '(keymap nil))
            (add-text-properties mention-start mention-end
                                 (list 'eca-chat-image-path path
                                       'help-echo "RET: toggle the thumbnail"))
            (eca-chat--auto-show-image-thumbnails mention-start mention-end)))))))

(provide 'eca-chat-image)
;;; eca-chat-image.el ends here
