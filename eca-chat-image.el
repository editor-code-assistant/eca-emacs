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
;;  zoom, saving the original bytes to disk via a keybinding or the
;;  image overlay's own RET / mouse-2 handler, and thumbnails toggled
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
            (let* ((save-fn (lambda ()
                              (interactive)
                              (eca-chat-save-image-at-point ov)))
                   (map (make-sparse-keymap)))
              (define-key map (kbd "RET") save-fn)
              (define-key map [mouse-2] save-fn)
              (overlay-put ov 'keymap map))))))
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
