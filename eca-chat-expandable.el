;;; eca-chat-expandable.el --- ECA chat expandable block UI -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Expandable/collapsible block UI component for ECA chat.
;;  Handles rendering, toggling, nesting, and segment management
;;  of expandable blocks (tool calls, reasoning blocks, etc.).
;;
;;; Code:

(require 'cl-lib)
(require 'color)
(require 'eca-util)

;; Forward declarations for eca-chat.el core
(defvar eca-chat-mode-map)
(defvar eca-chat--task-block-id)
(defvar eca-chat--tool-call-table-key-face)
(defvar eca-chat--tool-call-argument-key-face)
(defvar eca-chat--tool-call-argument-value-face)
(declare-function eca-chat--insert "eca-chat")
(declare-function eca-chat--content-insertion-point "eca-chat")
(declare-function eca-chat--protect-non-prompt "eca-chat")

;;;; Macros

(defmacro eca-chat--allow-write (&rest body)
  "Execute BODY allowing write to buffer."
  `(let ((inhibit-read-only t))
     ,@body))

(defmacro eca-chat--with-current-buffer (buffer &rest body)
  "Eval BODY inside chat BUFFER."
  (declare (indent 1) (debug t))
  `(with-current-buffer ,buffer
     (let ((inhibit-read-only t))
       ,@body)))

(defmacro eca-chat--with-preserved-scroll (&rest body)
  "Execute BODY preserving scroll position of all windows showing this buffer.
Saves `window-start' for every window displaying the current buffer
before BODY runs, then restores it afterwards.  This prevents the
visible content from jumping when buffer text is inserted or deleted
\(e.g. when an expandable block is toggled)."
  (declare (indent 0) (debug t))
  (let ((saved (gensym "saved-wins-")))
    `(let ((,saved (mapcar (lambda (w) (cons w (window-start w)))
                           (get-buffer-window-list (current-buffer) nil t))))
       (prog1 (progn ,@body)
         (dolist (entry ,saved)
           (let ((win (car entry))
                 (start (cdr entry)))
             (when (window-live-p win)
               (set-window-start win (min start (point-max)) t))))))))

;;;; Defcustoms

(defcustom eca-chat-expandable-block-open-symbol "⏵ "
  "The string used in eca chat buffer for blocks in open mode like tool calls.
Only shown with `eca-chat-expandable-block-arrows'."
  :type 'string
  :group 'eca)

(defcustom eca-chat-expandable-block-close-symbol "⏷ "
  "The string used in eca chat buffer for blocks in close mode like tool calls.
Only shown with `eca-chat-expandable-block-arrows'."
  :type 'string
  :group 'eca)

(defcustom eca-chat-expandable-block-arrows nil
  "Whether to mark expandable blocks with arrows instead of a bar.
By default, a bar runs along the blocks with content to expand, like
tool calls and reasoning, after a chevron, see
`eca-chat-expandable-block-chevrons'.  Its color follows the state of
tool calls, see `eca-chat-tool-call-running-face' and the other tool
call faces, and the color of the label for other blocks.  When
non-nil, the label of these blocks starts with
`eca-chat-expandable-block-open-symbol' or
`eca-chat-expandable-block-close-symbol' instead, in the color of the
label.  Either way, blocks without content to expand show neither, and
user messages and flags keep their bar."
  :type 'boolean
  :group 'eca)

(defcustom eca-chat-expandable-block-chevrons t
  "Whether a chevron before their bar tells blocks expand.
It points right on collapsed blocks, see
`eca-chat-expandable-block-collapsed-chevron', and down on expanded
ones, see `eca-chat-expandable-block-expanded-chevron'.  Top-level
blocks show it in the left margin of the chat, one column wide, and
nested blocks in the indent of their parent.  Clicking it expands or
collapses the block.  Has no effect with
`eca-chat-expandable-block-arrows'.  Takes effect in new chats."
  :type 'boolean
  :group 'eca)

(defcustom eca-chat-expandable-block-collapsed-chevron "›"
  "The chevron before the bar of collapsed blocks.
It takes one column, see `eca-chat-expandable-block-chevrons'."
  :type 'string
  :group 'eca)

(defcustom eca-chat-expandable-block-expanded-chevron "⌄"
  "The chevron before the bar of expanded blocks.
It takes one column, see `eca-chat-expandable-block-chevrons'."
  :type 'string
  :group 'eca)

(defcustom eca-chat-expandable-block-tint 7
  "Percentage of the color of their bar in the background of open blocks.
Expanded blocks, like tool calls and reasoning, get a background
tinted with the color of their bar, the state of tool calls, from
their bar to the window edge, their label lines included.  0 leaves
them on the default background.  Failed tool calls take
`eca-chat-expandable-block-error-tint' instead.  Takes effect in new
chats."
  :type 'number
  :group 'eca)

(defcustom eca-chat-expandable-block-error-tint 12
  "Percentage of the error color in the background of open failed blocks.
More than `eca-chat-expandable-block-tint', so that failures stand
out.  Takes effect in new chats."
  :type 'number
  :group 'eca)

(defcustom eca-chat-expandable-block-bg-shift-1 5
  "Percentage to shift background for level-1 blocks.
That is for the blocks whose bar has no color, which the others tint
their background with, see `eca-chat-expandable-block-tint'.
Higher values make the block background more distinct from the
surrounding buffer.  The shift direction is automatic: lightens
for dark themes and darkens for light themes."
  :type 'number
  :group 'eca)

(defcustom eca-chat-expandable-block-bg-shift-2 20
  "Percentage to shift background for level-2 nested blocks.
That is for the blocks whose bar has no color, see
`eca-chat-expandable-block-bg-shift-1'.
Higher values make the nested block background more distinct.
The shift direction is automatic: lightens for dark themes and
darkens for light themes."
  :type 'number
  :group 'eca)

(defcustom eca-chat-expandable-block-fontify-max-length 20000
  "Max content length of an expandable block that gets syntax highlighting.
Content of blocks longer than this many characters, like huge tool
call arguments or outputs, is shown without markdown/code
fontification, which can freeze Emacs on very long text.
When nil, block content is always fontified."
  :type '(choice (const :tag "Always fontify" nil)
                 (integer :tag "Max characters"))
  :group 'eca)

;;;; State

(defvar-local eca-chat-expandable--id->ov (make-hash-table :test 'equal)
  "Per-buffer map from expandable block ID to its label overlay.
Maintained by `eca-chat--insert-expandable-block' (puthash) and
`eca-chat--remove-expandable-content' / `eca-chat--destroy-nested-blocks'
\(remhash).  Accessed by `eca-chat--get-expandable-content' to avoid
the previous linear `(overlays-in (point-min) (point-max))' scan,
which scaled with chat length.

Stale entries (e.g. an overlay deleted by a path that did not
update the map) are tolerated: lookups validate the hit's
`overlay-buffer' / id and fall back to the linear scan plus a
refill when the cache misses or is stale.")

(defun eca-chat-expandable--reset-id-table ()
  "Clear the expandable id→overlay map for the current buffer.
Should be called whenever overlays are wholesale removed (e.g.
`erase-buffer' + `remove-overlays' in `eca-chat--clear')."
  (clrhash eca-chat-expandable--id->ov))

;;;; Content text

(cl-defstruct (eca-chat--expandable-text
               (:constructor eca-chat--expandable-text-create
                             (&optional string
                                        &aux (joined (or string ""))
                                        (length (length joined))))
               (:copier nil))
  "Content text of an expandable block, cheap to grow chunk by chunk.
JOINED is a string and PENDING the chunks appended after it, newest
first.  Streaming events like `reasonText' append a small chunk at a
time: concatenating on each append copied the whole content every
time, making long thinking blocks quadratic (#337).  Appending only
conses the chunk now, and the chunks are joined once, when the full
string is needed by `eca-chat--expandable-text-string'.  LENGTH is
the length of the whole text, kept up to date without joining.

The text is kept raw: the block indent is only added as `line-prefix'
when inserting it in the buffer, so plain chunks join into a string
without text property intervals, which are costly to copy."
  (joined "")
  (pending nil)
  (length 0))

(defun eca-chat--expandable-text-append (text chunk)
  "Append the string CHUNK to TEXT."
  (when (and chunk (not (string-empty-p chunk)))
    (push chunk (eca-chat--expandable-text-pending text))
    (cl-incf (eca-chat--expandable-text-length text) (length chunk))))

(defun eca-chat--expandable-text-replace (text string)
  "Replace all of TEXT with STRING."
  (setf (eca-chat--expandable-text-joined text) (or string "")
        (eca-chat--expandable-text-pending text) nil
        (eca-chat--expandable-text-length text) (length string)))

(defun eca-chat--expandable-text-string (text)
  "Return the full string of TEXT, joining its pending chunks once."
  (when-let* ((pending (eca-chat--expandable-text-pending text)))
    (setf (eca-chat--expandable-text-joined text)
          (apply #'concat (eca-chat--expandable-text-joined text)
                 (nreverse pending))
          (eca-chat--expandable-text-pending text) nil))
  (eca-chat--expandable-text-joined text))

(defun eca-chat--expandable-text-empty-p (text)
  "Return non-nil when TEXT is empty."
  (and (null (eca-chat--expandable-text-pending text))
       (string-empty-p (eca-chat--expandable-text-joined text))))

(defun eca-chat--expandable-content-empty-p (ov-content)
  "Return non-nil when the content overlay OV-CONTENT is empty."
  (let ((text (overlay-get ov-content 'eca-chat--expandable-content-content)))
    (or (null text) (eca-chat--expandable-text-empty-p text))))

(defun eca-chat--expandable-content-length (id)
  "Return the content length of the rendered expandable block ID, or nil."
  (when-let* ((ov-label (eca-chat--get-expandable-content id))
              (ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content))
              (text (overlay-get ov-content 'eca-chat--expandable-content-content)))
    (eca-chat--expandable-text-length text)))

(defun eca-chat--expandable-closing-line (ov-content)
  "Return the newline closing the content of OV-CONTENT when opened.
It carries the bar of the block down to the empty line it ends with,
unless `eca-chat-expandable-block-arrows'."
  (if eca-chat-expandable-block-arrows
      "\n"
    (eca-chat--expandable-content-indented ov-content "\n")))

(defun eca-chat--expandable-content-indented (ov-content string &optional total-length)
  "Return STRING with the content indent of OV-CONTENT as `line-prefix'.
Wrapped lines get it as `wrap-prefix' too, so they keep the bar or the
indent of the block.  STRING is tagged with `eca-no-fontify' when
TOTAL-LENGTH, the length of the whole block content (defaults to
STRING's), exceeds `eca-chat-expandable-block-fontify-max-length'."
  (let ((indent (overlay-get ov-content 'eca-chat--expandable-content-indent)))
    (if (and eca-chat-expandable-block-fontify-max-length
             (> (or total-length (length string))
                eca-chat-expandable-block-fontify-max-length))
        (propertize string 'line-prefix indent 'wrap-prefix indent 'eca-no-fontify t)
      (propertize string 'line-prefix indent 'wrap-prefix indent))))

;;;; Faces

(defface eca-chat-expandable-block-1-face
  '((t :extend t))
  "Face for the background of top-level expanded blocks.
That is of the blocks whose bar has no color, the others tint their
background with it, see `eca-chat-expandable-block-tint'.
Background is computed dynamically from the current theme by
`eca-chat--update-expandable-block-faces'."
  :group 'eca)

(defface eca-chat-expandable-block-2-face
  '((t :extend t))
  "Face for the background of nested expanded blocks (level 2).
That is of the blocks whose bar has no color, see
`eca-chat-expandable-block-1-face'.
Background is computed dynamically from the current theme by
`eca-chat--update-expandable-block-faces'."
  :group 'eca)

(defface eca-chat-expandable-block-chevron-face
  '((t :inherit shadow))
  "Face for the chevrons before the bars of blocks.
See `eca-chat-expandable-block-chevrons'."
  :group 'eca)

(defface eca-chat--expandable-bar-fill-face
  '((((type graphic)) :inverse-video t)
    (t nil))
  "Face filling the bar along blocks with its color in graphic frames.
The bar is a space there, see `eca-chat--expandable-bar-display', whose
background inverse video paints with the foreground of the face of the
bar.  Terminals show the glyph of the bar in that foreground instead."
  :group 'eca)

;;;; Functions

(defvar eca-chat--expandable-tint-faces nil
  "Alist of ((SOURCE . NESTED?) . FACE), the backgrounds of open blocks.
SOURCE is the face whose color tints FACE, the one of the bar of the
blocks, NESTED? non-nil for blocks nested in another one.  See
`eca-chat--expandable-tint-face'.")

(defvar eca-chat--expandable-tint-percents
  '((eca-chat-tool-call-error-face . eca-chat-expandable-block-error-tint))
  "Alist of (SOURCE . VARIABLE): how much of the color of SOURCE tints.
VARIABLE holds the percentage, see `eca-chat--expandable-tint-face'.
Other faces tint as much as `eca-chat-expandable-block-tint' tells.")

(defun eca-chat--expandable-set-tint (face)
  "Set the background of FACE, from `eca-chat--expandable-tint-face'.
That is from the colors of the current theme."
  (let* ((source (get face 'eca-chat-expandable-tint))
         (var (or (alist-get source eca-chat--expandable-tint-percents)
                  'eca-chat-expandable-block-tint))
         (percent (if (boundp var) (symbol-value var) 0))
         (color (and (numberp percent) (> percent 0)
                     (eca--tint-color source (/ (min percent 100) 100.0)))))
    (set-face-attribute face nil :background (or color 'unspecified) :extend t)))

(defun eca-chat--expandable-tint-face (source &optional nested?)
  "Return the face tinting backgrounds with the color of face SOURCE.
Open blocks get it, SOURCE being the face of their bar, see
`eca-chat-expandable-block-tint', and so do user messages.  Nested in
another block, NESTED?, a block gets a face of its own, of the same
color: painting one of them leaves the background of the other alone.
Made on first use, then follows theme switches, see
`eca-chat--update-expandable-block-faces'."
  (let ((key (cons source (and nested? t))))
    (or (cdr (assoc key eca-chat--expandable-tint-faces))
        (let ((face (intern (format "eca-chat--%stint-%s"
                                    (if nested? "nested-" "") source))))
          (make-face face)
          (set-face-documentation
           face (format "Background of open blocks, tinted by `%s'." source))
          (put face 'eca-chat-expandable-tint source)
          (eca-chat--expandable-set-tint face)
          (push (cons key face) eca-chat--expandable-tint-faces)
          face))))

(defun eca-chat--update-expandable-block-faces ()
  "Recompute expandable-block background faces from the current theme.
Shift percentages are controlled by `eca-chat-expandable-block-bg-shift-1'
and `eca-chat-expandable-block-bg-shift-2'.  Lightens for dark themes,
darkens for light themes.  So are the tints of
`eca-chat--expandable-tint-face'."
  (when-let* ((bg (eca-safe-face-background 'default nil t)))
    (let* ((dark? (eq 'dark (frame-parameter nil 'background-mode)))
           (fn (if dark? #'color-lighten-name #'color-darken-name))
           (bg1 (funcall fn bg eca-chat-expandable-block-bg-shift-1))
           (bg2 (funcall fn bg eca-chat-expandable-block-bg-shift-2)))
      (set-face-attribute 'eca-chat-expandable-block-1-face nil :background bg1)
      (set-face-attribute 'eca-chat-expandable-block-2-face nil :background bg2)))
  (dolist (entry eca-chat--expandable-tint-faces)
    (eca-chat--expandable-set-tint (cdr entry))))

(defun eca-chat--expandable-content-at-point ()
  "Return expandable content overlay at point, or nil if none."
  (-first (-lambda (ov) (overlay-get ov 'eca-chat--expandable-content-id))
          (overlays-in (line-beginning-position) (point))))

(defun eca-chat--expandable-content-at-point-dwim ()
  "Return the most specific expandable block overlay for point.

Prefers a block label on the current line; otherwise returns the
innermost block whose content region contains point."
  (let* ((pos (point))
         (label-candidates (delete-dups
                            (-filter (-lambda (ov)
                                       (overlay-get ov 'eca-chat--expandable-content-id))
                                     (append (overlays-at pos)
                                             (overlays-in (line-beginning-position) pos))))))
    (or
     ;; If point is on a block label line, toggle that block.
     (car (sort label-candidates
                (lambda (a b)
                  (> (overlay-start a) (overlay-start b)))))
     ;; Otherwise, toggle the innermost block whose content contains point.
     (let ((best-ov nil)
           (best-span nil)
           (best-start nil))
       (dolist (ov (overlays-in (point-min) (point-max)))
         (when-let* ((id (overlay-get ov 'eca-chat--expandable-content-id))
                     (_ id)
                     (ov-content (overlay-get ov 'eca-chat--expandable-content-ov-content))
                     (start (overlay-start ov-content))
                     (end (overlay-end ov-content))
                     (_ (and start end (<= start pos) (< pos end))))
           (let ((span (- end start)))
             (when (or (null best-ov)
                       (< span best-span)
                       (and (= span best-span)
                            (> start best-start)))
               (setq best-ov ov
                     best-span span
                     best-start start)))))
       best-ov))))

(defun eca-chat--get-expandable-content (id)
  "Return the overlay if there is a expandable content for ID.
Uses `eca-chat-expandable--id->ov' for an O(1) hit; on miss or
stale entry, falls back to the historical linear scan and
refills the cache."
  (or (let ((ov (gethash id eca-chat-expandable--id->ov)))
        (when (and (overlayp ov)
                   (overlay-buffer ov)
                   (eq (overlay-buffer ov) (current-buffer))
                   (equal id (overlay-get ov 'eca-chat--expandable-content-id)))
          ov))
      (when-let* ((ov (-first (-lambda (ov)
                                (string= id (overlay-get ov 'eca-chat--expandable-content-id)))
                              (overlays-in (point-min) (point-max)))))
        (puthash id ov eca-chat-expandable--id->ov)
        ov)))

(defun eca-chat--propertize-only-first-word (str &rest properties)
  "Return a new string propertizing PROPERTIES to the first word of STR.
If STR is empty or PROPERTIES is nil, return STR unchanged.  Existing
text properties on STR are preserved; only the first word gets the
additional PROPERTIES.  The first word is the substring up to the first
space, tab, or newline."
  (if (or (string-empty-p str) (null properties))
      str
    (let* ((split-pos (or (string-match "[ \t\n]" str)
                          (length str)))
           (first (substring str 0 split-pos))
           (rest (substring str split-pos)))
      ;; Preserve existing properties on `first` (copied by `substring`)
      ;; and add/override with the provided PROPERTIES only for the first word.
      (add-text-properties 0 (length first) properties first)
      (concat first rest))))

(defun eca-chat--propertize-unset (string &rest properties)
  "Return a copy of STRING with PROPERTIES on its text that has none.
For each property and value of PROPERTIES, the parts of STRING that
already have the property keep it, like the buttons of a label keep
their `keymap' and `help-echo'."
  (let ((string (copy-sequence string)))
    (cl-loop for (prop value) on properties by #'cddr
             do (let ((pos 0))
                  (while (< pos (length string))
                    (let ((next (or (next-single-property-change pos prop string)
                                    (length string))))
                      (unless (get-text-property pos prop string)
                        (put-text-property pos next prop value string))
                      (setq pos next)))))
    string))

(defconst eca-chat--expandable-content-base-indent (make-string 3 ?\s))

(defconst eca-chat--expandable-bar-symbol "▍"
  "The bar drawn along expandable blocks, user messages and flags.
Terminals show this glyph.  Graphic frames fill the part of its column
it covers instead, see `eca-chat--expandable-bar-display'.")

(defconst eca-chat--expandable-bar-display
  '(when (display-graphic-p) space :width (0.375 . width))
  "Display of `eca-chat--expandable-bar-symbol' in graphic frames.
A space as wide as the part of its column the glyph covers, three
eighths, filled with the color of the bar by
`eca-chat--expandable-bar-fill-face'.
Fonts draw the glyph only as high as their text, leaving gaps between
the bars of consecutive lines whenever a line is taller, e.g. with
`line-spacing', buttons on the line or a fallback font drawing the
glyph.  The background of a space covers the whole height of its line,
so the bars of the lines of a block join into one.
Its width is in columns of the font of its face, which follows
`text-scale-adjust', not relative to the glyph: when that font lacks
a glyph that is not ASCII, Emacs 28 to 31 look up the font drawing a
`:relative-width' at the buffer position in the string holding the
glyph, the prefix, which signals `args-out-of-range' in redisplay and
freezes the chat.  Before Emacs 30, that column is the widest glyph of
the font, as wide in monospace fonts.")

(defconst eca-chat--expandable-bar-gap-display
  '(when (display-graphic-p) space :relative-width 1.625)
  "Display of the space after the bar in graphic frames.
It also takes the rest of the column of the bar, which the glyph of the
bar leaves empty, so the bar and this space stay two columns wide.")

(defconst eca-chat--expandable-chevron-margin-width 1
  "Columns of the left margin of chats, where top-level chevrons show.
The fringe goes between the margin and the text, which keeps the
chevrons apart from the bars, see `eca-chat--expandable-chevron-gap'.
See `eca-chat-expandable-block-chevrons'.")

(defconst eca-chat--expandable-chevron-gap 0.375
  "Columns between the chevrons of blocks and their bar, as wide as it.
The left fringe of chats takes them after the chevrons of top-level
blocks, see `eca-chat--expandable-chevron-fringe-width', and a space
after those of nested blocks in graphic frames, see
`eca-chat--expandable-chevron-space-display'.")

(defconst eca-chat--expandable-chevron-gap-display
  ;; The columns of the prefix of the parent, less its bar, the chevron
  ;; and the gap after it.
  `((when (display-graphic-p) space
          :relative-width ,(- (length eca-chat--expandable-content-base-indent)
                              0.375 1 eca-chat--expandable-chevron-gap))
    (when (not (display-graphic-p)) space :width 0))
  "Display of the gap before the chevron of a nested block.
The chevron and the gap after it take the end of the gap after the bar
of the parent and the space after it: in graphic frames, this gap is
the rest, so the bar of the nested block stays where it is on its
other lines.  Terminals draw the bar of the parent as wide as a
column, and a space after the chevron, so the gap takes none there.")

(defconst eca-chat--expandable-chevron-space-display
  `(when (display-graphic-p) space :relative-width ,eca-chat--expandable-chevron-gap)
  "Display of the space after the chevron of a nested block.
In graphic frames it is the gap after the chevrons, see
`eca-chat--expandable-chevron-gap'.  Terminals show a space.")

(defun eca-chat--expandable-chevron-fringe-width (char-width)
  "Return the width in pixels of the left fringe of chats.
That is `eca-chat--expandable-chevron-gap' of CHAR-WIDTH, the width of
a column in pixels: none on terminals, whose columns are one wide."
  (round (* eca-chat--expandable-chevron-gap char-width)))

(defun eca-chat--expandable-bar-face (label)
  "Return the face of the bar of a block labeled LABEL.
That is the `eca-chat-expandable-bar-face' of LABEL, which tool calls
give their state symbol, else the face of LABEL."
  (when (and (stringp label) (not (string-empty-p label)))
    (if-let* ((pos (text-property-not-all 0 (length label)
                                          'eca-chat-expandable-bar-face nil label)))
        (get-text-property pos 'eca-chat-expandable-bar-face label)
      (get-text-property 0 'font-lock-face label))))

(defun eca-chat--expandable-bar-faces (face)
  "Return the faces of the bar drawn in FACE.
`eca-chat--expandable-bar-fill-face' comes first, so that it fills the
bar with the foreground of FACE in graphic frames whatever FACE sets."
  (cons 'eca-chat--expandable-bar-fill-face (and face (list face))))

(defun eca-chat--expandable-bar (face)
  "Return the bar drawn in FACE along a block, and the space after it.
They take two columns, at the start of the prefix of the lines of the
block.  Graphic frames draw the bar as high as the line, see
`eca-chat--expandable-bar-display'."
  (concat (propertize eca-chat--expandable-bar-symbol
                      'face (eca-chat--expandable-bar-faces face)
                      'display eca-chat--expandable-bar-display)
          (propertize " " 'display eca-chat--expandable-bar-gap-display)))

(defun eca-chat--expandable-recolor-bar (face old-face new-face)
  "Return FACE, the face of a bar drawn in OLD-FACE, drawn in NEW-FACE.
FACE is a face or a list of faces, like the background of the block
painted on the bar, which stays.  Return FACE when it is not the face
of a bar, see `eca-chat--expandable-bar-faces'."
  (let ((faces (ensure-list face)))
    (if (memq 'eca-chat--expandable-bar-fill-face faces)
        (append (remq 'eca-chat--expandable-bar-fill-face (remq old-face faces))
                (eca-chat--expandable-bar-faces new-face))
      face)))

(defun eca-chat--expandable-line-prefix (bar-face)
  "Return the prefix of the content lines of a block, bar in BAR-FACE.
It is as wide as `eca-chat--expandable-content-base-indent', starting
with the bar unless `eca-chat-expandable-block-arrows'."
  (if eca-chat-expandable-block-arrows
      eca-chat--expandable-content-base-indent
    (let ((bar (eca-chat--expandable-bar bar-face)))
      (concat bar
              (make-string (max 0 (- (length eca-chat--expandable-content-base-indent)
                                     (string-width bar)))
                           ?\s)))))

(defun eca-chat--expandable-parent-prefix (parent-id)
  "Return what the lines of the block PARENT-ID start with, or nil.
The lines of blocks nested in it start with it too, its bar first."
  (when parent-id
    (eca-chat--expandable-line-prefix
     (-some-> (eca-chat--get-expandable-content parent-id)
       (overlay-get 'eca-chat--expandable-content-bar-face)))))

(defun eca-chat--expandable-chevron (open?)
  "Return the chevron of a block, expanded when OPEN?, one column wide.
See `eca-chat-expandable-block-chevrons'."
  (propertize (truncate-string-to-width
               (if open?
                   eca-chat-expandable-block-expanded-chevron
                 eca-chat-expandable-block-collapsed-chevron)
               1 nil ?\s)
              'face 'eca-chat-expandable-block-chevron-face))

(defun eca-chat--expandable-chevron-prefix (open? &optional label-prefix)
  "Return what the label of a block starts with, up to its bar.
That is its chevron, expanded when OPEN?.  A top-level block shows it
in the left margin, which the fringe keeps apart from the bar, see
`eca-chat--expandable-chevron-margin-width'.  A nested block shows it
and a gap after it, keeping it apart from the bar, in LABEL-PREFIX,
what the lines of its parent start with, in place of its last two
columns: the gap after the bar of the parent gets shorter, see
`eca-chat--expandable-chevron-gap-display'."
  (let ((chevron (eca-chat--expandable-chevron open?)))
    (if (and label-prefix (>= (length label-prefix) 2))
        (concat (substring label-prefix 0 -2)
                (propertize " " 'display eca-chat--expandable-chevron-gap-display)
                chevron
                (propertize " " 'display eca-chat--expandable-chevron-space-display))
      (concat label-prefix
              (propertize " " 'display `((margin left-margin) ,chevron))))))

(defun eca-chat--make-expandable-icons (icon-face &optional label-prefix bar-face)
  "Create open/close icons with ICON-FACE and optional LABEL-PREFIX.
Both are the bar in BAR-FACE, ICON-FACE when nil, after a chevron
pointing right on the open icon, shown while the block is collapsed,
and down on the close one, see `eca-chat-expandable-block-chevrons'.
`eca-chat-expandable-block-arrows' makes them the open and close
symbols in ICON-FACE instead.
Uses the `face' property (not `font-lock-face') because these strings
are used as `line-prefix' values, where `font-lock-face' is not rendered."
  (if eca-chat-expandable-block-arrows
      (let ((icon (lambda (symbol)
                    (concat label-prefix
                            (if icon-face (propertize symbol 'face icon-face) symbol)))))
        (cons (funcall icon eca-chat-expandable-block-open-symbol)
              (funcall icon eca-chat-expandable-block-close-symbol)))
    (let ((bar (eca-chat--expandable-bar (or bar-face icon-face))))
      (if eca-chat-expandable-block-chevrons
          (cons (concat (eca-chat--expandable-chevron-prefix nil label-prefix) bar)
                (concat (eca-chat--expandable-chevron-prefix t label-prefix) bar))
        (let ((icon (concat label-prefix bar)))
          (cons icon icon))))))

(defun eca-chat-expandable-toggle-at-margin (event)
  "Expand or collapse the block whose chevron in the margin got EVENT.
Top-level blocks show their chevron there, see
`eca-chat-expandable-block-chevrons'.  Elsewhere, set point."
  (interactive "e")
  (let* ((posn (event-start event))
         (window (posn-window posn))
         (pos (posn-point posn))
         (ov (when (and (windowp window) pos)
               (with-current-buffer (window-buffer window)
                 (save-excursion
                   (goto-char pos)
                   (eca-chat--expandable-content-at-point))))))
    (if-let* ((id (and ov (overlay-get ov 'eca-chat--expandable-content-id)))
              (ov-content (overlay-get ov 'eca-chat--expandable-content-ov-content))
              ((or (not (eca-chat--expandable-content-empty-p ov-content))
                   (eca-chat--segments-children
                    (overlay-get ov 'eca-chat--expandable-content-segments)))))
        (with-current-buffer (window-buffer window)
          (eca-chat--expandable-content-toggle id))
      (mouse-set-point event))))

(defun eca-chat--expandable-plain-prefix (&optional label-prefix)
  "Return the label prefix of a block without content to expand.
It shows no bar, as the bar tells which blocks expand, and lines up
with the text around it: that is LABEL-PREFIX, what the lines of the
parent of a nested block start with, or nothing.  With
`eca-chat-expandable-block-arrows', a nested label gets spaces in
place of the arrow instead, lining up with the labels of the blocks
around it."
  (if eca-chat-expandable-block-arrows
      (when label-prefix
        (concat label-prefix
                (make-string (length eca-chat-expandable-block-open-symbol) ?\s)))
    label-prefix))

(defun eca-chat--expandable-label-prefix (ov-label has-content? open?)
  "Return the `line-prefix' of the label line of block OV-LABEL.
That is its close or open icon, as OPEN? tells, when HAS-CONTENT?,
else its plain prefix."
  (overlay-get ov-label
               (cond ((not has-content?) 'eca-chat--expandable-content-plain-prefix)
                     (open? 'eca-chat--expandable-content-close-icon)
                     (t 'eca-chat--expandable-content-open-icon))))

(defun eca-chat--expandable-set-prefixes (ov-label label)
  "Store on block OV-LABEL its prefixes for LABEL, when they changed.
Its icons, plain prefix, bar face and content indent follow the faces
of LABEL and of its bar, see `eca-chat--expandable-bar-face'.  Return
non-nil when they changed.  Storing equal prefixes again on every
streamed chunk would count as an overlay change, making redisplay lay
out the window again for nothing."
  (let* ((icon-face (get-text-property 0 'font-lock-face label))
         (bar-face (eca-chat--expandable-bar-face label))
         (faces (list icon-face bar-face))
         (old-faces (overlay-get ov-label 'eca-chat--expandable-content-icon-face)))
    (unless (equal faces old-faces)
      (let* ((label-indent (eca-chat--expandable-parent-prefix
                            (overlay-get ov-label 'eca-chat--expandable-content-parent-id)))
             (icons (eca-chat--make-expandable-icons icon-face label-indent bar-face)))
        (overlay-put ov-label 'eca-chat--expandable-content-icon-face faces)
        (overlay-put ov-label 'eca-chat--expandable-content-bar-face bar-face)
        (overlay-put ov-label 'eca-chat--expandable-content-open-icon (car icons))
        (overlay-put ov-label 'eca-chat--expandable-content-close-icon (cdr icons))
        (overlay-put ov-label 'eca-chat--expandable-content-plain-prefix
                     (eca-chat--expandable-plain-prefix label-indent))
        (when-let* ((ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content)))
          (overlay-put ov-content 'eca-chat--expandable-content-indent
                       (concat label-indent (eca-chat--expandable-line-prefix bar-face))))
        t))))

(defun eca-chat--expandable-repaint-background (ov-label)
  "Repaint the open block OV-LABEL in the background of its bar face.
That is its content, its label lines and the start of the lines of
its nested blocks, under their own background, see
`eca-chat--expandable-block-face'."
  (when-let* (((overlay-get ov-label 'eca-chat--expandable-content-toggle))
              (ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content))
              (old (overlay-get ov-content 'face))
              (new (eca-chat--expandable-block-face
                    (overlay-get ov-label 'eca-chat--expandable-content-nested)
                    (overlay-get ov-label 'eca-chat--expandable-content-bar-face)))
              ((not (eq old new))))
    (overlay-put ov-content 'face new)
    (when-let* ((ov (overlay-get ov-label 'eca-chat--expandable-label-background))
                ((overlay-buffer ov)))
      (overlay-put ov 'face new))
    (with-silent-modifications
      (eca-chat--swap-face-in-line-prefixes
       (overlay-start ov-label) (overlay-end ov-content) old new))))

(defun eca-chat--expandable-repaint-bar (ov-label old-face)
  "Repaint in its current face the bar of the open block OV-LABEL.
Its content lines, nested blocks included, drew it in OLD-FACE.  The
bar sits at the start of their prefixes, after the prefix of the
parent for a nested block.  Its background follows, see
`eca-chat--expandable-repaint-background'."
  (eca-chat--expandable-repaint-background ov-label)
  (when-let* (((not eca-chat-expandable-block-arrows))
              ((overlay-get ov-label 'eca-chat--expandable-content-toggle))
              (ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content))
              (new-face (overlay-get ov-label 'eca-chat--expandable-content-bar-face))
              ((not (eq new-face old-face))))
    (let ((column (length (eca-chat--expandable-parent-prefix
                           (overlay-get ov-label 'eca-chat--expandable-content-parent-id))))
          (inhibit-read-only t))
      (dolist (prop '(line-prefix wrap-prefix))
        (let ((pos (overlay-start ov-content))
              (end (overlay-end ov-content)))
          (while (< pos end)
            (let ((next (or (next-single-property-change pos prop nil end) end))
                  (prefix (get-text-property pos prop)))
              (when (and (stringp prefix) (> (length prefix) column))
                (let* ((face (get-text-property column 'face prefix))
                       (repainted (eca-chat--expandable-recolor-bar face old-face new-face)))
                  (unless (equal repainted face)
                    (let ((new-prefix (copy-sequence prefix)))
                      (put-text-property column (1+ column) 'face repainted new-prefix)
                      (put-text-property pos next prop new-prefix)))))
              (setq pos next))))))))

(defun eca-chat--expandable-block-face (nested? &optional bar-face)
  "Return the background face of an open block, nested when NESTED?.
That is the tint of BAR-FACE, the face of its bar, see
`eca-chat--expandable-tint-face'.  Without one, the level-2 face when
NESTED? is non-nil, otherwise level-1."
  (cond (bar-face (eca-chat--expandable-tint-face bar-face nested?))
        (nested? 'eca-chat-expandable-block-2-face)
        (t 'eca-chat-expandable-block-1-face)))

(defun eca-chat--expandable-bar-column (nested?)
  "Return a function telling where the bar of a block is in a prefix.
That is its first bar, or for a NESTED? block, the second one, after
the bar of its parent: the background of the block starts there.
Prefixes without that bar, as with `eca-chat-expandable-block-arrows',
start the block after the indent of its parent."
  (lambda (prefix)
    (let ((index (string-search eca-chat--expandable-bar-symbol prefix)))
      (when (and index nested?)
        (setq index (string-search eca-chat--expandable-bar-symbol prefix (1+ index))))
      (or index
          (if nested? (length eca-chat--expandable-content-base-indent) 0)))))

(defun eca-chat--apply-face-to-line-prefixes (start end face &optional column append)
  "Add FACE to every `line-prefix' and `wrap-prefix' between START and END.
From COLUMN on, a function of a prefix returning where FACE starts in
it, or from its start.  FACE goes before the faces there, unless
APPEND puts it after them: the background of the parent of a nested
block goes under the one of the block.
The face symbol itself is stored instead of its resolved background
color, so prefixes follow theme switches (#301).  FACE only defines a
background, so existing foreground colors (e.g. icon faces) are
preserved."
  (dolist (prop '(line-prefix wrap-prefix))
    (let ((pos start))
      (while (< pos end)
        (let* ((next-change (or (next-single-property-change pos prop nil end) end))
               (prefix (get-text-property pos prop))
               (from (when (stringp prefix)
                       (or (and column (funcall column prefix)) 0))))
          (when (and from (< from (length prefix)))
            (let ((existing (get-text-property from 'face prefix)))
              ;; Skip when already painted to avoid re-consing the same
              ;; face onto frequently repainted prefixes.
              (unless (or (eq existing face)
                          (and (listp existing) (memq face existing)))
                (let ((new-prefix (copy-sequence prefix)))
                  (add-face-text-property from (length new-prefix) face append new-prefix)
                  (put-text-property pos next-change prop new-prefix)))))
          (setq pos next-change))))))

(defun eca-chat--swap-face-in-line-prefixes (start end old new)
  "Replace face OLD by NEW in the prefixes between START and END.
That is in their `line-prefix' and `wrap-prefix'.  A nil NEW removes
OLD instead."
  (dolist (prop '(line-prefix wrap-prefix))
    (let ((pos start))
      (while (< pos end)
        (let ((next (or (next-single-property-change pos prop nil end) end))
              (prefix (get-text-property pos prop))
              (new-prefix nil))
          (when (stringp prefix)
            (dotimes (i (length prefix))
              (let ((faces (ensure-list (get-text-property i 'face prefix))))
                (when (memq old faces)
                  (let ((swapped (delq nil (mapcar (lambda (f) (if (eq f old) new f))
                                                   faces))))
                    (unless new-prefix
                      (setq new-prefix (copy-sequence prefix)))
                    (put-text-property i (1+ i) 'face
                                       (if (cdr swapped) swapped (car swapped))
                                       new-prefix)))))
            (when new-prefix
              (put-text-property pos next prop new-prefix)))
          (setq pos next))))))

(defun eca-chat--overlay-buttons (start end)
  "Draw the buttons between START and END with overlays of their own.
An open block paints its background with an overlay face, which wins
over the text faces of the pills of `eca-buttonize', leaving only
their edge: an overlay of a higher priority keeps their background,
with the fill of their pill, see `eca--button-fill-face'.
It goes away with the text of its button."
  (let ((pos start))
    (while (and (< pos end)
                (setq pos (text-property-not-all pos end 'eca-button-pill nil)))
      (let ((next (or (next-single-property-change pos 'eca-button-pill nil end) end)))
        (unless (-some (lambda (ov) (overlay-get ov 'eca-button-pill)) (overlays-at pos))
          (let ((ov (make-overlay pos next nil t nil))
                (pill (get-text-property pos 'eca-button-pill)))
            (overlay-put ov 'eca-button-pill t)
            (overlay-put ov 'face (or (eca--button-fill-face pill) pill))
            (overlay-put ov 'priority 10)
            (overlay-put ov 'evaporate t)))
        (setq pos next)))))

(defun eca-chat--expandable-parent-face (ov-label)
  "Return the background of the open parent of block OV-LABEL, or nil."
  (when-let* ((parent-id (overlay-get ov-label 'eca-chat--expandable-content-parent-id))
              (parent-ov (eca-chat--get-expandable-content parent-id))
              (parent-content-ov (overlay-get parent-ov 'eca-chat--expandable-content-ov-content)))
    (overlay-get parent-content-ov 'face)))

(defun eca-chat--paint-nested-label (ov-label)
  "Paint OV-LABEL's `line-prefix` with the parent block's background face.
So are the other lines of the label, like the approval lines of a tool
call, and the empty line closing the block, which carries the bar of
the parent.  It goes under the background of the block, open, from
its bar on.  Does nothing for top-level blocks or when parent has no
face set."
  (when (overlay-get ov-label 'eca-chat--expandable-content-nested)
    (when-let* ((parent-face (eca-chat--expandable-parent-face ov-label))
                (ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content)))
      (eca-chat--apply-face-to-line-prefixes
       (overlay-start ov-label) (overlay-start ov-content) parent-face nil t)
      ;; Its buttons, like approvals, sit on the background of the parent.
      (eca-chat--overlay-buttons (overlay-start ov-label) (overlay-start ov-content))
      (let ((end (overlay-end ov-content)))
        (when (< end (point-max))
          (eca-chat--apply-face-to-line-prefixes end (1+ end) parent-face nil t))))))

(defun eca-chat--expandable-paint-content (ov-label start end)
  "Paint the content of the open block OV-LABEL between START and END.
That is its background from its bar on, see
`eca-chat--expandable-block-face', and under it the background of its
parent when nested, which the start of its lines draws.  Its buttons
keep their own, see `eca-chat--overlay-buttons'."
  (when-let* ((ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content))
              (face (overlay-get ov-content 'face)))
    (let ((nested? (overlay-get ov-label 'eca-chat--expandable-content-nested)))
      ;; Only text properties change, which the prompt can't undo.
      (with-silent-modifications
        (eca-chat--apply-face-to-line-prefixes
         start end face (eca-chat--expandable-bar-column nested?))
        (when-let* ((parent-face (and nested? (eca-chat--expandable-parent-face ov-label))))
          (eca-chat--apply-face-to-line-prefixes start end parent-face nil t)))
      (eca-chat--overlay-buttons start end))))

(defun eca-chat--expandable-paint-label (ov-label)
  "Paint the label lines of block OV-LABEL with the backgrounds of blocks.
They get the one of the parent of a nested block, see
`eca-chat--paint-nested-label', and while the block is open its own
one, like its content: on their text and from its bar on in their
prefix.  An overlay paints their text, which streaming blocks insert
anew: call this after inserting their label."
  ;; Only text properties change, which the prompt can't undo.
  (with-silent-modifications
    (eca-chat--paint-nested-label ov-label))
  (when-let* ((ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content))
              (start (overlay-start ov-label))
              (end (overlay-start ov-content)))
    (let ((face (and (overlay-get ov-label 'eca-chat--expandable-content-toggle)
                     (overlay-get ov-content 'face)))
          (ov (overlay-get ov-label 'eca-chat--expandable-label-background)))
      (cond
       (face
        (if (and (overlayp ov) (overlay-buffer ov))
            (move-overlay ov start end)
          ;; Text inserted before the label stays out of it, like an
          ;; older history page, but not the label itself: inserted
          ;; anew at its start, this moves it back over the label.
          (setq ov (make-overlay start end nil t nil))
          (overlay-put ov 'evaporate t)
          (overlay-put ov-label 'eca-chat--expandable-label-background ov))
        (overlay-put ov 'face face)
        ;; Over the background of the parent, which the content of the
        ;; parent paints.
        (overlay-put ov 'priority (overlay-get ov-content 'priority))
        (with-silent-modifications
          (eca-chat--apply-face-to-line-prefixes
           start end face (eca-chat--expandable-bar-column
                           (overlay-get ov-label 'eca-chat--expandable-content-nested))))
        (eca-chat--overlay-buttons start end))
       ((overlayp ov)
        ;; Closed: the prefix of its first line is the one of a closed
        ;; block again, the others, like approval lines, keep theirs.
        (when-let* ((painted (overlay-get ov 'face)))
          (with-silent-modifications
            (eca-chat--swap-face-in-line-prefixes start end painted nil)))
        (delete-overlay ov)
        (overlay-put ov-label 'eca-chat--expandable-label-background nil))))))

(defun eca-chat--insert-expandable-block (id label content open-icon close-icon content-indent &optional nested-props)
  "Insert an expandable block with ID, LABEL, CONTENT, icons and indent.
CONTENT is a string or an `eca-chat--expandable-text', which the block
then shares: a nested block shares it with its spec in the parent.
OPEN-ICON and CLOSE-ICON are the toggle icons.
CONTENT-INDENT is the `line-prefix` for content.
NESTED-PROPS is a plist with :parent-id and :label-indent for nested blocks."
  (let ((ov-label (make-overlay (point) (point) (current-buffer)))
        (label-indent (plist-get nested-props :label-indent))
        (text (if (eca-chat--expandable-text-p content)
                  content
                (eca-chat--expandable-text-create content))))
    (overlay-put ov-label 'eca-chat--expandable-content-id id)
    (puthash id ov-label eca-chat-expandable--id->ov)
    (overlay-put ov-label 'eca-chat--expandable-content-open-icon open-icon)
    (overlay-put ov-label 'eca-chat--expandable-content-close-icon close-icon)
    (overlay-put ov-label 'eca-chat--expandable-content-toggle nil)
    (when nested-props
      (overlay-put ov-label 'eca-chat--expandable-content-nested t)
      (overlay-put ov-label 'eca-chat--expandable-content-parent-id (plist-get nested-props :parent-id)))
    (unless nested-props
      (overlay-put ov-label 'eca-chat--expandable-content-segments nil))
    ;; The callers made the icons and content indent for these faces.
    (let ((bar-face (eca-chat--expandable-bar-face label)))
      (overlay-put ov-label 'eca-chat--expandable-content-bar-face bar-face)
      (overlay-put ov-label 'eca-chat--expandable-content-icon-face
                   (list (get-text-property 0 'font-lock-face label) bar-face))
      (overlay-put ov-label 'eca-chat--expandable-content-plain-prefix
                   (eca-chat--expandable-plain-prefix label-indent)))
    ;; Buttons of the label, like approvals, keep their own keymap.
    (eca-chat--insert (eca-chat--propertize-unset
                       (eca-chat--propertize-only-first-word label
                                                             'line-prefix (eca-chat--expandable-label-prefix
                                                                           ov-label
                                                                           (not (eca-chat--expandable-text-empty-p text))
                                                                           nil))
                                  'keymap (let ((km (make-sparse-keymap)))
                                            (define-key km (kbd "<mouse-1>") (lambda () (interactive) (eca-chat--expandable-content-toggle id)))
                                            (define-key km (kbd "TAB") (lambda () (interactive) (eca-chat--expandable-content-toggle id)))
                                            (define-key km (kbd "RET") (lambda () (interactive) (eca-chat--expandable-content-toggle id)))
                                            km)
                                  'help-echo "mouse-1 / tab / RET: expand/collapse"))
    (eca-chat--insert "\n")
    (let* ((start-point (point))
           ;; The empty line closing a nested block carries the bar
           ;; of the parent, which goes on past it.
           (_ (eca-chat--insert (if (and label-indent
                                         (not eca-chat-expandable-block-arrows))
                                    (propertize "\n" 'line-prefix label-indent
                                                'wrap-prefix label-indent)
                                  "\n")))
           (ov-content (make-overlay start-point start-point (current-buffer) nil t))
           (nested? (plist-get nested-props :parent-id)))
      (overlay-put ov-content 'eca-chat--expandable-content-content text)
      (overlay-put ov-content 'eca-chat--expandable-content-indent content-indent)
      (overlay-put ov-content 'eca-chat--expandable-block-nested nested?)
      (overlay-put ov-content 'priority (if nested? 1 0))
      (overlay-put ov-label 'eca-chat--expandable-content-ov-content ov-content))))

(defun eca-chat--render-nested-block (parent-ov child-spec)
  "Render a nested block CHILD-SPEC within PARENT-OV's content area."
  (-let* (((&plist :id id :label label :content content :icon-face icon-face) child-spec)
          (parent-id (overlay-get parent-ov 'eca-chat--expandable-content-id))
          (parent-content-ov (overlay-get parent-ov 'eca-chat--expandable-content-ov-content))
          ;; The lines of the child start with the bar of the parent.
          (label-indent (eca-chat--expandable-parent-prefix parent-id))
          (bar-face (eca-chat--expandable-bar-face label))
          (icons (eca-chat--make-expandable-icons icon-face label-indent bar-face)))
    (save-excursion
      (goto-char (overlay-end parent-content-ov))
      (unless (bolp) (eca-chat--insert "\n"))
      (eca-chat--insert-expandable-block id label content
                                         (car icons) (cdr icons)
                                         (concat label-indent
                                                 (eca-chat--expandable-line-prefix bar-face))
                                         (list :parent-id parent-id
                                               :label-indent label-indent))
      ;; Paint label's line-prefix with parent's background when parent is open
      (when-let* ((child-ov (eca-chat--get-expandable-content id)))
        (eca-chat--expandable-paint-label child-ov)))))

(defun eca-chat--destroy-nested-blocks (parent-id)
  "Remove all nested block overlays that belong to PARENT-ID.
The rendered children are found from the child ids in the parent
segments, through `eca-chat-expandable--id->ov', instead of scanning
all the buffer overlays, which grow with the chat.  Their own nested
blocks are removed too."
  (when-let* ((parent-ov (eca-chat--get-expandable-content parent-id)))
    (dolist (spec (eca-chat--segments-children
                   (overlay-get parent-ov 'eca-chat--expandable-content-segments)))
      (let* ((id (plist-get spec :id))
             (ov (gethash id eca-chat-expandable--id->ov)))
        ;; Children of a collapsed parent are not rendered, so they
        ;; have no overlay to remove.
        (when (and (overlayp ov)
                   (eq (overlay-buffer ov) (current-buffer))
                   (equal parent-id
                          (overlay-get ov 'eca-chat--expandable-content-parent-id)))
          (eca-chat--destroy-nested-blocks id)
          (when-let* ((content-ov (overlay-get ov 'eca-chat--expandable-content-ov-content)))
            (delete-overlay content-ov))
          (remhash id eca-chat-expandable--id->ov)
          (delete-overlay ov))))))

(defun eca-chat--segments-total-text (segments)
  "Return the concatenation of all text segment contents in SEGMENTS."
  (mapconcat (lambda (seg)
               (if (eq 'text (plist-get seg :type))
                   (plist-get seg :content)
                 ""))
             segments
             ""))

(defun eca-chat--segments-children (segments)
  "Return all child specs from SEGMENTS."
  (-filter (lambda (seg) (eq 'child (plist-get seg :type))) segments))

(defun eca-chat--add-expandable-content (id label content &optional parent-id at-point)
  "Add LABEL to the chat current position for ID as a interactive text.
When expanded, shows CONTENT.
Applies ICON-FACE to open/close icons.
If PARENT-ID is provided, adds as a nested block under that parent.
If AT-POINT is provided, inserts at that buffer position instead of
the default content insertion point."
  (if parent-id
      (when-let* ((parent-ov (eca-chat--get-expandable-content parent-id)))
        (let* ((segments (overlay-get parent-ov 'eca-chat--expandable-content-segments))
               (existing-spec (-first (lambda (s) (and (eq 'child (plist-get s :type))
                                                       (string= id (plist-get s :id))))
                                      segments)))
          (if existing-spec
              ;; Child spec already exists (e.g. parent was collapsed destroying
              ;; the overlay but keeping the spec); delegate to update to avoid
              ;; duplicating the entry.
              (eca-chat--update-expandable-content id label content nil parent-id)
            (let* ((icon-face (get-text-property 0 'font-lock-face label))
                   (child-spec (list :type 'child :id id :label label
                                     :content (eca-chat--expandable-text-create content)
                                     :icon-face icon-face))
                   ;; Capture any unsegmented text that was appended to content
                   ;; since the last segment, and add it as a text segment first
                   (ov-content (overlay-get parent-ov 'eca-chat--expandable-content-ov-content))
                   (full-content (eca-chat--expandable-text-string
                                  (overlay-get ov-content 'eca-chat--expandable-content-content)))
                   (segmented-text (eca-chat--segments-total-text segments))
                   (unsegmented-text (when (> (length full-content) (length segmented-text))
                                       (substring full-content (length segmented-text))))
                   (new-segments (append segments
                                         (when unsegmented-text
                                           (list (list :type 'text :content unsegmented-text)))
                                         (list child-spec))))
              (overlay-put parent-ov 'eca-chat--expandable-content-segments new-segments)
              (when (overlay-get parent-ov 'eca-chat--expandable-content-toggle)
                (eca-chat--render-nested-block parent-ov child-spec))))))
    (save-excursion
      (let* ((start-point (or at-point (eca-chat--content-insertion-point)))
             (icon-face (get-text-property 0 'font-lock-face label))
             (bar-face (eca-chat--expandable-bar-face label))
             (icons (eca-chat--make-expandable-icons icon-face nil bar-face)))
        (goto-char start-point)
        (unless (bolp) (eca-chat--insert "\n"))
        (eca-chat--insert-expandable-block id label content
                                           (car icons) (cdr icons)
                                           (eca-chat--expandable-line-prefix bar-face))))))

(defun eca-chat--remove-expandable-content (id)
  "Remove the expandable block with ID from the buffer.
Deletes both the label overlay and its content overlay, along with
any text they covered, including surrounding newlines added during insertion."
  (when-let* ((ov-label (eca-chat--get-expandable-content id)))
    (let* ((ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content))
           ;; Determine the full region: from label overlay start to content overlay end
           (start (overlay-start ov-label))
           (end (if ov-content
                    (overlay-end ov-content)
                  (overlay-end ov-label)))
           ;; Include preceding newline (inserted by add-expandable-content)
           (start (if (and (> start (point-min))
                           (eq (char-before start) ?\n))
                      (1- start)
                    start))
           ;; Include trailing newline (inserted between label/content in
           ;; insert-expandable-block)
           (end (if (and (< end (point-max))
                         (eq (char-after end) ?\n))
                    (1+ end)
                  end)))
      ;; Destroy any nested blocks first
      (eca-chat--destroy-nested-blocks id)
      ;; Delete overlays
      (when ov-content (delete-overlay ov-content))
      (delete-overlay ov-label)
      (remhash id eca-chat-expandable--id->ov)
      ;; Remove the text region
      (let ((inhibit-read-only t))
        (delete-region start end)))))

(defun eca-chat--update-expandable-content (id label content &optional append-content? parent-id)
  "Update to LABEL and CONTENT the expandable content of id ID.
If LABEL is nil, the existing label is preserved.
If APPEND-CONTENT? is non-nil, append CONTENT to existing content.
Appending is cheap, so streaming a long block chunk by chunk stays
linear, see `eca-chat--expandable-text'.
If PARENT-ID is provided and block doesn't exist yet, updates the spec
in parent."
  (if-let* ((ov-label (eca-chat--get-expandable-content id)))
      ;; Block exists (rendered), update it directly
      (let* ((ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content))
             (nested? (overlay-get ov-label 'eca-chat--expandable-content-nested))
             (text (overlay-get ov-content 'eca-chat--expandable-content-content))
             (open? (overlay-get ov-label 'eca-chat--expandable-content-toggle)))
        (if append-content?
            (eca-chat--expandable-text-append text content)
          (eca-chat--expandable-text-replace text content))
        (save-excursion
          (when label
            (let* ((old-bar-face (overlay-get ov-label 'eca-chat--expandable-content-bar-face))
                   ;; A new label face or state changes the prefixes.
                   (prefixes-changed? (eca-chat--expandable-set-prefixes ov-label label))
                   (children (eca-chat--segments-children
                              (overlay-get ov-label 'eca-chat--expandable-content-segments)))
                   (has-content? (or (not (eca-chat--expandable-text-empty-p text)) children))
                   (label-prefix (eca-chat--expandable-label-prefix ov-label has-content? open?))
                   (label-start (overlay-start ov-label))
                   (label-end (1- (overlay-start ov-content)))
                   (cur-prefix (get-text-property label-start 'line-prefix)))
              (when prefixes-changed?
                (eca-chat--expandable-repaint-bar ov-label old-bar-face))
              ;; Skip the delete/re-insert when the rendered label is
              ;; already up to date; frequent while toolCallPrepare
              ;; streams argsText updates (#268).
              (unless (and (not prefixes-changed?)
                           (string= (substring-no-properties label)
                                    (buffer-substring-no-properties label-start label-end))
                           (equal (get-text-property 0 'font-lock-face label)
                                  (get-text-property label-start 'font-lock-face))
                           (equal (and (stringp label-prefix) (substring-no-properties label-prefix))
                                  (and (stringp cur-prefix) (substring-no-properties cur-prefix))))
                (goto-char label-start)
                (delete-region (point) label-end)
                (eca-chat--insert (eca-chat--propertize-unset
                                   (eca-chat--propertize-only-first-word label
                                                                         'line-prefix label-prefix)
                                   'help-echo "mouse-1 / RET / tab: expand/collapse"))
                ;; Repaint the backgrounds of the label after replacing it
                (eca-chat--expandable-paint-label ov-label))))
          (when open?
            (let ((delta (eca-chat--expandable-content-indented
                          ov-content content
                          (eca-chat--expandable-text-length text))))
              (if append-content?
                  (let ((insert-start (overlay-end ov-content)))
                    (goto-char insert-start)
                    (eca-chat--insert delta)
                    (eca-chat--expandable-paint-content
                     ov-label insert-start (overlay-end ov-content)))
                (progn
                  (delete-region (overlay-start ov-content) (overlay-end ov-content))
                  (goto-char (overlay-start ov-content))
                  (eca-chat--insert delta)
                  (eca-chat--expandable-paint-content
                   ov-label (overlay-start ov-content) (overlay-end ov-content)))))))
        ;; When nested, sync the updated label back to parent's child spec
        ;; so that toggling the parent closed and reopened preserves it.
        ;; The spec shares this block's content text, so it is in sync.
        (when (and nested? label)
          (when-let* ((parent-id (overlay-get ov-label 'eca-chat--expandable-content-parent-id))
                      (parent-ov (eca-chat--get-expandable-content parent-id))
                      (segments (overlay-get parent-ov 'eca-chat--expandable-content-segments))
                      (spec (-first (lambda (s) (and (eq 'child (plist-get s :type))
                                                     (string= id (plist-get s :id))))
                                    segments)))
            (plist-put spec :label label))))
    ;; Block not rendered yet
    (if parent-id
        ;; Nested: update spec in parent
        (when-let* ((parent-ov (eca-chat--get-expandable-content parent-id)))
          (let* ((segments (overlay-get parent-ov 'eca-chat--expandable-content-segments))
                 (existing-spec (-first (lambda (spec) (and (eq 'child (plist-get spec :type))
                                                            (string= id (plist-get spec :id))))
                                        segments)))
            (if existing-spec
                (let ((text (plist-get existing-spec :content)))
                  (when label
                    (plist-put existing-spec :label label)
                    (plist-put existing-spec :icon-face (get-text-property 0 'font-lock-face label)))
                  (if append-content?
                      (eca-chat--expandable-text-append text content)
                    (eca-chat--expandable-text-replace text content)))
              (let ((child-spec (list :type 'child :id id :label label
                                      :content (eca-chat--expandable-text-create content)
                                      :icon-face (get-text-property 0 'font-lock-face label))))
                (overlay-put parent-ov 'eca-chat--expandable-content-segments
                             (append segments (list child-spec)))
                (when (overlay-get parent-ov 'eca-chat--expandable-content-toggle)
                  (eca-chat--render-nested-block parent-ov child-spec))))))
      ;; Top-level: create the block so toolCallRun et al. don't
      ;; silently lose content when no toolCallPrepare preceded them.
      (eca-chat--add-expandable-content id label (or content "")))))

(defun eca-chat--expandable-content-toggle (id &optional force? close?)
  "Toggle the expandable-content of ID.
If FORCE? decide to CLOSE? or not."
  (when-let* ((ov-label (eca-chat--get-expandable-content id)))
    (let* ((inhibit-read-only t)
           (ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content))
           (segments (overlay-get ov-label 'eca-chat--expandable-content-segments))
           (children (eca-chat--segments-children segments))
           (has-content? (or (not (eca-chat--expandable-content-empty-p ov-content))
                             children))
           (currently-open? (overlay-get ov-label 'eca-chat--expandable-content-toggle))
           (close? (if force?
                       close?
                     currently-open?))
           ;; Skip when force-toggling to a state we're already in
           (already-in-state? (and force?
                                   (if close? (not currently-open?) currently-open?))))
      (unless already-in-state?
        (eca-chat--with-preserved-scroll
          (save-excursion
            (goto-char (overlay-start ov-label))
            (if (or close? (not has-content?))
                (progn
                  (put-text-property (point) (line-end-position)
                                     'line-prefix (eca-chat--expandable-label-prefix
                                                   ov-label has-content? nil))
                  (goto-char (1+ (line-end-position)))
                  ;; Destroy nested blocks first (they are within content region)
                  (eca-chat--destroy-nested-blocks id)
                  (delete-region (overlay-start ov-content) (overlay-end ov-content))
                  (overlay-put ov-content 'face nil)
                  (overlay-put ov-label 'eca-chat--expandable-content-toggle nil))
              (progn
                (put-text-property (point) (line-end-position)
                                   'line-prefix (overlay-get ov-label 'eca-chat--expandable-content-close-icon))
                (goto-char (overlay-start ov-content))
                (let ((content (eca-chat--expandable-text-string
                                (overlay-get ov-content 'eca-chat--expandable-content-content))))
                  (if segments
                      ;; Segments-aware rendering: interleave text and children in order
                      (let* ((segmented-text (eca-chat--segments-total-text segments))
                             (trailing-text (when (> (length content) (length segmented-text))
                                              (substring content (length segmented-text)))))
                        (dolist (seg segments)
                          (pcase (plist-get seg :type)
                            ('text (eca-chat--insert (eca-chat--expandable-content-indented
                                                      ov-content (plist-get seg :content)
                                                      (length content))))
                            ('child
                             (eca-chat--render-nested-block ov-label seg)
                             ;; render-nested-block uses save-excursion so point stays
                             ;; before the child; advance past it to maintain order
                             (goto-char (overlay-end ov-content)))))
                        ;; Insert any trailing text not yet captured in segments
                        (when (and trailing-text (not (string-empty-p trailing-text)))
                          (eca-chat--insert (eca-chat--expandable-content-indented
                                             ov-content trailing-text (length content))))
                        (eca-chat--insert (eca-chat--expandable-closing-line ov-content)))
                    ;; Legacy path: no segments, insert content then children
                    (eca-chat--insert (eca-chat--expandable-content-indented ov-content content)
                                      (eca-chat--expandable-closing-line ov-content))
                    (dolist (child-spec children)
                      (eca-chat--render-nested-block ov-label child-spec))))
                (overlay-put ov-content 'face
                             (eca-chat--expandable-block-face
                              (overlay-get ov-content 'eca-chat--expandable-block-nested)
                              (overlay-get ov-label 'eca-chat--expandable-content-bar-face)))
                (eca-chat--expandable-paint-content
                 ov-label (overlay-start ov-content) (overlay-end ov-content))
                (overlay-put ov-label 'eca-chat--expandable-content-toggle t)))
            ;; Repaint the backgrounds of the label after the icon swap
            (eca-chat--expandable-paint-label ov-label)))
        ;; Toggling open inserts block content into the read-only history
        ;; region, so re-protect the block, which may belong to an
        ;; earlier turn outside the streamed scope.  Only the block
        ;; changed, so the rest of the chat does not need the walk.
        (eca-chat--protect-non-prompt (overlay-start ov-label)
                                      (overlay-end ov-content)))
      close?)))

(defun eca-chat--content-table (key-vals)
  "Return a string in table format for KEY-VALS."
  (-reduce-from
   (-lambda (a (k . v))
     (concat a "\n" (propertize (concat k ":") 'font-lock-face 'eca-chat--tool-call-table-key-face) " "
             (if (listp v)
                 (concat "\n"
                         (string-join (-map-indexed
                                       (lambda (i item)
                                         (if (cl-evenp i)
                                             (propertize (concat "  " (substring (symbol-name item) 1) ": ")
                                                         'font-lock-face 'eca-chat--tool-call-argument-key-face)
                                           (propertize (concat (prin1-to-string item) "\n")
                                                       'font-lock-face 'eca-chat--tool-call-argument-value-face)))
                                       v)
                                      ""))
               v)))
   ""
   key-vals))

(provide 'eca-chat-expandable)
;;; eca-chat-expandable.el ends here
