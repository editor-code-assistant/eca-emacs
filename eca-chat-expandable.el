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

(require 'color)
(require 'eca-util)

;; Forward declarations for eca-chat.el core
(defvar eca-chat-mode-map)
(defvar eca-chat--task-block-id)
(defvar eca-chat--tool-call-table-key-face)
(defvar eca-chat--tool-call-argument-key-face)
(defvar eca-chat--tool-call-argument-value-face)
(defvar-local eca-chat--expandable-content-index
    (make-hash-table :test 'equal)
  "Map expandable block IDs to label overlays.")
(defvar-local eca-chat--expandable-content-children-index
    (make-hash-table :test 'equal)
  "Map parent block IDs to rendered child block IDs.")
(defvar-local eca-chat--expandable-content-storage
    (make-hash-table :test 'equal)
  "Map expandable block IDs to stored content state.")
(declare-function eca-chat--insert "eca-chat")
(declare-function eca-chat--content-insertion-point "eca-chat")

(defun eca-chat--expandable-storage-state (id)
  "Return stored expandable state for ID."
  (gethash id eca-chat--expandable-content-storage))

(defun eca-chat--expandable-storage-put (id state)
  "Persist expandable STATE for ID."
  (puthash id state eca-chat--expandable-content-storage))

(defun eca-chat--expandable-storage-clear (id)
  "Drop any stored expandable state for ID."
  (remhash id eca-chat--expandable-content-storage))

(defun eca-chat--expandable-state-content (state)
  "Return STATE content as a single string."
  (mapconcat #'identity
             (nreverse (copy-sequence
                        (plist-get state :chunks-reversed)))
             ""))

(defun eca-chat--expandable-state-has-content-p (state)
  "Return non-nil when STATE stores visible content."
  (not (string-empty-p (eca-chat--expandable-state-content state))))

(defun eca-chat--expandable-state-set-content (state content)
  "Return STATE updated to contain CONTENT only."
  (let ((chunks (unless (string-empty-p (or content ""))
                  (list content))))
    (plist-put state :chunks-reversed chunks)))

(defun eca-chat--expandable-state-append-content (state content)
  "Return STATE with CONTENT appended efficiently."
  (if (string-empty-p (or content ""))
      state
    (plist-put state :chunks-reversed
               (cons content (plist-get state :chunks-reversed)))))

(defun eca-chat--expandable-content-state (id)
  "Return expandable content state for ID."
  (or (eca-chat--expandable-storage-state id) '(:chunks-reversed nil)))

(defun eca-chat--expandable-content-raw (id)
  "Return raw expandable content for ID."
  (eca-chat--expandable-state-content
   (eca-chat--expandable-content-state id)))

(defun eca-chat--expandable-content-set (id content)
  "Replace stored expandable content for ID with CONTENT."
  (eca-chat--expandable-storage-put
   id
   (eca-chat--expandable-state-set-content
    (eca-chat--expandable-content-state id)
    content)))

(defun eca-chat--expandable-content-append (id content)
  "Append CONTENT to the stored expandable content for ID."
  (eca-chat--expandable-storage-put
   id
   (eca-chat--expandable-state-append-content
    (eca-chat--expandable-content-state id)
    content)))

(defun eca-chat--expandable-content-render (id indent)
  "Return stored expandable content for ID with INDENT applied."
  (mapconcat (lambda (chunk)
               (propertize chunk 'line-prefix indent))
             (nreverse
              (copy-sequence
               (plist-get (eca-chat--expandable-content-state id)
                          :chunks-reversed)))
             ""))

(defun eca-chat--child-spec-content (child-spec)
  "Return CHILD-SPEC content as a single string."
  (mapconcat #'identity
             (nreverse (copy-sequence
                        (plist-get child-spec :chunks-reversed)))
             ""))

(defun eca-chat--child-spec-has-content-p (child-spec)
  "Return non-nil when CHILD-SPEC stores visible content."
  (not (string-empty-p (eca-chat--child-spec-content child-spec))))

(defun eca-chat--child-spec-set-content (child-spec content)
  "Return CHILD-SPEC updated to contain CONTENT only."
  (plist-put child-spec
             :chunks-reversed
             (unless (string-empty-p (or content ""))
               (list content))))

(defun eca-chat--child-spec-append-content (child-spec content)
  "Return CHILD-SPEC with CONTENT appended efficiently."
  (if (string-empty-p (or content ""))
      child-spec
    (plist-put child-spec
               :chunks-reversed
               (cons content (plist-get child-spec :chunks-reversed)))))

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
  "The string used in eca chat buffer for blocks in open mode like tool calls."
  :type 'string
  :group 'eca)

(defcustom eca-chat-expandable-block-close-symbol "⏷ "
  "The string used in eca chat buffer for blocks in close mode like tool calls."
  :type 'string
  :group 'eca)

(defcustom eca-chat-expandable-block-bg-shift-1 5
  "Percentage to shift background for level-1 blocks.
Higher values make the block background more distinct from the
surrounding buffer.  The shift direction is automatic: lightens
for dark themes and darkens for light themes."
  :type 'number
  :group 'eca)

(defcustom eca-chat-expandable-block-bg-shift-2 20
  "Percentage to shift background for level-2 nested blocks.
Higher values make the nested block background more distinct.
The shift direction is automatic: lightens for dark themes and
darkens for light themes."
  :type 'number
  :group 'eca)

;;;; Faces

(defface eca-chat-expandable-block-1-face
  '((t :extend t))
  "Face for the background of top-level expanded blocks.
Background is computed dynamically from the current theme by
`eca-chat--update-expandable-block-faces'."
  :group 'eca)

(defface eca-chat-expandable-block-2-face
  '((t :extend t))
  "Face for the background of nested expanded blocks (level 2).
Background is computed dynamically from the current theme by
`eca-chat--update-expandable-block-faces'."
  :group 'eca)

;;;; Functions

(defun eca-chat--index-expandable-content (ov-label)
  "Index OV-LABEL for fast expandable block lookup."
  (when-let* ((id (overlay-get ov-label 'eca-chat--expandable-content-id)))
    (puthash id ov-label eca-chat--expandable-content-index)
    (when-let* ((parent-id
                 (overlay-get ov-label
                              'eca-chat--expandable-content-parent-id)))
      (let ((children (gethash parent-id
                               eca-chat--expandable-content-children-index)))
        (unless (member id children)
          (puthash parent-id
                   (append children (list id))
                   eca-chat--expandable-content-children-index))))))

(defun eca-chat--unindex-expandable-content (ov-label)
  "Remove OV-LABEL from expandable block indexes."
  (when-let* ((id (overlay-get ov-label 'eca-chat--expandable-content-id)))
    (remhash id eca-chat--expandable-content-index)
    (remhash id eca-chat--expandable-content-children-index)
    (eca-chat--expandable-storage-clear id)
    (when-let* ((parent-id
                 (overlay-get ov-label
                              'eca-chat--expandable-content-parent-id)))
      (let ((children (delete id
                              (copy-sequence
                               (gethash parent-id
                                        eca-chat--expandable-content-children-index)))))
        (if children
            (puthash parent-id
                     children
                     eca-chat--expandable-content-children-index)
          (remhash parent-id eca-chat--expandable-content-children-index))))))

(defun eca-chat--update-expandable-block-faces ()
  "Recompute expandable-block background faces from the current theme.
Shift percentages are controlled by `eca-chat-expandable-block-bg-shift-1'
and `eca-chat-expandable-block-bg-shift-2'.  Lightens for dark themes,
darkens for light themes."
  (when-let* ((bg (face-background 'default nil t)))
    (let* ((dark? (eq 'dark (frame-parameter nil 'background-mode)))
           (fn (if dark? #'color-lighten-name #'color-darken-name))
           (bg1 (funcall fn bg eca-chat-expandable-block-bg-shift-1))
           (bg2 (funcall fn bg eca-chat-expandable-block-bg-shift-2)))
      (set-face-attribute 'eca-chat-expandable-block-1-face nil :background bg1)
      (set-face-attribute 'eca-chat-expandable-block-2-face nil :background bg2))))

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
     (car (sort label-candidates
                (lambda (a b)
                  (> (overlay-start a) (overlay-start b)))))
     (let ((best-ov nil)
           (best-span nil)
           (best-start nil))
       (maphash
        (lambda (_id ov)
          (when (overlay-buffer ov)
            (when-let* ((ov-content
                         (overlay-get ov 'eca-chat--expandable-content-ov-content))
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
                        best-start start))))))
        eca-chat--expandable-content-index)
       best-ov))))

(defun eca-chat--get-expandable-content (id)
  "Return the overlay if there is expandable content for ID."
  (let ((ov (gethash id eca-chat--expandable-content-index)))
    (when ov
      (if (overlay-buffer ov)
          ov
        (remhash id eca-chat--expandable-content-index)
        nil))))

(defun eca-chat--propertize-only-first-word (str &rest properties)
  "Return a new string propertizing PROPERTIES to the first word of STR.
If STR is empty or PROPERTIES is nil, return STR unchanged. Existing
text properties on STR are preserved; only the first word gets the
additional PROPERTIES. The first word is the substring up to the first
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

(defconst eca-chat--expandable-content-base-indent (make-string 3 ?\s))
(defconst eca-chat--expandable-content-nested-indent (make-string 6 ?\s))

(defun eca-chat--make-expandable-icons (icon-face &optional label-prefix)
  "Create open/close icons with ICON-FACE and optional LABEL-PREFIX.
Uses the `face' property (not `font-lock-face') because these strings
are used as `line-prefix' values, where `font-lock-face' is not rendered."
  (let ((open-icon (if icon-face
                       (propertize eca-chat-expandable-block-open-symbol 'face icon-face)
                     eca-chat-expandable-block-open-symbol))
        (close-icon (if icon-face
                        (propertize eca-chat-expandable-block-close-symbol 'face icon-face)
                      eca-chat-expandable-block-close-symbol)))
    (if label-prefix
        (cons (concat label-prefix open-icon) (concat label-prefix close-icon))
      (cons open-icon close-icon))))

(defun eca-chat--expandable-block-face (nested?)
  "Return the appropriate background face for an expandable block.
When NESTED? is non-nil, return the level-2 face; otherwise level-1."
  (if nested?
      'eca-chat-expandable-block-2-face
    'eca-chat-expandable-block-1-face))

(defun eca-chat--apply-face-to-line-prefixes (start end face)
  "Add background of FACE to every `line-prefix' string between START and END.
Only the `:background' attribute is applied so that existing foreground
colors (e.g. icon faces via `font-lock-face') are preserved."
  (when-let* ((bg (face-background face nil t)))
    (let ((bg-plist `(:background ,bg))
          (pos start))
      (while (< pos end)
        (let* ((next-change (or (next-single-property-change pos 'line-prefix nil end) end))
               (prefix (get-text-property pos 'line-prefix)))
          (when (and prefix (stringp prefix))
            (let ((new-prefix (copy-sequence prefix)))
              (add-face-text-property 0 (length new-prefix) bg-plist nil new-prefix)
              (put-text-property pos next-change 'line-prefix new-prefix)))
          (setq pos next-change))))))

(defun eca-chat--paint-nested-label (ov-label)
  "Paint OV-LABEL's `line-prefix` with the parent block's background face.
Does nothing for top-level blocks or when parent has no face set."
  (when (overlay-get ov-label 'eca-chat--expandable-content-nested)
    (when-let* ((parent-id (overlay-get ov-label 'eca-chat--expandable-content-parent-id))
                (parent-ov (eca-chat--get-expandable-content parent-id))
                (parent-content-ov (overlay-get parent-ov 'eca-chat--expandable-content-ov-content))
                (parent-face (overlay-get parent-content-ov 'face)))
      (save-excursion
        (goto-char (overlay-start ov-label))
        (eca-chat--apply-face-to-line-prefixes (point) (line-end-position) parent-face)))))

(defun eca-chat--insert-expandable-block (id label content open-icon close-icon
                                             content-indent
                                             &optional nested-props)
  "Insert an expandable block with ID, LABEL, and stored CONTENT.
OPEN-ICON and CLOSE-ICON are the toggle icons.
CONTENT-INDENT is the `line-prefix' for rendered content.
NESTED-PROPS is a plist with parent metadata for nested blocks."
  (let ((ov-label (make-overlay (point) (point) (current-buffer)))
        (label-indent (plist-get nested-props :label-indent)))
    (eca-chat--expandable-content-set id content)
    (overlay-put ov-label 'eca-chat--expandable-content-id id)
    (overlay-put ov-label 'eca-chat--expandable-content-open-icon open-icon)
    (overlay-put ov-label 'eca-chat--expandable-content-close-icon close-icon)
    (overlay-put ov-label 'eca-chat--expandable-content-toggle nil)
    (when nested-props
      (overlay-put ov-label 'eca-chat--expandable-content-nested t)
      (overlay-put ov-label 'eca-chat--expandable-content-parent-id
                   (plist-get nested-props :parent-id)))
    (unless nested-props
      (overlay-put ov-label 'eca-chat--expandable-content-segments nil))
    (eca-chat--insert
     (propertize
      (eca-chat--propertize-only-first-word
       label
       'line-prefix
       (cond
        ((not (string-empty-p (or content ""))) open-icon)
        (label-indent
         (concat label-indent
                 (make-string (length eca-chat-expandable-block-open-symbol)
                              ?\s)))))
      'keymap (let ((km (make-sparse-keymap)))
                (define-key km (kbd "<mouse-1>")
                            (lambda ()
                              (interactive)
                              (eca-chat--expandable-content-toggle id)))
                (define-key km (kbd "<tab>")
                            (lambda ()
                              (interactive)
                              (eca-chat--expandable-content-toggle id)))
                km)
      'help-echo "mouse-1 / tab / RET: expand/collapse"))
    (eca-chat--insert "\n")
    (let* ((start-point (point))
           (_ (eca-chat--insert "\n"))
           (ov-content (make-overlay start-point start-point
                                     (current-buffer) nil t))
           (nested? (plist-get nested-props :parent-id)))
      (overlay-put ov-content 'eca-chat--expandable-content-indent content-indent)
      (overlay-put ov-content 'eca-chat--expandable-block-nested nested?)
      (overlay-put ov-content 'priority (if nested? 1 0))
      (overlay-put ov-label 'eca-chat--expandable-content-ov-content ov-content)
      (eca-chat--index-expandable-content ov-label)
      ov-label)))

(defun eca-chat--render-nested-block (parent-ov child-spec)
  "Render CHILD-SPEC within PARENT-OV's content area."
  (-let* (((&plist :id id :label label :icon-face icon-face) child-spec)
          (content (eca-chat--child-spec-content child-spec))
          (parent-content-ov (overlay-get parent-ov
                                          'eca-chat--expandable-content-ov-content))
          (label-indent eca-chat--expandable-content-base-indent)
          (icons (eca-chat--make-expandable-icons icon-face label-indent)))
    (save-excursion
      (goto-char (overlay-end parent-content-ov))
      (unless (bolp) (eca-chat--insert "\n"))
      (eca-chat--insert-expandable-block
       id label content
       (car icons) (cdr icons)
       eca-chat--expandable-content-nested-indent
       (list :parent-id (overlay-get parent-ov 'eca-chat--expandable-content-id)
             :label-indent label-indent))
      (when-let* ((child-ov (eca-chat--get-expandable-content id)))
        (eca-chat--paint-nested-label child-ov)))))

(defun eca-chat--destroy-nested-blocks (parent-id)
  "Remove all nested block overlays that belong to PARENT-ID."
  (dolist (child-id
           (copy-sequence
            (gethash parent-id eca-chat--expandable-content-children-index)))
    (when-let* ((ov (eca-chat--get-expandable-content child-id)))
      (eca-chat--destroy-nested-blocks child-id)
      (when-let* ((content-ov
                   (overlay-get ov 'eca-chat--expandable-content-ov-content)))
        (delete-overlay content-ov))
      (eca-chat--unindex-expandable-content ov)
      (delete-overlay ov)))
  (remhash parent-id eca-chat--expandable-content-children-index))

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

(defun eca-chat--add-expandable-content (id label content
                                             &optional parent-id at-point)
  "Add interactive LABEL content for ID at the current chat position.
When expanded, shows CONTENT.
If PARENT-ID is non-nil, add the block under that parent.
If AT-POINT is non-nil, insert there instead of the default point."
  (if parent-id
      (when-let* ((parent-ov (eca-chat--get-expandable-content parent-id)))
        (let* ((segments (overlay-get parent-ov
                                      'eca-chat--expandable-content-segments))
               (existing-spec (-first (lambda (seg)
                                        (and (eq 'child (plist-get seg :type))
                                             (string= id (plist-get seg :id))))
                                      segments)))
          (if existing-spec
              (eca-chat--update-expandable-content id label content nil parent-id)
            (let* ((icon-face (get-text-property 0 'font-lock-face label))
                   (child-spec (list :type 'child
                                     :id id
                                     :label label
                                     :icon-face icon-face))
                   (child-spec (eca-chat--child-spec-set-content child-spec content))
                   (full-content (eca-chat--expandable-content-raw parent-id))
                   (segmented-text (eca-chat--segments-total-text segments))
                   (unsegmented-text
                    (when (> (length full-content) (length segmented-text))
                      (substring full-content (length segmented-text))))
                   (new-segments
                    (append segments
                            (when unsegmented-text
                              (list (list :type 'text :content unsegmented-text)))
                            (list child-spec))))
              (overlay-put parent-ov 'eca-chat--expandable-content-segments
                           new-segments)
              (when (overlay-get parent-ov 'eca-chat--expandable-content-toggle)
                (eca-chat--render-nested-block parent-ov child-spec))))))
    (save-excursion
      (let* ((start-point (or at-point (eca-chat--content-insertion-point)))
             (icon-face (get-text-property 0 'font-lock-face label))
             (icons (eca-chat--make-expandable-icons icon-face)))
        (goto-char start-point)
        (unless (bolp) (eca-chat--insert "\n"))
        (eca-chat--insert-expandable-block
         id label content
         (car icons) (cdr icons)
         eca-chat--expandable-content-base-indent)))))

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
      (eca-chat--unindex-expandable-content ov-label)
      (delete-overlay ov-label)
      ;; Remove the text region
      (let ((inhibit-read-only t))
        (delete-region start end)))))

(defun eca-chat--update-expandable-content (id label content
                                                &optional append-content?
                                                parent-id)
  "Update expandable block ID with LABEL and CONTENT.
If LABEL is nil, keep the current label.
If APPEND-CONTENT? is non-nil, append CONTENT efficiently.
If PARENT-ID is non-nil and ID is not rendered, update the parent spec."
  (if-let* ((ov-label (eca-chat--get-expandable-content id)))
      (let* ((ov-content (overlay-get ov-label
                                      'eca-chat--expandable-content-ov-content))
             (nested? (overlay-get ov-label 'eca-chat--expandable-content-nested))
             (indent (or (overlay-get ov-content 'eca-chat--expandable-content-indent)
                         (if nested?
                             eca-chat--expandable-content-nested-indent
                           eca-chat--expandable-content-base-indent)))
             (open? (overlay-get ov-label 'eca-chat--expandable-content-toggle))
             (updated-raw
              (progn
                (if append-content?
                    (eca-chat--expandable-content-append id content)
                  (eca-chat--expandable-content-set id content))
                (eca-chat--expandable-content-raw id))))
        (save-excursion
          (when label
            (let* ((new-icon-face (get-text-property 0 'font-lock-face label))
                   (label-indent (when nested?
                                   eca-chat--expandable-content-base-indent))
                   (new-icons (eca-chat--make-expandable-icons new-icon-face
                                                               label-indent))
                   (children (eca-chat--segments-children
                              (overlay-get ov-label
                                           'eca-chat--expandable-content-segments)))
                   (has-content? (or (not (string-empty-p updated-raw)) children))
                   (label-prefix
                    (cond
                     (has-content?
                      (if open?
                          (overlay-get ov-label
                                       'eca-chat--expandable-content-close-icon)
                        (overlay-get ov-label
                                     'eca-chat--expandable-content-open-icon)))
                     (nested?
                      (concat
                       eca-chat--expandable-content-base-indent
                       (make-string
                        (length eca-chat-expandable-block-open-symbol)
                        ?\s))))))
              (overlay-put ov-label 'eca-chat--expandable-content-open-icon
                           (car new-icons))
              (overlay-put ov-label 'eca-chat--expandable-content-close-icon
                           (cdr new-icons))
              (goto-char (overlay-start ov-label))
              (delete-region (point) (1- (overlay-start ov-content)))
              (eca-chat--insert
               (propertize
                (eca-chat--propertize-only-first-word label
                                                     'line-prefix label-prefix)
                'help-echo "mouse-1 / RET / tab: expand/collapse"))
              (eca-chat--paint-nested-label ov-label)))
          (when open?
            (let ((block-face (overlay-get ov-content 'face)))
              (if append-content?
                  (let ((insert-start (overlay-end ov-content))
                        (delta (propertize (or content "") 'line-prefix indent)))
                    (goto-char insert-start)
                    (eca-chat--insert delta)
                    (when block-face
                      (eca-chat--apply-face-to-line-prefixes
                       insert-start (overlay-end ov-content) block-face)))
                (delete-region (overlay-start ov-content) (overlay-end ov-content))
                (goto-char (overlay-start ov-content))
                (eca-chat--insert
                 (eca-chat--expandable-content-render id indent))
                (when block-face
                  (eca-chat--apply-face-to-line-prefixes
                   (overlay-start ov-content) (overlay-end ov-content)
                   block-face))))))
        (when nested?
          (when-let* ((owner-id (overlay-get ov-label
                                             'eca-chat--expandable-content-parent-id))
                      (parent-ov (eca-chat--get-expandable-content owner-id))
                      (segments (overlay-get parent-ov
                                             'eca-chat--expandable-content-segments))
                      (spec (-first (lambda (seg)
                                      (and (eq 'child (plist-get seg :type))
                                           (string= id (plist-get seg :id))))
                                    segments)))
            (when label
              (plist-put spec :label label)
              (plist-put spec :icon-face (get-text-property 0 'font-lock-face
                                                            label)))
            (if append-content?
                (eca-chat--child-spec-append-content spec content)
              (eca-chat--child-spec-set-content spec content)))))
    (if parent-id
        (when-let* ((parent-ov (eca-chat--get-expandable-content parent-id)))
          (let* ((segments (overlay-get parent-ov
                                        'eca-chat--expandable-content-segments))
                 (existing-spec (-first (lambda (spec)
                                          (and (eq 'child (plist-get spec :type))
                                               (string= id (plist-get spec :id))))
                                        segments)))
            (if existing-spec
                (progn
                  (when label
                    (plist-put existing-spec :label label)
                    (plist-put existing-spec :icon-face
                               (get-text-property 0 'font-lock-face label)))
                  (if append-content?
                      (eca-chat--child-spec-append-content existing-spec content)
                    (eca-chat--child-spec-set-content existing-spec content)))
              (let ((child-spec (list :type 'child
                                     :id id
                                     :label label
                                     :icon-face
                                     (get-text-property 0 'font-lock-face label))))
                (setq child-spec
                      (eca-chat--child-spec-set-content child-spec content))
                (overlay-put parent-ov 'eca-chat--expandable-content-segments
                             (append segments (list child-spec)))
                (when (overlay-get parent-ov 'eca-chat--expandable-content-toggle)
                  (eca-chat--render-nested-block parent-ov child-spec))))))
      (eca-chat--add-expandable-content id label (or content "")))))

(defun eca-chat--expandable-content-toggle (id &optional force? close?)
  "Toggle the expandable-content of ID.
If FORCE? is non-nil, use CLOSE? as the target state."
  (when-let* ((ov-label (eca-chat--get-expandable-content id)))
    (let* ((ov-content (overlay-get ov-label
                                    'eca-chat--expandable-content-ov-content))
           (indent (or (overlay-get ov-content 'eca-chat--expandable-content-indent)
                       eca-chat--expandable-content-base-indent))
           (content (eca-chat--expandable-content-raw id))
           (segments (overlay-get ov-label 'eca-chat--expandable-content-segments))
           (children (eca-chat--segments-children segments))
           (has-content? (or (not (string-empty-p content)) children))
           (currently-open? (overlay-get ov-label
                                         'eca-chat--expandable-content-toggle))
           (close? (if force? close? currently-open?))
           (already-in-state? (and force?
                                   (if close?
                                       (not currently-open?)
                                     currently-open?))))
      (unless already-in-state?
        (eca-chat--with-preserved-scroll
         (save-excursion
           (goto-char (overlay-start ov-label))
           (if (or close? (not has-content?))
               (progn
                 (put-text-property
                  (point) (line-end-position)
                  'line-prefix
                  (when has-content?
                    (overlay-get ov-label
                                 'eca-chat--expandable-content-open-icon)))
                 (goto-char (1+ (line-end-position)))
                 (eca-chat--destroy-nested-blocks id)
                 (delete-region (overlay-start ov-content) (overlay-end ov-content))
                 (overlay-put ov-content 'face nil)
                 (overlay-put ov-label 'eca-chat--expandable-content-toggle nil))
             (put-text-property
              (point) (line-end-position)
              'line-prefix
              (overlay-get ov-label 'eca-chat--expandable-content-close-icon))
             (goto-char (overlay-start ov-content))
             (if segments
                 (let* ((full-content content)
                        (segmented-text (eca-chat--segments-total-text segments))
                        (trailing-text
                         (when (> (length full-content) (length segmented-text))
                           (substring full-content (length segmented-text)))))
                   (dolist (seg segments)
                     (pcase (plist-get seg :type)
                       ('text
                        (eca-chat--insert
                         (propertize (plist-get seg :content)
                                     'line-prefix indent)))
                       ('child
                        (eca-chat--render-nested-block ov-label seg)
                        (goto-char (overlay-end ov-content)))))
                   (when (and trailing-text (not (string-empty-p trailing-text)))
                     (eca-chat--insert
                      (propertize trailing-text 'line-prefix indent)))
                   (eca-chat--insert "\n"))
               (eca-chat--insert (eca-chat--expandable-content-render id indent)
                                 "\n")
               (dolist (child-spec children)
                 (eca-chat--render-nested-block ov-label child-spec)))
             (let ((block-face
                    (eca-chat--expandable-block-face
                     (overlay-get ov-content 'eca-chat--expandable-block-nested))))
               (overlay-put ov-content 'face block-face)
               (eca-chat--apply-face-to-line-prefixes
                (overlay-start ov-content) (overlay-end ov-content) block-face))
             (overlay-put ov-label 'eca-chat--expandable-content-toggle t))
           (eca-chat--paint-nested-label ov-label))))
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
