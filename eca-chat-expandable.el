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
(defvar eca-buttons-allow-mouse)
(declare-function eca-chat--insert "eca-chat")
(declare-function eca-chat--content-insertion-point "eca-chat")

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
  "Return the overlay if there is a expandable content for ID."
  (-first (-lambda (ov) (string= id (overlay-get ov 'eca-chat--expandable-content-id)))
          (overlays-in (point-min) (point-max))))

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

(defun eca-chat--make-expandable-toggle-keymap (id &optional include-mouse)
  "Create keymap to toggle expandable block ID.
When INCLUDE-MOUSE is non-nil and `eca-buttons-allow-mouse' is non-nil,
bind `mouse-1' as well."
  (let ((km (make-sparse-keymap)))
    (when (and include-mouse eca-buttons-allow-mouse)
      (define-key km (kbd "<mouse-1>") (lambda () (interactive) (eca-chat--expandable-content-toggle id))))
    (define-key km (kbd "<tab>") (lambda () (interactive) (eca-chat--expandable-content-toggle id)))
    (define-key km (kbd "RET") (lambda () (interactive) (eca-chat--expandable-content-toggle id)))
    km))

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

(defun eca-chat--make-expandable-toggle-prefix (id prefix)
  "Return PREFIX with mouse/keyboard toggle bindings for expandable block ID."
  (let ((result (copy-sequence prefix))
        (km (eca-chat--make-expandable-toggle-keymap id t)))
    (when eca-buttons-allow-mouse
      (add-text-properties
       0 (length result)
       '(mouse-face highlight
         help-echo "mouse-1: expand/collapse")
       result))
    (put-text-property 0 (length result) 'keymap km result)
    result))

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

(defun eca-chat--insert-expandable-block (id label content open-icon close-icon content-indent &optional nested-props)
  "Insert an expandable block with ID, LABEL, CONTENT, icons and indent.
OPEN-ICON and CLOSE-ICON are the toggle icons.
CONTENT-INDENT is the `line-prefix` for content.
NESTED-PROPS is a plist with :parent-id and :label-indent for nested blocks."
  (let ((ov-label (make-overlay (point) (point) (current-buffer)))
        (label-indent (plist-get nested-props :label-indent)))
    (setq open-icon (eca-chat--make-expandable-toggle-prefix id open-icon))
    (setq close-icon (eca-chat--make-expandable-toggle-prefix id close-icon))
    (overlay-put ov-label 'eca-chat--expandable-content-id id)
    (overlay-put ov-label 'eca-chat--expandable-content-open-icon open-icon)
    (overlay-put ov-label 'eca-chat--expandable-content-close-icon close-icon)
    (overlay-put ov-label 'eca-chat--expandable-content-toggle nil)
    (when nested-props
      (overlay-put ov-label 'eca-chat--expandable-content-nested t)
      (overlay-put ov-label 'eca-chat--expandable-content-parent-id (plist-get nested-props :parent-id)))
    (unless nested-props
      (overlay-put ov-label 'eca-chat--expandable-content-segments nil))
    (eca-chat--insert (propertize (eca-chat--propertize-only-first-word label
                                                                        'line-prefix (cond
                                                                                      ((not (string-empty-p content)) open-icon)
                                                                                      (label-indent (concat label-indent
                                                                                                            (make-string (length eca-chat-expandable-block-open-symbol) ?\s)))))
                                  'keymap (eca-chat--make-expandable-toggle-keymap id t)
                                  'help-echo "TAB / RET / mouse-1: expand/collapse"))
    (eca-chat--insert "\n")
    (let* ((start-point (point))
           (_ (eca-chat--insert "\n"))
           (ov-content (make-overlay start-point start-point (current-buffer) nil t))
           (nested? (plist-get nested-props :parent-id)))
      (overlay-put ov-content 'eca-chat--expandable-content-content (propertize content 'line-prefix content-indent))
      (overlay-put ov-content 'eca-chat--expandable-block-nested nested?)
      (overlay-put ov-content 'priority (if nested? 1 0))
      (overlay-put ov-label 'eca-chat--expandable-content-ov-content ov-content))))

(defun eca-chat--render-nested-block (parent-ov child-spec)
  "Render a nested block CHILD-SPEC within PARENT-OV's content area."
  (-let* (((&plist :id id :label label :content content :icon-face icon-face) child-spec)
          (parent-content-ov (overlay-get parent-ov 'eca-chat--expandable-content-ov-content))
          (label-indent eca-chat--expandable-content-base-indent)
          (icons (eca-chat--make-expandable-icons icon-face label-indent)))
    (save-excursion
      (goto-char (overlay-end parent-content-ov))
      (unless (bolp) (eca-chat--insert "\n"))
      (eca-chat--insert-expandable-block id label content
                                         (car icons) (cdr icons)
                                         eca-chat--expandable-content-nested-indent
                                         (list :parent-id (overlay-get parent-ov 'eca-chat--expandable-content-id)
                                               :label-indent label-indent))
      ;; Paint label's line-prefix with parent's background when parent is open
      (when-let* ((child-ov (eca-chat--get-expandable-content id)))
        (eca-chat--paint-nested-label child-ov)))))

(defun eca-chat--destroy-nested-blocks (parent-id)
  "Remove all nested block overlays that belong to PARENT-ID."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (string= parent-id (overlay-get ov 'eca-chat--expandable-content-parent-id))
      (when-let* ((content-ov (overlay-get ov 'eca-chat--expandable-content-ov-content)))
        (delete-overlay content-ov))
      (delete-overlay ov))))

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
                   (child-spec (list :type 'child :id id :label label :content content :icon-face icon-face))
                   ;; Capture any unsegmented text that was appended to content
                   ;; since the last segment, and add it as a text segment first
                   (ov-content (overlay-get parent-ov 'eca-chat--expandable-content-ov-content))
                   (full-content (overlay-get ov-content 'eca-chat--expandable-content-content))
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
             (icons (eca-chat--make-expandable-icons icon-face)))
        (goto-char start-point)
        (unless (bolp) (eca-chat--insert "\n"))
        (eca-chat--insert-expandable-block id label content
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
      (delete-overlay ov-label)
      ;; Remove the text region
      (let ((inhibit-read-only t))
        (delete-region start end)))))

(defun eca-chat--update-expandable-content (id label content &optional append-content? parent-id)
  "Update to LABEL and CONTENT the expandable content of id ID.
If LABEL is nil, the existing label is preserved.
If APPEND-CONTENT? is non-nil, append CONTENT to existing content.
If PARENT-ID is provided and block doesn't exist yet, updates the spec
in parent."
  (if-let* ((ov-label (eca-chat--get-expandable-content id)))
      ;; Block exists (rendered), update it directly
      (let* ((ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content))
             (nested? (overlay-get ov-label 'eca-chat--expandable-content-nested))
             (indent (if nested?
                         eca-chat--expandable-content-nested-indent
                       eca-chat--expandable-content-base-indent))
             (existing (overlay-get ov-content 'eca-chat--expandable-content-content))
             (delta (propertize content 'line-prefix indent))
             (new-content (if append-content?
                              (concat existing delta)
                            delta))
             (open? (overlay-get ov-label 'eca-chat--expandable-content-toggle)))
        (overlay-put ov-content 'eca-chat--expandable-content-content new-content)
        (save-excursion
          (when label
            ;; Update stored icons when the label face changes
            (let* ((new-icon-face (get-text-property 0 'font-lock-face label))
                   (label-indent (when nested? eca-chat--expandable-content-base-indent))
                   (new-icons (eca-chat--make-expandable-icons new-icon-face label-indent)))
              (overlay-put ov-label 'eca-chat--expandable-content-open-icon
                           (eca-chat--make-expandable-toggle-prefix id (car new-icons)))
              (overlay-put ov-label 'eca-chat--expandable-content-close-icon
                           (eca-chat--make-expandable-toggle-prefix id (cdr new-icons))))
            (goto-char (overlay-start ov-label))
            (delete-region (point) (1- (overlay-start ov-content)))
            (let* ((children (eca-chat--segments-children
                              (overlay-get ov-label 'eca-chat--expandable-content-segments)))
                   (has-content? (or (not (string-empty-p new-content)) children))
                   (label-prefix (cond
                                  (has-content?
                                   (if open?
                                       (overlay-get ov-label 'eca-chat--expandable-content-close-icon)
                                     (overlay-get ov-label 'eca-chat--expandable-content-open-icon)))
                                  (nested?
                                   (concat eca-chat--expandable-content-base-indent
                                           (make-string (length eca-chat-expandable-block-open-symbol) ?\s))))))
              (eca-chat--insert (propertize (eca-chat--propertize-only-first-word label
                                                                                  'line-prefix label-prefix)
                                            'keymap (eca-chat--make-expandable-toggle-keymap id t)
                                            'help-echo "mouse-1 / RET / tab: expand/collapse")))
            ;; Repaint nested label's line-prefix after label replacement
            (eca-chat--paint-nested-label ov-label))
          (when open?
            (let ((block-face (overlay-get ov-content 'face)))
              (if append-content?
                  (let ((insert-start (overlay-end ov-content)))
                    (goto-char insert-start)
                    (eca-chat--insert delta)
                    (when block-face
                      (eca-chat--apply-face-to-line-prefixes
                       insert-start (overlay-end ov-content) block-face)))
                (progn
                  (delete-region (overlay-start ov-content) (overlay-end ov-content))
                  (goto-char (overlay-start ov-content))
                  (eca-chat--insert new-content)
                  (when block-face
                    (eca-chat--apply-face-to-line-prefixes
                     (overlay-start ov-content) (overlay-end ov-content) block-face)))))))
        ;; When nested, sync updated label/content back to parent's child spec
        ;; so that toggling the parent closed and reopened preserves latest state
        (when nested?
          (when-let* ((parent-id (overlay-get ov-label 'eca-chat--expandable-content-parent-id))
                      (parent-ov (eca-chat--get-expandable-content parent-id))
                      (segments (overlay-get parent-ov 'eca-chat--expandable-content-segments))
                      (spec (-first (lambda (s) (and (eq 'child (plist-get s :type))
                                                     (string= id (plist-get s :id))))
                                    segments)))
            (when label (plist-put spec :label label))
            (plist-put spec :content new-content))))
    ;; Block not rendered yet
    (if parent-id
        ;; Nested: update spec in parent
        (when-let* ((parent-ov (eca-chat--get-expandable-content parent-id)))
          (let* ((segments (overlay-get parent-ov 'eca-chat--expandable-content-segments))
                 (existing-spec (-first (lambda (spec) (and (eq 'child (plist-get spec :type))
                                                            (string= id (plist-get spec :id))))
                                        segments)))
            (if existing-spec
                (let* ((old-content (plist-get existing-spec :content))
                       (new-content (if append-content?
                                        (concat old-content content)
                                      content)))
                  (when label
                    (plist-put existing-spec :label label)
                    (plist-put existing-spec :icon-face (get-text-property 0 'font-lock-face label)))
                  (plist-put existing-spec :content new-content))
              (let ((child-spec (list :type 'child :id id :label label :content content
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
  (when-let* ((ov-label (-first (-lambda (ov) (string= id (overlay-get ov 'eca-chat--expandable-content-id)))
                                (overlays-in (point-min) (point-max)))))
    (let* ((ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content))
           (content (overlay-get ov-content 'eca-chat--expandable-content-content))
           (segments (overlay-get ov-label 'eca-chat--expandable-content-segments))
           (children (eca-chat--segments-children segments))
           (has-content? (or (not (string-empty-p content)) children))
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
                                     'line-prefix (when has-content?
                                                    (overlay-get ov-label 'eca-chat--expandable-content-open-icon)))
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
                (if segments
                    ;; Segments-aware rendering: interleave text and children in order
                    (let* ((full-content content)
                           (segmented-text (eca-chat--segments-total-text segments))
                           (trailing-text (when (> (length full-content) (length segmented-text))
                                            (substring full-content (length segmented-text)))))
                      (dolist (seg segments)
                        (pcase (plist-get seg :type)
                          ('text (eca-chat--insert (plist-get seg :content)))
                          ('child
                           (eca-chat--render-nested-block ov-label seg)
                           ;; render-nested-block uses save-excursion so point stays
                           ;; before the child; advance past it to maintain order
                           (goto-char (overlay-end ov-content)))))
                      ;; Insert any trailing text not yet captured in segments
                      (when (and trailing-text (not (string-empty-p trailing-text)))
                        (eca-chat--insert trailing-text))
                      (eca-chat--insert "\n"))
                  ;; Legacy path: no segments, insert content then children
                  (eca-chat--insert content "\n")
                  (dolist (child-spec children)
                    (eca-chat--render-nested-block ov-label child-spec)))
                (let ((block-face (eca-chat--expandable-block-face
                                  (overlay-get ov-content 'eca-chat--expandable-block-nested))))
                  (overlay-put ov-content 'face block-face)
                  (eca-chat--apply-face-to-line-prefixes
                   (overlay-start ov-content) (overlay-end ov-content) block-face))
                (overlay-put ov-label 'eca-chat--expandable-content-toggle t)))
            ;; Repaint nested label's line-prefix after icon swap
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
