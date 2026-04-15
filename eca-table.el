;;; eca-table.el --- Table beautification for ECA chat -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Enhanced visual rendering of markdown tables in ECA chat buffers.
;;  Provides header highlighting, separator dimming, zebra-striped rows,
;;  dimmed pipe characters, and a toggle between wrapped and scrollable
;;  display for wide tables.
;;
;;  All visual changes are overlay-only — buffer text is untouched so
;;  copy/paste works normally.
;;
;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'markdown-mode)
(require 'seq)

;; Forward-declare defcustom defined in eca-chat.el.
(defvar eca-chat-table-beautify)
(defvar eca-chat-parent-mode)

;; Faces ------------------------------------------------------------------

(defface eca-table-action-bar-face
  '((((background dark))  (:foreground "turquoise" :weight bold))
    (((background light)) (:foreground "dark cyan" :weight bold)))
  "Face for the table action bar label."
  :group 'eca)

(defface eca-table-action-key-face
  '((t (:inherit shadow)))
  "Face for keybinding hints in the table action bar."
  :group 'eca)

(defface eca-table-header-face
  '((t :weight bold :extend t))
  "Face for the header row of markdown tables.
Background is computed dynamically from the current theme by
`eca-table-update-faces'."
  :group 'eca)

(defface eca-table-separator-face
  '((((background dark))  (:foreground "gray30" :extend t))
    (((background light)) (:foreground "gray75" :extend t)))
  "Face for the separator row (e.g. |---|---|) in markdown tables."
  :group 'eca)

(defface eca-table-pipe-face
  '((((background dark))  (:foreground "gray40"))
    (((background light)) (:foreground "gray65")))
  "Face for pipe characters in markdown tables."
  :group 'eca)

(defface eca-table-row-even-face
  '((t :extend t))
  "Face for even-numbered data rows in markdown tables (zebra striping).
Background is computed dynamically from the current theme by
`eca-table-update-faces'."
  :group 'eca)

(defun eca-table-update-faces ()
  "Recompute table faces from the current theme.
Sets subtle background tints on header and even-row faces."
  (when-let* ((bg (face-background 'default nil t)))
    (let* ((dark? (eq 'dark (frame-parameter nil 'background-mode)))
           (fn (if dark? #'color-lighten-name #'color-darken-name)))
      (set-face-attribute 'eca-table-header-face nil
                          :background (funcall fn bg 8))
      (set-face-attribute 'eca-table-row-even-face nil
                          :background (funcall fn bg 3)))))

;; Keymap -----------------------------------------------------------------

(defvar-keymap eca-table-actions-map
  :doc "Keymap active on table overlays with an action bar."
  "w" #'eca-table-toggle-wrap)

;; Markdown display width ------------------------------------------------

(defvar eca-table--fontlock-buffer nil
  "Reusable buffer for font-lock based width measurement.")

(defun eca-table--get-fontlock-buffer ()
  "Return a reusable buffer configured for width measurement.
Uses the chat parent mode and markdown-hide-markup to match
the actual display rendering."
  (let ((buf (or (and (buffer-live-p eca-table--fontlock-buffer)
                      eca-table--fontlock-buffer)
                 (setq eca-table--fontlock-buffer
                       (generate-new-buffer " *eca-table-width*"))))
        (mode (or eca-chat-parent-mode 'gfm-mode)))
    (with-current-buffer buf
      (unless (derived-mode-p mode)
        (funcall mode)
        (setq-local markdown-hide-markup t)
        (add-to-invisibility-spec 'markdown-markup)))
    buf))

(defun eca-table--display-width (str)
  "Return the display width of STR as rendered by the chat mode.
Uses font-lock with `markdown-hide-markup' to determine which
characters are invisible, then sums the widths of visible ones."
  (with-current-buffer (eca-table--get-fontlock-buffer)
    (erase-buffer)
    (insert str)
    (font-lock-ensure)
    (let ((width 0))
      (dotimes (i (- (point-max) (point-min)))
        (let ((pos (+ (point-min) i)))
          (unless (invisible-p pos)
            (setq width (+ width (char-width (char-after pos)))))))
      width)))

;; Helpers ----------------------------------------------------------------

(defun eca-table--action-overlay-at-point ()
  "Return the table action overlay at point, or nil."
  (seq-find (lambda (ov) (overlay-get ov 'eca-table-action))
            (overlays-at (point))))

(defun eca-table--any-truncated-p ()
  "Return non-nil if any table action overlay in the buffer wants truncation."
  (let ((found nil))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (and (overlay-get ov 'eca-table-action)
                 (overlay-get ov 'eca-table-truncated))
        (setq found t)))
    found))

(defun eca-table--max-line-width (beg end)
  "Return the maximum display line width between BEG and END.
Subtracts hidden markup characters so the result reflects
the visual width when `markdown-hide-markup' is active."
  (let ((max-width 0))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let* ((raw (- (line-end-position)
                       (line-beginning-position)))
               (hidden (eca-table--count-markup-chars
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))))
          (setq max-width
                (max max-width (- raw hidden))))
        (forward-line 1)))
    max-width))

(defun eca-table--count-markup-chars (text)
  "Count hidden bold/italic `*' delimiters in TEXT.
Matches **text** (4 chars) and *text* (2 chars)."
  (let ((count 0)
        (start 0))
    (while (string-match
            "\\(\\*\\{1,2\\}\\)\\([^*].*?\\)\\1"
            text start)
      (setq count (+ count (* 2 (length
                                  (match-string 1 text)))))
      (setq start (match-end 0)))
    count))

(defun eca-table--action-bar-string (truncated-p)
  "Build the action bar before-string.
TRUNCATED-P indicates the current display mode."
  (let* ((mode-label (if truncated-p "scrollable" "wrapped"))
         (toggle-label (if truncated-p "wrap" "scroll"))
         (label (concat
                 (propertize "Table: " 'face 'eca-table-action-bar-face)
                 (propertize (concat "(" mode-label ")  ") 'face 'eca-table-action-key-face)
                 (if (fboundp 'rmc--add-key-description)
                     (cdr (funcall 'rmc--add-key-description (list ?w toggle-label)))
                   (concat "[w] " toggle-label)))))
    (concat label "\n")))

(defun eca-table--refresh-action-bar (ov)
  "Update the before-string of table action overlay OV."
  (overlay-put ov 'before-string
               (eca-table--action-bar-string
                (overlay-get ov 'eca-table-truncated))))

;; Commands ---------------------------------------------------------------

(defun eca-table-toggle-wrap ()
  "Toggle between wrapped and scrollable (truncated) table display."
  (interactive)
  (when-let* ((ov (eca-table--action-overlay-at-point)))
    (let ((currently-truncated (overlay-get ov 'eca-table-truncated)))
      (overlay-put ov 'eca-table-truncated (not currently-truncated))
      (eca-table--refresh-action-bar ov)
      ;; Update truncate-lines: enable if ANY table overlay wants truncation
      (setq-local truncate-lines (eca-table--any-truncated-p))
      (when (not truncate-lines)
        (setq-local word-wrap t)))))

;; Overlay management -----------------------------------------------------

(defun eca-table-remove-overlays (beg end)
  "Remove all table-beautify overlays between BEG and END."
  (dolist (ov (overlays-in beg end))
    (when (overlay-get ov 'eca-table-overlay)
      (delete-overlay ov))))

(defun eca-table--beautify-at-point ()
  "Apply visual overlays to the markdown table at point.
Point must be inside a table.  Adds header highlighting, dims the
separator row and pipe characters, and applies zebra striping to
data rows.  For tables wider than the window, an action bar is shown
allowing the user to toggle between wrapped and scrollable display.
All changes are overlay-only — buffer text is untouched."
  (let* ((tbl-beg (markdown-table-begin))
         (tbl-end (markdown-table-end))
         (row-num 0)
         (max-width (eca-table--max-line-width tbl-beg tbl-end))
         (window-width (when-let* ((win (get-buffer-window (current-buffer))))
                         (window-body-width win)))
         (wide-p (and window-width (> max-width window-width))))
    ;; Clean any previous overlays in this table region
    (eca-table-remove-overlays tbl-beg tbl-end)
    ;; Add action bar for wide tables
    (when wide-p
      (let ((action-ov (make-overlay tbl-beg tbl-end)))
        (overlay-put action-ov 'eca-table-overlay t)
        (overlay-put action-ov 'eca-table-action t)
        (overlay-put action-ov 'eca-table-truncated nil)
        (overlay-put action-ov 'keymap eca-table-actions-map)
        (overlay-put action-ov 'evaporate t)
        (eca-table--refresh-action-bar action-ov)))
    (save-excursion
      (goto-char tbl-beg)
      (while (< (point) tbl-end)
        (let ((line-beg (line-beginning-position))
              (line-end (line-end-position)))
          (cond
           ;; Row 0: header row
           ((= row-num 0)
            (let ((ov (make-overlay line-beg (1+ line-end))))
              (overlay-put ov 'eca-table-overlay t)
              (overlay-put ov 'face 'eca-table-header-face)
              (overlay-put ov 'evaporate t)))
           ;; Row 1: separator row (|---|---|)
           ((= row-num 1)
            (let ((ov (make-overlay line-beg (1+ line-end))))
              (overlay-put ov 'eca-table-overlay t)
              (overlay-put ov 'face 'eca-table-separator-face)
              (overlay-put ov 'evaporate t)))
           ;; Data rows: zebra stripe even rows (row 2 = first data row = even)
           (t
            (when (cl-evenp row-num)
              (let ((ov (make-overlay line-beg (1+ line-end))))
                (overlay-put ov 'eca-table-overlay t)
                (overlay-put ov 'face 'eca-table-row-even-face)
                (overlay-put ov 'evaporate t)))))
          ;; Dim pipe characters on this line
          (save-excursion
            (goto-char line-beg)
            (while (search-forward "|" line-end t)
              (let ((ov (make-overlay (1- (point)) (point))))
                (overlay-put ov 'eca-table-overlay t)
                (overlay-put ov 'face 'eca-table-pipe-face)
                (overlay-put ov 'evaporate t)))))
        (setq row-num (1+ row-num))
        (forward-line 1)))))

;; Table parsing and alignment -------------------------------------------

(defun eca-table--parse-row (line)
  "Parse LINE into a list of cell contents.
Handles escaped pipes and code spans correctly.
Supports rows with or without leading/trailing pipes."
  (with-temp-buffer
    (insert line)
    (goto-char (point-min))
    (let ((cells '())
          (in-code nil)
          (cell-start nil))
      ;; Skip leading whitespace and optional first pipe
      (if (looking-at "[ \t]*|")
          (progn
            (goto-char (match-end 0))
            (setq cell-start (point)))
        (skip-chars-forward " \t")
        (setq cell-start (point)))
      (while (< (point) (point-max))
        (cond
         ;; Toggle code span state on backtick
         ((eq (char-after) ?\`)
          (setq in-code (not in-code))
          (forward-char 1))
         ;; Escaped character - skip next char (guard end of buffer)
         ((eq (char-after) ?\\)
          (forward-char (min 2 (- (point-max) (point)))))
         ;; Pipe outside code span - end of cell
         ((and (eq (char-after) ?|) (not in-code))
          (push (string-trim (buffer-substring-no-properties
                              cell-start (point)))
                cells)
          (forward-char 1)
          (setq cell-start (point)))
         (t (forward-char 1))))
      ;; Push trailing content if row lacks a trailing pipe
      (when (and cell-start (< cell-start (point-max)))
        (let ((trailing (string-trim
                         (buffer-substring-no-properties
                          cell-start (point-max)))))
          (unless (string-empty-p trailing)
            (push trailing cells))))
      (nreverse cells))))

(defun eca-table--separator-row-p (line)
  "Return non-nil if LINE is a markdown table separator row.
Each cell must match the separator grammar: optional colons
around one or more dashes (e.g. ---, :---, ---:, :---:)."
  (when (string-match-p "^[ \t]*|" line)
    (let ((cells (split-string
                  (replace-regexp-in-string
                   "^[ \t]*|\\||[ \t]*$" "" line)
                  "|" t)))
      (and cells
           (cl-every (lambda (cell)
                       (string-match-p
                        "^[ \t]*:?--+:?[ \t]*$"
                        cell))
                     cells)))))

(defun eca-table--parse-separator-alignments (line)
  "Parse alignment markers from separator LINE.
Returns a list of alignment specs per column: \"l\" for left
\(:---\), \"r\" for right \(---:\), \"c\" for center \(:---:\),
or nil for default."
  (let ((cells (split-string
                (replace-regexp-in-string
                 "^[ \t]*|\\||[ \t]*$" "" line)
                "|" t)))
    (mapcar (lambda (cell)
              (let ((s (string-trim cell)))
                (cond
                 ((and (string-prefix-p ":" s)
                       (string-suffix-p ":" s))
                  "c")
                 ((string-prefix-p ":" s) "l")
                 ((string-suffix-p ":" s) "r")
                 (t nil))))
            cells)))

(defun eca-table--make-separator-cell (width alignment)
  "Build a separator cell of WIDTH dashes with ALIGNMENT markers.
ALIGNMENT is \"l\", \"r\", \"c\", or nil for plain dashes."
  (pcase alignment
    ("c" (concat ":" (make-string (max 1 width) ?-) ":"))
    ("l" (concat ":" (make-string (max 1 (1+ width)) ?-)))
    ("r" (concat (make-string (max 1 (1+ width)) ?-) ":"))
    (_ (make-string (+ 2 width) ?-))))

(defun eca-table--insert-cell (cell padding alignment)
  "Insert CELL content with PADDING extra spaces using ALIGNMENT.
ALIGNMENT is \"l\", \"r\", \"c\", or nil (left-aligned)."
  (pcase alignment
    ("r"
     (insert (make-string (+ 1 padding) ?\s) cell " |"))
    ("c"
     (let* ((left (/ padding 2))
            (right (- padding left)))
       (insert (make-string (+ 1 left) ?\s) cell
               (make-string (+ 1 right) ?\s) "|")))
    (_
     (insert " " cell (make-string (+ 1 padding) ?\s) "|"))))

(defun eca-table--align-at-point ()
  "Align markdown table at point.
Uses display-width-aware calculation, preserves separator
alignment markers, and pads content cells accordingly."
  (let* ((tbl-beg (markdown-table-begin))
         (tbl-end (markdown-table-end))
         (indent (save-excursion
                   (goto-char tbl-beg)
                   (if (looking-at "[ \t]*") (match-string 0) "")))
         (lines (split-string (buffer-substring-no-properties tbl-beg tbl-end)
                              "\n" t))
         (parsed-rows '())
         (separator-indices '())
         (separator-alignments '())
         (row-idx 0)
         (max-cols 0)
         (col-max-display '()))
    ;; Parse all rows and track separator rows
    (dolist (line lines)
      (if (eca-table--separator-row-p line)
          (progn
            (push row-idx separator-indices)
            (push (eca-table--parse-separator-alignments line)
                  separator-alignments)
            (push nil parsed-rows))
        (let ((cells (eca-table--parse-row line)))
          (push cells parsed-rows)
          (setq max-cols (max max-cols (length cells)))))
      (setq row-idx (1+ row-idx)))
    (setq parsed-rows (nreverse parsed-rows))
    (setq separator-indices (nreverse separator-indices))
    (setq separator-alignments (nreverse separator-alignments))
    ;; Use first separator row for column alignments
    (let ((col-aligns (car separator-alignments)))
      ;; Calculate max display width for each column
      (dotimes (col max-cols)
        (let ((max-disp 1))
          (dolist (row parsed-rows)
            (when row
              (let* ((cell (or (nth col row) ""))
                     (disp-w (eca-table--display-width cell)))
                (setq max-disp (max max-disp disp-w)))))
          (push max-disp col-max-display)))
      (setq col-max-display (nreverse col-max-display))
      ;; Rebuild the table
      (delete-region tbl-beg tbl-end)
      (goto-char tbl-beg)
      (let ((row-idx 0)
            (sep-idx 0))
        (dolist (row parsed-rows)
          (insert indent "|")
          (if (member row-idx separator-indices)
              ;; Separator row: preserve alignment markers
              (let ((aligns (nth sep-idx separator-alignments)))
                (dotimes (col max-cols)
                  (let ((align (nth col aligns)))
                    (insert (eca-table--make-separator-cell
                             (nth col col-max-display) align)
                            "|")))
                (setq sep-idx (1+ sep-idx)))
            ;; Content row: pad using column alignment
            (dotimes (col max-cols)
              (let* ((cell (or (nth col row) ""))
                     (disp-w (eca-table--display-width cell))
                     (target-disp (nth col col-max-display))
                     (padding (- target-disp disp-w))
                     (align (nth col col-aligns)))
                (eca-table--insert-cell cell padding align))))
          (insert "\n")
          (setq row-idx (1+ row-idx)))))))

;; Public API -------------------------------------------------------------

(defun eca-table-align (from end)
  "Align all markdown tables between FROM and END.
After aligning each table, compensates for hidden bold/italic
markup so columns stay visually aligned."
  (save-excursion
    (goto-char from)
    (while (and (< (point) end)
                (re-search-forward markdown-table-line-regexp end t))
      (when (markdown-table-at-point-p)
        (markdown-table-align)
        (eca-table--align-at-point)
        ;; Move past this table to avoid re-processing
        (goto-char (markdown-table-end))))))

(defun eca-table-beautify (from end)
  "Apply visual enhancements to all markdown tables between FROM and END.
Respects `eca-chat-table-beautify'.  Should be called after
`eca-table-align'."
  (when eca-chat-table-beautify
    (save-excursion
      (goto-char from)
      ;; Clean any stale overlays first
      (eca-table-remove-overlays from end)
      (while (and (< (point) end)
                  (re-search-forward markdown-table-line-regexp end t))
        (when (markdown-table-at-point-p)
          (eca-table--beautify-at-point)
          ;; Move past this table
          (goto-char (markdown-table-end)))))))

(provide 'eca-table)
;;; eca-table.el ends here
