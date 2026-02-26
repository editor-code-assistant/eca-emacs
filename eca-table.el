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
(require 'markdown-mode)
(require 'seq)

;; Forward-declare defcustom defined in eca-chat.el.
(defvar eca-chat-table-beautify)

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
  "Return the maximum line width in the region between BEG and END."
  (let ((max-width 0))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (setq max-width (max max-width (- (line-end-position) (line-beginning-position))))
        (forward-line 1)))
    max-width))

(defun eca-table--action-bar-string (truncated-p)
  "Build the action bar before-string.
TRUNCATED-P indicates the current display mode."
  (let* ((mode-label (if truncated-p "scrollable" "wrapped"))
         (toggle-label (if truncated-p "wrap" "scroll"))
         (label (concat
                 (propertize "Table: " 'face 'eca-table-action-bar-face)
                 (propertize (concat "(" mode-label ")  ") 'face 'eca-table-action-key-face)
                 (if (fboundp #'rmc--add-key-description)
                     (cdr (rmc--add-key-description (list ?w toggle-label)))
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

;; Public API -------------------------------------------------------------

(defun eca-table-align (from end)
  "Align all markdown tables between FROM and END."
  (save-excursion
    (goto-char from)
    (while (and (< (point) end)
                (re-search-forward markdown-table-line-regexp end t))
      (when (markdown-table-at-point-p)
        (markdown-table-align)
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
