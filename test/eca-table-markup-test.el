;;; eca-table-markup-test.el --- Test hidden-markup table compensation -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Interactive test for table column alignment with hidden markup.
;;
;;  Usage:
;;    M-x eval-buffer   (in this file)
;;    M-x eca-table-markup-test
;;
;;  This opens a test buffer in markdown-mode containing tables
;;  with bold/italic cells, aligns them, hides markup via
;;  `markdown-hide-markup', and applies the compensation from
;;  `eca-table.el' to restore correct visual alignment.
;;
;;  The buffer shows two sections for comparison:
;;    1. "BEFORE" — aligned + markup hidden (misaligned)
;;    2. "AFTER"  — with compensation (aligned)
;;
;;; Code:

(require 'markdown-mode)
(require 'eca-table)

;; ---------------------------------------------------------------
;; Sample tables
;; ---------------------------------------------------------------

(defconst eca-table-markup-test--tables
  '(;; Table 1: bold header, bold and italic in data
    "| **Column A** | **Column B** | **Column C** |
|---|---|---|
| 10 | **25** | 30 |
| 40 | 55 | *60* |
| 70 | 85 | 90 |"

    ;; Table 2: bold and italic in data cells
    "| Name | Score | Notes |
|---|---|---|
| Alice | **100** | *excellent* |
| Bob | 75 | plain |
| Carol | **90** | good |"

    ;; Table 3: multiple bold spans in one cell
    "| Key | Value |
|---|---|
| **a** and **b** | normal |
| plain | **highlighted** |"

    ;; Table 4: no markup (control — should be unchanged)
    "| X | Y | Z |
|---|---|---|
| 1 | 2 | 3 |
| 4 | 5 | 6 |"))

;; ---------------------------------------------------------------
;; Test harness
;; ---------------------------------------------------------------

(defun eca-table-markup-test--insert-section
    (title tables compensate)
  "Insert TITLE and TABLES into the current buffer.
If COMPENSATE is non-nil, apply markup compensation after
alignment."
  (let ((section-beg (point)))
    (insert (format "## %s\n\n" title))
    (dolist (tbl tables)
      (insert tbl)
      (insert "\n\n"))
    (let ((section-end (point)))
      ;; Align tables in this section
      (save-excursion
        (goto-char section-beg)
        (while (and (< (point) section-end)
                    (re-search-forward
                     markdown-table-line-regexp
                     section-end t))
          (when (markdown-table-at-point-p)
            (markdown-table-align)
            (setq section-end (point-max))
            (goto-char (markdown-table-end)))))
      (font-lock-ensure)
      ;; Optionally compensate
      (when compensate
        (save-excursion
          (goto-char section-beg)
          (while (and (< (point) (point-max))
                      (re-search-forward
                       markdown-table-line-regexp
                       nil t))
            (when (markdown-table-at-point-p)
              (eca-table--compensate-hidden-markup)
              (goto-char (markdown-table-end)))))))))

(defun eca-table-markup-test ()
  "Open a test buffer showing table alignment before/after fix."
  (interactive)
  (let ((buf (get-buffer-create "*eca-table-markup-test*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (markdown-mode)
        (setq-local markdown-hide-markup t)
        (add-to-invisibility-spec 'markdown-markup)
        (face-remap-add-relative
         'markdown-table-face '(:inherit fixed-pitch))
        (insert "# Table Markup Compensation Test\n\n")
        (insert (propertize
                 (concat
                  "BEFORE: columns with markup are too narrow.\n"
                  "AFTER:  compensation restores alignment.\n\n")
                 'face 'font-lock-comment-face))
        (eca-table-markup-test--insert-section
         "BEFORE (no compensation)"
         eca-table-markup-test--tables
         nil)
        (insert "\n---\n\n")
        (eca-table-markup-test--insert-section
         "AFTER (with compensation)"
         eca-table-markup-test--tables
         t)
        (font-lock-ensure)
        (goto-char (point-min))))
    (switch-to-buffer buf)))

;; ---------------------------------------------------------------
;; Unit-style checks
;; ---------------------------------------------------------------

(defun eca-table-markup-test-run-checks ()
  "Run programmatic checks; return t if all pass.
Results are printed to *Messages*."
  (interactive)
  (let ((pass t))
    ;; Check 1: regex counter
    (message "--- Check: markup counting ---")
    (let ((cases '((" **25** " . 4)
                   (" *60* " . 2)
                   (" **a** and **b** " . 8)
                   (" normal " . 0)
                   (" 55 " . 0))))
      (dolist (c cases)
        (let* ((input (car c))
               (expected (cdr c))
               (actual (eca-table--count-markup-chars
                        input)))
          (if (= actual expected)
              (message "  PASS: %S -> %d" input actual)
            (message "  FAIL: %S -> %d (expected %d)"
                     input actual expected)
            (setq pass nil)))))
    ;; Check 2: compensation overlays
    (message "--- Check: table compensation ---")
    (with-temp-buffer
      (markdown-mode)
      (setq-local markdown-hide-markup t)
      (add-to-invisibility-spec 'markdown-markup)
      (insert
       "| A | B |\n|---|---|\n| **x** | *y* |\n")
      (goto-char (point-min))
      (markdown-table-align)
      (font-lock-ensure)
      (goto-char (point-min))
      (when (markdown-table-at-point-p)
        (eca-table--compensate-hidden-markup)
        (let* ((tbl-beg (markdown-table-begin))
               (tbl-end (markdown-table-end))
               (ovs (seq-filter
                     (lambda (ov)
                       (overlay-get ov 'eca-table-markup-pad))
                     (overlays-in tbl-beg tbl-end))))
          (if (= (length ovs) 2)
              (message "  PASS: 2 compensation overlays")
            (message "  FAIL: expected 2, got %d"
                     (length ovs))
            (setq pass nil))
          (dolist (ov ovs)
            (message "    overlay at %d: pad=%d"
                     (overlay-start ov)
                     (length
                      (overlay-get ov 'after-string))))
          ;; Idempotency
          (goto-char (point-min))
          (eca-table--compensate-hidden-markup)
          (let ((ovs2 (seq-filter
                       (lambda (ov)
                         (overlay-get
                          ov 'eca-table-markup-pad))
                       (overlays-in tbl-beg tbl-end))))
            (if (= (length ovs2) (length ovs))
                (message "  PASS: idempotent (%d overlays)"
                         (length ovs2))
              (message "  FAIL: not idempotent (%d -> %d)"
                       (length ovs) (length ovs2))
              (setq pass nil))))))
    (message (if pass "All checks PASSED"
               "Some checks FAILED"))
    pass))

(provide 'eca-table-markup-test)
;;; eca-table-markup-test.el ends here
