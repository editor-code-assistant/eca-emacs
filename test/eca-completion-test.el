;;; eca-completion-test.el --- Tests for eca-completion -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'buttercup)
(require 'color)
(require 'eca-completion)

(defun eca-completion-test--has-face-property-p (str)
  "Return non-nil when STR carries a non-nil `face' text property."
  (let ((pos 0)
        (len (length str))
        (found nil))
    (while (and (not found) (< pos len))
      (when (get-text-property pos 'face str)
        (setq found t))
      (setq pos (1+ pos)))
    found))

(defun eca-completion-test--face-foreground-at (str pos)
  "Resolve the foreground at POS in STR.
Falls back to the `default' face's foreground when none is set."
  (or (eca-completion--resolve-foreground
       (get-text-property pos 'face str))
      (face-foreground 'default)))

(defun eca-completion-test--deleted-overlay-p (overlay)
  "Return non-nil when OVERLAY is a region-replace deletion preview."
  (eq (overlay-get overlay 'face)
      'eca-completion-region-replace-deleted-face))

(defun eca-completion-test--string-has-face-p (str face)
  "Return non-nil when any position of STR has FACE in its `face' property.
The `face' property may be a single face or a list of faces; this
helper handles both."
  (let ((pos 0)
        (len (length str))
        (found nil))
    (while (and (not found) (< pos len))
      (let ((val (get-text-property pos 'face str)))
        (when (or (eq val face)
                  (and (listp val) (memq face val)))
          (setq found t)))
      (setq pos (1+ pos)))
    found))

;; eca-completion-diff-opcodes

(describe "eca-completion-diff-opcodes"
  (it "returns no opcodes for two empty vectors"
    (expect (eca-completion-diff-opcodes [] []) :to-be nil)))

;; eca-completion--fontify-as-mode

(describe "eca-completion--fontify-as-mode"
  (it "applies font-lock faces using the given major-mode"
    (let* ((src "(defun foo () 1)")
           (out (eca-completion--fontify-as-mode src 'emacs-lisp-mode)))
      (expect (stringp out) :to-be-truthy)
      (expect out :to-equal src)
      (expect (eca-completion-test--has-face-property-p out)
              :to-be-truthy)))

  (it "returns a fresh copy of TEXT when MODE is nil"
    (let* ((src "anything")
           (out (eca-completion--fontify-as-mode src nil)))
      (expect out :to-equal src)
      (expect (eq out src) :not :to-be-truthy)))

  (it "returns a fresh copy of TEXT when MODE is unbound"
    (let* ((src "anything")
           (out (eca-completion--fontify-as-mode
                 src 'eca-completion-test--no-such-mode)))
      (expect out :to-equal src)
      (expect (eq out src) :not :to-be-truthy))))

;; eca-completion--blend-color

(describe "eca-completion--blend-color"
  (it "returns BG when ratio is 0.0"
    (expect (eca-completion--blend-color "#ff0000" "#000000" 0.0)
            :to-equal "#000000"))

  (it "returns FG when ratio is 1.0"
    (expect (eca-completion--blend-color "#ff0000" "#000000" 1.0)
            :to-equal "#ff0000"))

  (it "returns a midpoint between FG and BG at ratio 0.5"
    ;; #ff0000 blended 50/50 with #000000 -> ~#7f0000 (rounding to 2 digits).
    (let ((mid (eca-completion--blend-color "#ff0000" "#000000" 0.5)))
      (expect (stringp mid) :to-be-truthy)
      (expect (string-match-p "\\`#[0-9a-f]\\{6\\}\\'" mid)
              :to-be-truthy)
      (expect mid :not :to-equal "#ff0000")
      (expect mid :not :to-equal "#000000")))

  (it "returns nil when either color cannot be resolved"
    (expect (eca-completion--blend-color nil "#000000" 0.5) :to-be nil)
    (expect (eca-completion--blend-color "#ffffff" nil 0.5) :to-be nil)
    (expect (eca-completion--blend-color "not-a-real-color"
                                         "#000000" 0.5)
            :to-be nil)))

;; eca-completion--dim-fontified

(describe "eca-completion--dim-fontified"
  (it "blends each colored span's foreground toward BG"
    (let* ((src (propertize "abc" 'face '(:foreground "#ff0000")))
           (dimmed (eca-completion--dim-fontified src "#000000" 0.5))
           (fg-after (eca-completion-test--face-foreground-at dimmed 0)))
      (expect dimmed :to-equal src)
      ;; A new, blended foreground should be layered on top.
      (expect fg-after :not :to-equal "#ff0000")
      (expect (string-match-p "\\`#[0-9a-f]\\{6\\}\\'" fg-after)
              :to-be-truthy)))

  (it "returns a fresh copy and does not mutate its argument"
    (let* ((src (propertize "x" 'face '(:foreground "#ff0000")))
           (src-fg-before (eca-completion-test--face-foreground-at src 0))
           (_ (eca-completion--dim-fontified src "#000000" 0.5))
           (src-fg-after (eca-completion-test--face-foreground-at src 0)))
      (expect src-fg-after :to-equal src-fg-before)
      (expect src-fg-after :to-equal "#ff0000")))

  (it "leaves spans untouched when colors cannot be resolved"
    (let* ((src (propertize "x" 'face '(:foreground "not-a-color")))
           ;; `default' may itself resolve, so to truly disable blending
           ;; we also pass an unresolvable BG.
           (dimmed (eca-completion--dim-fontified src "still-not-a-color"
                                                  0.5)))
      ;; No new foreground was layered, so resolution still yields the
      ;; original (unparseable) value or default fallback.
      (expect dimmed :to-equal src))))

;; eca-completion--after-change

(describe "eca-completion--after-change"
  (it "increments eca-completion--doc-version on each call"
    (with-temp-buffer
      (setq-local eca-completion--doc-version 3)
      (eca-completion--after-change 1 1 0)
      (expect eca-completion--doc-version :to-equal 4)
      (eca-completion--after-change 1 1 0)
      (expect eca-completion--doc-version :to-equal 5))))

;; eca-completion--show-completion + eca-completion-accept

(describe "eca-completion--show-completion (legacy zero-width)"
  (it "renders an after-cursor ghost overlay tagged 'legacy"
    (with-temp-buffer
      (insert "hello")
      (goto-char (point-max))
      (setq-local eca-completion--doc-version 0)
      (eca-completion--show-completion
       '(:text " world"
         :id "1"
         :range (:start (:line 1 :character 6) :end (:line 1 :character 6))
         :docVersion 0))
      (expect (eca-completion--overlay-visible) :to-be-truthy)
      (expect (overlay-get eca-completion--overlay 'eca-mode)
              :to-equal 'legacy)))

  (it "accepts the suggestion by inserting at the cursor"
    (with-temp-buffer
      (insert "hello")
      (goto-char (point-max))
      (setq-local eca-completion--doc-version 0)
      (eca-completion--show-completion
       '(:text " world"
         :id "1"
         :range (:start (:line 1 :character 6) :end (:line 1 :character 6))
         :docVersion 0))
      (eca-completion-accept)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "hello world")
      ;; Overlay is gone after accept.
      (expect (eca-completion--overlay-visible) :not :to-be-truthy)))

  (it "drops stale completions whose docVersion no longer matches"
    (with-temp-buffer
      (insert "x")
      (setq-local eca-completion--doc-version 5)
      (eca-completion--show-completion
       '(:text "y"
         :id "1"
         :range (:start (:line 1 :character 2) :end (:line 1 :character 2))
         :docVersion 1))
      (expect (eca-completion--overlay-visible) :not :to-be-truthy))))

(describe "eca-completion--show-completion (region-replace before cursor)"
  (it "tags the overlay 'region-replace and replaces atomically on accept"
    (with-temp-buffer
      (insert "thersholdd")
      (goto-char (point-max))
      (setq-local eca-completion--doc-version 0)
      ;; Drop the trailing duplicate 'd' (chars 10..11 in 1-based protocol).
      (eca-completion--show-completion
       '(:text ""
         :id "1"
         :range (:start (:line 1 :character 10) :end (:line 1 :character 11))
         :docVersion 0))
      (expect (eca-completion--overlay-visible) :to-be-truthy)
      (expect (overlay-get eca-completion--overlay 'eca-mode)
              :to-equal 'region-replace)
      (eca-completion-accept)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "thershold"))))

(describe "eca-completion--show-completion (region-replace multi-line)"
  (it "replaces a multi-line range and lands point at start + len(text)"
    (with-temp-buffer
      (insert "alpha\nbeta\ngamma")
      (goto-char (point-min))
      (setq-local eca-completion--doc-version 0)
      ;; Replace "beta" on line 2 with "BETA".
      (eca-completion--show-completion
       '(:text "BETA"
         :id "1"
         :range (:start (:line 2 :character 1) :end (:line 2 :character 5))
         :docVersion 0))
      (expect (overlay-get eca-completion--overlay 'eca-mode)
              :to-equal 'region-replace)
      (eca-completion-accept)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "alpha\nBETA\ngamma")
      ;; Point should be right after the inserted "BETA" — buffer position 11.
      (expect (point) :to-equal 11))))

(describe "eca-completion--show-completion (region-replace inserts new lines)"
  (it "supports replacement text that introduces new lines"
    (with-temp-buffer
      (insert "first\nsecond")
      (goto-char (point-max))
      (setq-local eca-completion--doc-version 0)
      ;; Replace "second" with "second\nthird".
      (eca-completion--show-completion
       '(:text "second\nthird"
         :id "1"
         :range (:start (:line 2 :character 1) :end (:line 2 :character 7))
         :docVersion 0))
      (eca-completion-accept)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "first\nsecond\nthird"))))

(describe "eca-completion--show-completion (region-replace preview)"
  (it "clears sub-overlays with the main overlay"
    (with-temp-buffer
      (insert "foo bar")
      (goto-char (point-min))
      (setq-local eca-completion--doc-version 0)
      (let ((eca-completion-syntax-highlight nil))
        (eca-completion--show-completion
         '(:text "foo baz"
           :id "1"
           :range (:start (:line 1 :character 1) :end (:line 1 :character 8))
           :docVersion 0)))
      (let ((subs (overlay-get eca-completion--overlay 'sub-overlays)))
        (expect subs :to-be-truthy)
        (eca-completion--clear-overlay)
        (expect (cl-every #'null (mapcar #'overlay-buffer subs))
                :to-be-truthy))))

  (it "highlights deleted and inserted token spans"
    (with-temp-buffer
      (insert "foo bar")
      (goto-char (point-min))
      (setq-local eca-completion--doc-version 0)
      (let ((eca-completion-syntax-highlight nil))
        (eca-completion--show-completion
         '(:text "foo baz"
           :id "1"
           :range (:start (:line 1 :character 1) :end (:line 1 :character 8))
           :docVersion 0)))
      (let* ((subs (overlay-get eca-completion--overlay 'sub-overlays))
             (deleted (cl-find-if
                       (lambda (sub)
                         (eq (overlay-get sub 'face)
                             'eca-completion-region-replace-deleted-face))
                       subs))
             (inserted (cl-find-if
                        (lambda (sub)
                          (let ((after (overlay-get sub 'after-string)))
                            (and after
                                 (eca-completion-test--string-has-face-p
                                  after
                                  'eca-completion-region-replace-inserted-face))))
                        subs)))
        (expect deleted :to-be-truthy)
        (expect inserted :to-be-truthy))))

  (it "previews pure deletion with deleted token spans"
    (with-temp-buffer
      (insert "thersholdd")
      (goto-char (point-max))
      (setq-local eca-completion--doc-version 0)
      (let ((eca-completion-syntax-highlight nil))
        (eca-completion--show-completion
         '(:text ""
           :id "1"
           :range (:start (:line 1 :character 10) :end (:line 1 :character 11))
           :docVersion 0)))
      (expect (cl-find-if
               (lambda (sub)
                 (eq (overlay-get sub 'face)
                     'eca-completion-region-replace-deleted-face))
               (overlay-get eca-completion--overlay 'sub-overlays))
              :to-be-truthy)
      (eca-completion-accept)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "thershold")))

  (it "aligns shifted unchanged lines after a full-line deletion"
    (with-temp-buffer
      (insert "console.log('')\nif (!isIssue && !isPR) return {};")
      (goto-char (point-min))
      (setq-local eca-completion--doc-version 0)
      (let ((eca-completion-syntax-highlight nil))
        (eca-completion--show-completion
         '(:text ""
           :id "1"
           :range (:start (:line 1 :character 1) :end (:line 2 :character 1))
           :docVersion 0)))
      (let* ((subs (overlay-get eca-completion--overlay 'sub-overlays))
             (line-2-start (save-excursion
                             (goto-char (point-min))
                             (forward-line 1)
                             (point)))
             (deleted-on-line-2
              (cl-find-if
               (lambda (sub)
                 (and (eca-completion-test--deleted-overlay-p sub)
                      (>= (overlay-start sub) line-2-start)))
               subs))
             (inserted-if-preview
              (cl-find-if
               (lambda (sub)
                 (let ((after (overlay-get sub 'after-string)))
                   (and after
                        (string-match-p "if (!isIssue && !isPR) return {};"
                                        after))))
               subs)))
        (expect (cl-find-if #'eca-completion-test--deleted-overlay-p subs)
                :to-be-truthy)
        (expect deleted-on-line-2 :to-be nil)
        (expect inserted-if-preview :to-be nil))))

  (it "aligns shifted unchanged lines after a full-line insertion"
    (with-temp-buffer
      (insert "return value;")
      (goto-char (point-min))
      (setq-local eca-completion--doc-version 0)
      (let ((eca-completion-syntax-highlight nil))
        (eca-completion--show-completion
         '(:text "const value = compute();\nreturn value;"
           :id "1"
           :range (:start (:line 1 :character 1) :end (:line 1 :character 14))
           :docVersion 0)))
      (let* ((subs (overlay-get eca-completion--overlay 'sub-overlays))
             (deleted-return
              (cl-find-if #'eca-completion-test--deleted-overlay-p subs))
             (inserted-value-preview
              (cl-find-if
               (lambda (sub)
                 (let ((after (overlay-get sub 'after-string)))
                   (and after
                        (string-match-p "return value;" after))))
               subs))
             (inserted-new-line-preview
              (cl-find-if
               (lambda (sub)
                 (let ((after (overlay-get sub 'after-string)))
                   (and after
                        (string-match-p "const value = compute();" after))))
               subs)))
        (expect deleted-return :to-be nil)
        (expect inserted-value-preview :to-be nil)
        (expect inserted-new-line-preview :to-be-truthy))))

  (it "aligns an unchanged line after deleting partial-line text"
    (with-temp-buffer
      (insert "    console.log(\"isDraft:\", isDraft);\n    const \n    if (!isIssue && !isPR) return {};")
      (goto-char (point-min))
      (setq-local eca-completion--doc-version 0)
      (let ((eca-completion-syntax-highlight nil))
        (eca-completion--show-completion
         '(:text "\n    if (!isIssue && !isPR) return {};"
           :id "1"
           :range (:start (:line 2 :character 1) :end (:line 3 :character 38))
           :docVersion 0)))
      (let* ((subs (overlay-get eca-completion--overlay 'sub-overlays))
             (line-3-start (save-excursion
                             (goto-char (point-min))
                             (forward-line 2)
                             (point)))
             (deleted-on-if-line
              (cl-find-if
               (lambda (sub)
                 (and (eca-completion-test--deleted-overlay-p sub)
                      (>= (overlay-start sub) line-3-start)))
               subs))
             (inserted-if-preview
              (cl-find-if
               (lambda (sub)
                 (let ((after (overlay-get sub 'after-string)))
                   (and after
                        (string-match-p "if (!isIssue && !isPR) return {};"
                                        after))))
               subs)))
        (expect (cl-find-if #'eca-completion-test--deleted-overlay-p subs)
                :to-be-truthy)
        (expect deleted-on-if-line :to-be nil)
        (expect inserted-if-preview :to-be nil)))))

(defun eca-completion-test--after-string-subs (subs)
  "Return the elements of SUBS that carry an `after-string' property."
  (cl-remove-if-not (lambda (s) (overlay-get s 'after-string)) subs))

(defun eca-completion-test--deleted-subs (subs)
  "Return the deletion-faced overlays in SUBS."
  (cl-remove-if-not #'eca-completion-test--deleted-overlay-p subs))

(describe "eca-completion--show-completion (region-replace removes 2, adds 1)"
  (it "marks both deleted lines red and anchors the preview after them"
    (with-temp-buffer
      (insert "first\nsecond\nthird")
      (goto-char (point-min))
      (setq-local eca-completion--doc-version 0)
      (let ((eca-completion-syntax-highlight nil))
        (eca-completion--show-completion
         '(:text "REPLACED"
           :id "1"
           :range (:start (:line 2 :character 1) :end (:line 3 :character 6))
           :docVersion 0)))
      (let* ((subs (overlay-get eca-completion--overlay 'sub-overlays))
             (deleted (eca-completion-test--deleted-subs subs))
             (afters (eca-completion-test--after-string-subs subs))
             (line-3-end (save-excursion (goto-char (point-min))
                                         (forward-line 2)
                                         (line-end-position))))
        (expect (length deleted) :to-equal 2)
        ;; Multi-line delete + single-line insert no longer pairs with the
        ;; first deleted line; the green preview renders as its own block
        ;; anchored after the entire deletion run, so red and green never
        ;; share a visual row.
        (expect (length afters) :to-equal 1)
        (expect (overlay-start (car afters)) :to-equal line-3-end)
        (expect (string-match-p "REPLACED"
                                (overlay-get (car afters) 'after-string))
                :to-be-truthy)))))

(describe "eca-completion--show-completion (region-replace removes 1, adds 2)"
  (it "anchors the new-line block at the consumed line's end"
    (with-temp-buffer
      (insert "first\nsecond\nthird")
      (goto-char (point-min))
      (setq-local eca-completion--doc-version 0)
      (let ((eca-completion-syntax-highlight nil))
        (eca-completion--show-completion
         '(:text "A\nB"
           :id "1"
           :range (:start (:line 2 :character 1) :end (:line 2 :character 7))
           :docVersion 0)))
      (let* ((subs (overlay-get eca-completion--overlay 'sub-overlays))
             (deleted (eca-completion-test--deleted-subs subs))
             (afters (eca-completion-test--after-string-subs subs))
             (line-2-end (save-excursion (goto-char (point-min))
                                         (forward-line 1)
                                         (line-end-position))))
        (expect (length deleted) :to-equal 1)
        (expect (length afters) :to-equal 1)
        (expect (overlay-start (car afters)) :to-equal line-2-end)
        (expect (cl-some (lambda (a)
                           (string-match-p
                            "A" (overlay-get a 'after-string)))
                         afters)
                :to-be-truthy)
        (expect (cl-some (lambda (a)
                           (string-match-p
                            "B" (overlay-get a 'after-string)))
                         afters)
                :to-be-truthy)))))

(describe "eca-completion--show-completion (region-replace 2 changed lines)"
  (it "renders the green block after the deletion run, never on a deleted row"
    (with-temp-buffer
      (insert "      (shared/)\n      (strng/replace fence-re \"$1\")")
      (goto-char (point-min))
      (setq-local eca-completion--doc-version 0)
      (let ((eca-completion-syntax-highlight nil))
        (eca-completion--show-completion
         `(:text ,(concat "trim-preamble-postamble)\n"
                          "      (stri")
           :id "1"
           :range (:start (:line 1 :character 15)
                   :end (:line 2 :character 11))
           :docVersion 0)))
      (let* ((subs (overlay-get eca-completion--overlay 'sub-overlays))
             (deleted (eca-completion-test--deleted-subs subs))
             (afters (eca-completion-test--after-string-subs subs))
             (max-deleted-end
              (apply #'max 0 (mapcar #'overlay-end deleted))))
        ;; All inserted previews collapse into one contiguous green block
        ;; anchored after every deletion, so red and green never share a
        ;; visual row.
        (expect (length afters) :to-equal 1)
        (expect (>= (overlay-start (car afters)) max-deleted-end)
                :to-be-truthy)))))

(describe "eca-completion--show-completion (region-replace independent edits)"
  (it "leaves unchanged context lines untouched between two edits"
    (with-temp-buffer
      (insert "one\ntwo\nthree\nfour\nfive")
      (goto-char (point-min))
      (setq-local eca-completion--doc-version 0)
      (let ((eca-completion-syntax-highlight nil))
        (eca-completion--show-completion
         '(:text "ONE\ntwo\nthree\nfour\nFIVE"
           :id "1"
           :range (:start (:line 1 :character 1) :end (:line 5 :character 5))
           :docVersion 0)))
      (let* ((subs (overlay-get eca-completion--overlay 'sub-overlays))
             (deleted (eca-completion-test--deleted-subs subs))
             (afters (eca-completion-test--after-string-subs subs))
             (line-2-start (save-excursion (goto-char (point-min))
                                           (forward-line 1)
                                           (point)))
             (line-5-start (save-excursion (goto-char (point-min))
                                           (forward-line 4)
                                           (point))))
        (expect (length deleted) :to-equal 2)
        (expect (length afters) :to-equal 2)
        (expect (cl-every (lambda (s)
                            (or (< (overlay-start s) line-2-start)
                                (>= (overlay-start s) line-5-start)))
                          subs)
                :to-be-truthy)
        (expect (cl-some (lambda (a)
                           (string-match-p
                            "ONE" (overlay-get a 'after-string)))
                         afters)
                :to-be-truthy)
        (expect (cl-some (lambda (a)
                           (string-match-p
                            "FIVE" (overlay-get a 'after-string)))
                         afters)
                :to-be-truthy)))))

(describe "eca-completion--show-completion (region-replace sub-line token)"
  (it "narrows the deletion to the changed token within the line"
    (with-temp-buffer
      (insert "let x = 1;")
      (goto-char (point-min))
      (setq-local eca-completion--doc-version 0)
      (let ((eca-completion-syntax-highlight nil))
        (eca-completion--show-completion
         '(:text "let x = 42;"
           :id "1"
           :range (:start (:line 1 :character 1) :end (:line 1 :character 11))
           :docVersion 0)))
      (let* ((subs (overlay-get eca-completion--overlay 'sub-overlays))
             (deleted (eca-completion-test--deleted-subs subs))
             (afters (eca-completion-test--after-string-subs subs)))
        (expect (length deleted) :to-equal 1)
        (let* ((d (car deleted))
               (s (overlay-start d))
               (e (overlay-end d)))
          (expect (- e s) :to-equal 1)
          (expect (buffer-substring-no-properties s e) :to-equal "1"))
        (expect (length afters) :to-equal 1)
        (let ((after (overlay-get (car afters) 'after-string)))
          (expect (string-match-p "let x = 42;" after) :to-be-truthy)
          (let* ((idx (string-match "42" after))
                 (slice (substring after idx (+ idx 2))))
            (expect (eca-completion-test--string-has-face-p
                     slice 'eca-completion-region-replace-inserted-face)
                    :to-be-truthy)))))))

(describe "eca-completion--show-completion (region-replace EOF no newline)"
  (it "previews and accepts the last line without a trailing newline"
    (with-temp-buffer
      (insert "line1\nline2")
      (goto-char (point-min))
      (setq-local eca-completion--doc-version 0)
      (let ((eca-completion-syntax-highlight nil))
        (eca-completion--show-completion
         '(:text "LINE2"
           :id "1"
           :range (:start (:line 2 :character 1) :end (:line 2 :character 6))
           :docVersion 0)))
      (let* ((subs (overlay-get eca-completion--overlay 'sub-overlays))
             (afters (eca-completion-test--after-string-subs subs)))
        (expect afters :to-be-truthy)
        (expect (cl-every (lambda (a)
                            (let ((s (overlay-get a 'after-string)))
                              (not (equal s "\n"))))
                          afters)
                :to-be-truthy))
      (eca-completion-accept)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "line1\nLINE2"))))

(describe "eca-completion--show-completion (region-replace multibyte EOF)"
  (it "covers the multibyte deletion span and accepts cleanly"
    (with-temp-buffer
      (insert "hello\n세계")
      (goto-char (point-min))
      (setq-local eca-completion--doc-version 0)
      (let ((eca-completion-syntax-highlight nil))
        (eca-completion--show-completion
         '(:text "world"
           :id "1"
           :range (:start (:line 2 :character 1) :end (:line 2 :character 3))
           :docVersion 0)))
      (let* ((subs (overlay-get eca-completion--overlay 'sub-overlays))
             (deleted (eca-completion-test--deleted-subs subs)))
        (expect (length deleted) :to-equal 1)
        (let* ((d (car deleted))
               (span (- (overlay-end d) (overlay-start d))))
          (expect span :to-equal (length "세계"))))
      (eca-completion-accept)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "hello\nworld"))))

(describe "eca-completion--show-completion (region-replace identical text)"
  (it "creates no preview sub-overlays when text matches the range"
    (with-temp-buffer
      (insert "foo")
      (goto-char (point-min))
      (setq-local eca-completion--doc-version 0)
      (let ((eca-completion-syntax-highlight nil))
        (eca-completion--show-completion
         '(:text "foo"
           :id "1"
           :range (:start (:line 1 :character 1) :end (:line 1 :character 4))
           :docVersion 0)))
      (let ((subs (overlay-get eca-completion--overlay 'sub-overlays)))
        (expect subs :to-be nil)))))

(describe "eca-completion--show-completion (legacy multi-line insertion)"
  (it "routes zero-width multi-line text to the legacy ghost path"
    (with-temp-buffer
      (insert "hello")
      (goto-char (point-max))
      (setq-local eca-completion--doc-version 0)
      (let ((eca-completion-syntax-highlight nil))
        (eca-completion--show-completion
         '(:text "a\nb"
           :id "1"
           :range (:start (:line 1 :character 6) :end (:line 1 :character 6))
           :docVersion 0)))
      (expect (eca-completion--overlay-visible) :to-be-truthy)
      (expect (overlay-get eca-completion--overlay 'eca-mode)
              :to-equal 'legacy)
      (expect (overlay-get eca-completion--overlay 'sub-overlays)
              :to-be nil))))

(provide 'eca-completion-test)
;;; eca-completion-test.el ends here
