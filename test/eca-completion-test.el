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

;; eca-diff-lcs-opcodes

(describe "eca-diff-lcs-opcodes"
  (it "returns no opcodes for two empty vectors"
    (expect (eca-diff-lcs-opcodes [] []) :to-be nil)))
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

(provide 'eca-completion-test)
;;; eca-completion-test.el ends here
