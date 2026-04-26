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

(provide 'eca-completion-test)
;;; eca-completion-test.el ends here
