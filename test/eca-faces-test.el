;;; eca-faces-test.el --- Tests for TTY-safe face background -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Regression tests for issue #244 — chat startup failed with
;; (wrong-number-of-arguments color-rgb-to-hsl 0) on a no-window
;; Emacs because `face-background' returned the literal sentinel
;; "unspecified-bg" instead of nil, which was then fed straight
;; into `color-lighten-name' / `color-darken-name'.
;;
;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'eca-util)
(require 'eca-chat-expandable)
(require 'eca-table)

(describe "eca-safe-face-background"
  (it "returns nil when face-background returns nil"
    (cl-letf (((symbol-function 'face-background) (lambda (&rest _) nil)))
      (expect (eca-safe-face-background 'default) :to-be nil)))

  (it "returns nil for the unspecified-bg TTY sentinel"
    (cl-letf (((symbol-function 'face-background)
               (lambda (&rest _) "unspecified-bg")))
      (expect (eca-safe-face-background 'default) :to-be nil)))

  (it "returns nil for the unspecified-fg TTY sentinel"
    (cl-letf (((symbol-function 'face-background)
               (lambda (&rest _) "unspecified-fg")))
      (expect (eca-safe-face-background 'default) :to-be nil)))

  (it "returns the value when face-background returns a real color"
    (cl-letf (((symbol-function 'face-background)
               (lambda (&rest _) "#1e1e2e")))
      (expect (eca-safe-face-background 'default) :to-equal "#1e1e2e")))

  (it "forwards FRAME and INHERIT to face-background"
    (let (captured)
      (cl-letf (((symbol-function 'face-background)
                 (lambda (&rest args)
                   (setq captured args)
                   "#abcdef")))
        (eca-safe-face-background 'default 'some-frame t)
        (expect captured :to-equal '(default some-frame t))))))

(describe "eca-chat--update-expandable-block-faces (issue #244)"
  (it "does not signal and leaves faces untouched on a TTY"
    (set-face-attribute 'eca-chat-expandable-block-1-face nil
                        :background 'unspecified)
    (set-face-attribute 'eca-chat-expandable-block-2-face nil
                        :background 'unspecified)
    (cl-letf (((symbol-function 'face-background)
               (lambda (&rest _) "unspecified-bg")))
      ;; Before the fix this raised (wrong-number-of-arguments
      ;; color-rgb-to-hsl 0) and aborted chat startup.
      (eca-chat--update-expandable-block-faces))
    (expect (face-attribute 'eca-chat-expandable-block-1-face :background nil)
            :to-equal 'unspecified)
    (expect (face-attribute 'eca-chat-expandable-block-2-face :background nil)
            :to-equal 'unspecified))

  (it "computes block backgrounds when default has a real color"
    (set-face-attribute 'eca-chat-expandable-block-1-face nil
                        :background 'unspecified)
    (set-face-attribute 'eca-chat-expandable-block-2-face nil
                        :background 'unspecified)
    (cl-letf (((symbol-function 'face-background)
               (lambda (&rest _) "#202020")))
      (eca-chat--update-expandable-block-faces))
    (expect (face-attribute 'eca-chat-expandable-block-1-face :background nil)
            :not :to-equal 'unspecified)
    (expect (face-attribute 'eca-chat-expandable-block-2-face :background nil)
            :not :to-equal 'unspecified)))

(describe "eca-table-update-faces (issue #244)"
  (it "does not signal and leaves faces untouched on a TTY"
    (set-face-attribute 'eca-table-header-face nil :background 'unspecified)
    (set-face-attribute 'eca-table-row-even-face nil :background 'unspecified)
    (cl-letf (((symbol-function 'face-background)
               (lambda (&rest _) "unspecified-bg")))
      (eca-table-update-faces))
    (expect (face-attribute 'eca-table-header-face :background nil)
            :to-equal 'unspecified)
    (expect (face-attribute 'eca-table-row-even-face :background nil)
            :to-equal 'unspecified))

  (it "computes table backgrounds when default has a real color"
    (set-face-attribute 'eca-table-header-face nil :background 'unspecified)
    (set-face-attribute 'eca-table-row-even-face nil :background 'unspecified)
    (cl-letf (((symbol-function 'face-background)
               (lambda (&rest _) "#202020")))
      (eca-table-update-faces))
    (expect (face-attribute 'eca-table-header-face :background nil)
            :not :to-equal 'unspecified)
    (expect (face-attribute 'eca-table-row-even-face :background nil)
            :not :to-equal 'unspecified)))

(provide 'eca-faces-test)
;;; eca-faces-test.el ends here
