;;; eca-faces-test.el --- Tests for theme-derived faces -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Regression tests for issue #244 — chat startup failed with
;; (wrong-number-of-arguments color-rgb-to-hsl 0) on a no-window
;; Emacs because `face-background' returned the literal sentinel
;; "unspecified-bg" instead of nil, which was then fed straight
;; into `color-lighten-name' / `color-darken-name'.
;;
;; And for issue #301 — chat faces did not follow theme switches:
;; the refresh hook was buffer-local (so it never ran from the
;; buffer where `load-theme' was called) and `line-prefix' strings
;; baked resolved background colors instead of face symbols.
;;
;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'seq)
(require 'eca-util)
(require 'eca-chat-expandable)
(require 'eca-table)
(require 'eca-chat)

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

(describe "eca-chat--apply-face-to-line-prefixes (issue #301)"
  (it "stores the face symbol instead of a resolved color"
    (with-temp-buffer
      (insert (propertize "hello\n" 'line-prefix "   "))
      (eca-chat--apply-face-to-line-prefixes
       (point-min) (point-max) 'eca-chat-expandable-block-1-face)
      (let* ((prefix (get-text-property (point-min) 'line-prefix))
             (face (get-text-property 0 'face prefix))
             (faces (if (listp face) face (list face))))
        (expect (memq 'eca-chat-expandable-block-1-face faces)
                :to-be-truthy))))

  (it "preserves existing prefix faces and takes background priority"
    (with-temp-buffer
      (insert (propertize "hello\n" 'line-prefix
                          (propertize ">" 'face 'font-lock-keyword-face)))
      (eca-chat--apply-face-to-line-prefixes
       (point-min) (point-max) 'eca-chat-expandable-block-2-face)
      (let* ((prefix (get-text-property (point-min) 'line-prefix))
             (face (get-text-property 0 'face prefix))
             (faces (if (listp face) face (list face))))
        (expect (memq 'eca-chat-expandable-block-2-face faces) :to-be-truthy)
        (expect (memq 'font-lock-keyword-face faces) :to-be-truthy)
        ;; Prepended, so its background overrides later faces.
        (expect (car faces) :to-be 'eca-chat-expandable-block-2-face))))

  (it "does not accumulate duplicate faces on repeated paints"
    (with-temp-buffer
      (insert (propertize "hello\n" 'line-prefix "   "))
      (dotimes (_ 3)
        (eca-chat--apply-face-to-line-prefixes
         (point-min) (point-max) 'eca-chat-expandable-block-1-face))
      (let* ((prefix (get-text-property (point-min) 'line-prefix))
             (face (get-text-property 0 'face prefix))
             (faces (if (listp face) face (list face))))
        (expect (seq-count (lambda (f)
                             (eq f 'eca-chat-expandable-block-1-face))
                           faces)
                :to-equal 1)))))

(describe "eca-chat theme switch refresh (issue #301)"
  (it "eca-chat--refresh-theme-faces recomputes block and table faces"
    (set-face-attribute 'eca-chat-expandable-block-1-face nil
                        :background 'unspecified)
    (set-face-attribute 'eca-table-header-face nil :background 'unspecified)
    (cl-letf (((symbol-function 'face-background)
               (lambda (&rest _) "#101010")))
      (eca-chat--refresh-theme-faces))
    (expect (face-attribute 'eca-chat-expandable-block-1-face :background nil)
            :not :to-equal 'unspecified)
    (expect (face-attribute 'eca-table-header-face :background nil)
            :not :to-equal 'unspecified))

  (it "registers a global refresh so switching from any buffer works"
    (unwind-protect
        (progn
          (eca-chat--register-theme-refresh)
          (if (>= emacs-major-version 29)
              (progn
                (expect (memq #'eca-chat--refresh-theme-faces
                              (default-value 'enable-theme-functions))
                        :to-be-truthy)
                (expect (memq #'eca-chat--refresh-theme-faces
                              (default-value 'disable-theme-functions))
                        :to-be-truthy))
            (expect (advice-member-p #'eca-chat--refresh-theme-faces
                                     'enable-theme)
                    :to-be-truthy)
            (expect (advice-member-p #'eca-chat--refresh-theme-faces
                                     'disable-theme)
                    :to-be-truthy)))
      (if (>= emacs-major-version 29)
          (progn
            (remove-hook 'enable-theme-functions #'eca-chat--refresh-theme-faces)
            (remove-hook 'disable-theme-functions #'eca-chat--refresh-theme-faces))
        (advice-remove 'enable-theme #'eca-chat--refresh-theme-faces)
        (advice-remove 'disable-theme #'eca-chat--refresh-theme-faces)))))

(provide 'eca-faces-test)
;;; eca-faces-test.el ends here
