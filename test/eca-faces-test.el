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

(describe "eca-chat--expandable-tint-face"
  (it "tints with the color of a face over the default background"
    (let ((eca-chat--expandable-tint-faces nil)
          (eca-chat-expandable-block-tint 50))
      (spy-on 'eca--tint-color :and-call-fake
              (lambda (source alpha) (format "%s %s" source alpha)))
      (let ((face (eca-chat--expandable-tint-face 'eca-chat-reason-label-face)))
        (expect (face-attribute face :background)
                :to-equal "eca-chat-reason-label-face 0.5")
        ;; Up to the window edge, past the end of the lines.
        (expect (face-attribute face :extend) :to-be t)
        (expect (get face 'eca-chat-expandable-tint) :to-be 'eca-chat-reason-label-face)
        ;; Made once.
        (expect (eca-chat--expandable-tint-face 'eca-chat-reason-label-face)
                :to-be face))))

  (it "gives nested blocks a face of their own, of the same color"
    (let ((eca-chat--expandable-tint-faces nil))
      (spy-on 'eca--tint-color :and-return-value "#123456")
      (let ((top (eca-chat--expandable-tint-face 'eca-chat-reason-label-face))
            (nested (eca-chat--expandable-tint-face 'eca-chat-reason-label-face t)))
        (expect nested :not :to-be top)
        (expect (face-attribute nested :background)
                :to-equal (face-attribute top :background)))))

  (it "tints failed tool calls and user messages as much as their option tells"
    (let ((eca-chat--expandable-tint-faces nil)
          (eca-chat-expandable-block-tint 7)
          (eca-chat-expandable-block-error-tint 12)
          (eca-chat-user-message-tint 10))
      (spy-on 'eca--tint-color :and-call-fake (lambda (_source alpha) (format "%s" alpha)))
      (cl-flet ((tint (source)
                  (face-attribute (eca-chat--expandable-tint-face source) :background)))
        (expect (tint 'eca-chat-tool-call-success-face) :to-equal "0.07")
        (expect (tint 'eca-chat-tool-call-error-face) :to-equal "0.12")
        (expect (tint 'eca-chat-user-message-bar-face) :to-equal "0.1"))))

  (it "leaves the default background with no tint or unknown colors"
    (let ((eca-chat--expandable-tint-faces nil)
          (eca-chat-expandable-block-tint 0))
      (spy-on 'eca--tint-color :and-return-value "#123456")
      (expect (face-attribute (eca-chat--expandable-tint-face 'eca-chat-reason-label-face)
                              :background)
              :to-be 'unspecified))
    (let ((eca-chat--expandable-tint-faces nil))
      (spy-on 'eca--tint-color :and-return-value nil)
      (expect (face-attribute (eca-chat--expandable-tint-face 'eca-chat-reason-label-face)
                              :background)
              :to-be 'unspecified)))

  (it "follows theme switches"
    (let ((eca-chat--expandable-tint-faces nil)
          (color "#111111"))
      (spy-on 'eca--tint-color :and-call-fake (lambda (&rest _) color))
      (let ((face (eca-chat--expandable-tint-face 'eca-chat-reason-label-face)))
        (setq color "#222222")
        (cl-letf (((symbol-function 'face-background) (lambda (&rest _) "#101010")))
          (eca-chat--refresh-theme-faces))
        (expect (face-attribute face :background) :to-equal "#222222"))))

  (it "follows theme switches on terminals not telling their colors"
    (let ((eca-chat--expandable-tint-faces nil)
          (color "#111111"))
      (spy-on 'eca--tint-color :and-call-fake (lambda (&rest _) color))
      (let ((face (eca-chat--expandable-tint-face 'eca-chat-reason-label-face)))
        (setq color nil)
        (cl-letf (((symbol-function 'face-background) (lambda (&rest _) "unspecified-bg")))
          (eca-chat--update-expandable-block-faces))
        (expect (face-attribute face :background) :to-be 'unspecified)))))

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
