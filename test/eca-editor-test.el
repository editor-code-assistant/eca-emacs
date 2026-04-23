;;; eca-editor-test.el --- Tests for eca-editor -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'eca-editor)

;; Forward declaration so byte-compilation does not warn when flycheck
;; is not installed in the test environment.
(defvar flycheck-mode)

(describe "eca-editor--flycheck-active-in-locus-p"
  (it "returns nil for a nil locus"
    (expect (eca-editor--flycheck-active-in-locus-p nil)
            :not :to-be-truthy))

  (it "returns nil for a file-path (string) locus"
    (expect (eca-editor--flycheck-active-in-locus-p "/tmp/foo.el")
            :not :to-be-truthy))

  (it "returns nil for a killed buffer"
    (let ((buf (generate-new-buffer " *eca-editor-test-killed*")))
      (kill-buffer buf)
      (expect (eca-editor--flycheck-active-in-locus-p buf)
              :not :to-be-truthy)))

  (it "returns nil for a live buffer where flycheck-mode is unset"
    (with-temp-buffer
      (expect (eca-editor--flycheck-active-in-locus-p (current-buffer))
              :not :to-be-truthy)))

  (it "returns nil for a live buffer where flycheck-mode is nil"
    (with-temp-buffer
      (setq-local flycheck-mode nil)
      (expect (eca-editor--flycheck-active-in-locus-p (current-buffer))
              :not :to-be-truthy)))

  (it "returns non-nil for a live buffer where flycheck-mode is t"
    (with-temp-buffer
      (setq-local flycheck-mode t)
      (expect (eca-editor--flycheck-active-in-locus-p (current-buffer))
              :to-be-truthy))))

(provide 'eca-editor-test)
;;; eca-editor-test.el ends here
