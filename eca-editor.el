;;; eca-editor.el --- ECA (Editor Code Assistant) editor -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) editor.
;;
;;; Code:

(require 'dash)
(require 'f)

(require 'lsp-mode nil t)
(require 'flymake nil t)
(require 'flycheck nil t)
(require 'ht nil t)

(require 'eca-util)

(declare-function lsp-diagnostics "lsp-mode" (current-workspace?))
(declare-function lsp-get "lsp-mode" (from key))
(declare-function ht-select "ht" (function table))
(declare-function flymake-diagnostic-code "flymake" (diagnostic))
(declare-function flycheck-running-p "flycheck" ())
(declare-function flycheck-buffer "flycheck" ())
(declare-function flycheck-error-filename "flycheck" (err))
(declare-function flycheck-error-line "flycheck" (err))
(declare-function flycheck-error-column "flycheck" (err))
(declare-function flycheck-error-end-line "flycheck" (err))
(declare-function flycheck-error-end-column "flycheck" (err))
(declare-function flycheck-error-level "flycheck" (err))
(declare-function flycheck-error-message "flycheck" (err))
(declare-function flycheck-error-id "flycheck" (err))
(declare-function flycheck-error-checker "flycheck" (err))

(defun eca-editor--lsp-to-eca-severity (severity)
  "Convert lsp SEVERITY to eca one."
  (pcase severity
    (1 "error")
    (2 "warning")
    (3 "info")
    (4 "hint")
    (_ "unknown")))

(defun eca-editor--flymake-to-eca-severity (severity)
  "Convert flymake SEVERITY to eca one."
  (pcase severity
    ((or 'flymake-error ':error 'eglot-error) "error")
    ((or 'flymake-warning ':warning 'eglot-warning) "warning")
    ((or 'flymake-note ':note 'eglot-note) "information")
    (_ "unknown")))

(defun eca-editor--flycheck-active-in-locus-p (locus)
  "Return non-nil if LOCUS is a live buffer with `flycheck-mode' on.
Used to suppress flymake diagnostics for buffers where flycheck is
already the active diagnostic surface, mirroring how the flycheck
branch suppresses its own output in lsp-managed buffers.  The
resulting precedence is: lsp-mode > flycheck > flymake."
  (and (buffer-live-p locus)
       (with-current-buffer locus
         (bound-and-true-p flycheck-mode))))

(defun eca-editor--flycheck-to-eca-severity (level)
  "Convert flycheck LEVEL to eca severity."
  (pcase level
    ('error "error")
    ('warning "warning")
    ('info "information")
    (_ "unknown")))

(defun eca-editor--lsp-mode-diagnostics (uri workspace)
  "Find all `lsp-mode` diagnostics found for WORKSPACE.
If URI is nil find all diagnostics otherwise filter to that uri."
  (let* ((eca-diagnostics '())
         (all-diagnostics (car (--keep (when (and (buffer-file-name it)
                                                  (f-ancestor-of? workspace (buffer-file-name it)))
                                         (with-current-buffer it
                                           (lsp-diagnostics t)))
                                       (buffer-list))))
         (diagnostics (if uri
                          (ht-select (lambda (k _v) (string= k (eca--uri-to-path uri))) all-diagnostics)
                        all-diagnostics)))
    (when diagnostics
      (maphash (lambda (path lsp-diagnostics)
                 (let ((uri (eca--path-to-uri path)))
                   (--map (let* ((range (lsp-get it :range))
                                 (start-r (lsp-get range :start))
                                 (end-r (lsp-get range :end)))
                            (push (list :uri uri
                                        :severity (eca-editor--lsp-to-eca-severity (lsp-get it :severity))
                                        :code (when-let ((code (lsp-get it :code)))
                                                (if (symbolp code)
                                                    (symbol-name code)
                                                  (format "%s" code)))
                                        :range (list :start (list :line (lsp-get start-r :line)
                                                                  :character (lsp-get start-r :character))
                                                     :end (list :line (lsp-get end-r :line)
                                                                :character (lsp-get end-r :character)))
                                        :source (when-let ((source (lsp-get it :source)))
                                                  (if (symbolp source)
                                                      (symbol-name source)
                                                    (format "%s" source)))
                                        :message (lsp-get it :message))
                                  eca-diagnostics))
                          lsp-diagnostics)))
               diagnostics))
    eca-diagnostics))

(defun eca-editor--flymake-diagnostics (uri workspace)
  "Find all flymake diagnostics found for WORKSPACE.
If URI is nil find all diagnostics otherwise filter to that uri."
  (let ((default-directory workspace))
    (--keep
     (let* ((locus (flymake-diagnostic-buffer it))
            (buffer-p (bufferp locus))
            (file-path (if buffer-p (buffer-file-name locus) locus))
            (diag-uri (eca--path-to-uri file-path))
            (beg (flymake-diagnostic-beg it))
            (end (flymake-diagnostic-end it)))
       (when (and (or (null uri) (string= uri diag-uri))
                  (not (eca-editor--flycheck-active-in-locus-p locus)))
         (let ((range (if buffer-p
                          (with-current-buffer locus
                            (save-excursion
                              (list :start (list :line (progn (goto-char beg) (line-number-at-pos))
                                                 :character (current-column))
                                    :end (list :line (progn (goto-char (or end beg)) (line-number-at-pos))
                                               :character (current-column)))))
                        ;; file path locus - beg/end are already (line . col)
                        (list :start (list :line (car beg) :character (cdr beg))
                              :end (list :line (if end (car end) (car beg))
                                         :character (if end (cdr end) (cdr beg)))))))
           (list :uri diag-uri
                 :severity (eca-editor--flymake-to-eca-severity (flymake-diagnostic-type it))
                 :code (when-let ((code (flymake-diagnostic-code it)))
                         (if (symbolp code) (symbol-name code) (format "%s" code)))
                 :range range
                 :source (when-let ((backend (flymake-diagnostic-backend it)))
                           (if (symbolp backend) (symbol-name backend) (format "%s" backend)))
                 :message (flymake-diagnostic-text it)))))
     (flymake--project-diagnostics))))

(defun eca-editor--flycheck-diagnostics (uri workspaces)
  "Collect flycheck diagnostics from buffers in WORKSPACES.
Skips buffers where `lsp-mode' is active to avoid duplicates.
If URI is non-nil, filter to that uri."
  (let ((diagnostics '()))
    (dolist (buf (buffer-list))
      (when-let ((file (buffer-file-name buf)))
        (let ((file-uri (eca--path-to-uri file)))
          (when (and (or (null uri) (string= uri file-uri))
                     (seq-some (lambda (ws)
                                 (f-ancestor-of? ws file))
                               workspaces))
            (with-current-buffer buf
              (when (and (bound-and-true-p flycheck-mode)
                         (not (bound-and-true-p lsp-mode)))
                (dolist (err flycheck-current-errors)
                  (let* ((err-file (or (flycheck-error-filename err)
                                       file))
                         (err-uri (eca--path-to-uri err-file))
                         (line (flycheck-error-line err))
                         (col (flycheck-error-column err))
                         (end-line (flycheck-error-end-line err))
                         (end-col (flycheck-error-end-column err))
                         (start-line (max 0 (1- (or line 1))))
                         (start-char (max 0 (1- (or col 1))))
                         (e-line (max 0 (1- (or end-line line 1))))
                         (e-char (max 0 (1- (or end-col col 1)))))
                    (push (list :uri err-uri
                                :severity (eca-editor--flycheck-to-eca-severity
                                           (flycheck-error-level err))
                                :code (when-let ((id (flycheck-error-id err)))
                                        (format "%s" id))
                                :range (list :start (list :line start-line
                                                          :character start-char)
                                             :end (list :line e-line
                                                        :character e-char))
                                :source (when-let ((checker (flycheck-error-checker err)))
                                          (symbol-name checker))
                                :message (flycheck-error-message err))
                          diagnostics)))))))))
    diagnostics))

(defun eca-editor--wait-for-flycheck (timeout)
  "Wait for flycheck to finish, up to TIMEOUT seconds."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (flycheck-running-p)
                (< (float-time) deadline))
      (sit-for 0.2))))

(defun eca-editor--ensure-diagnostics-fresh (uri)
  "Ensure diagnostics for URI are fresh.
Visits the buffer if needed, reverts if the file
changed on disk, and waits for checkers to finish."
  (when uri
    (let* ((file (eca--uri-to-path uri))
           (buf (or (find-buffer-visiting file)
                    (find-file-noselect file))))
      (with-current-buffer buf
        ;; Revert if file on disk is newer than buffer
        (when (and (buffer-file-name)
                   (not (verify-visited-file-modtime buf)))
          (revert-buffer t t t))
        (font-lock-ensure)
        (cond
         ;; lsp-mode: diagnostics are pushed async by
         ;; the server, we can only wait.
         ((bound-and-true-p lsp-mode)
          (sit-for 2))
         ;; flycheck: trigger and poll for completion
         ((bound-and-true-p flycheck-mode)
          (flycheck-buffer)
          (eca-editor--wait-for-flycheck 3.0))
         ;; flymake: trigger and wait
         ((bound-and-true-p flymake-mode)
          (flymake-start)
          (sit-for 2))
         ;; fallback: wait for mode hooks to settle
         (t
          (sit-for 2)))))))

(defun eca-editor--collect-all-diagnostics (uri workspaces)
  "Collect diagnostics from all backends for WORKSPACES.
If URI is non-nil, filter to that uri."
  (let ((diagnostics '()))
    ;; lsp-mode: workspace-level diagnostics
    (when (featurep 'lsp-mode)
      (seq-doseq (workspace workspaces)
        (setq diagnostics
              (append diagnostics
                      (eca-editor--lsp-mode-diagnostics
                       uri workspace)))))
    ;; flycheck: buffer-level, non-LSP buffers only
    (when (featurep 'flycheck)
      (setq diagnostics
            (append diagnostics
                    (eca-editor--flycheck-diagnostics
                     uri workspaces))))
    ;; flymake: for eglot users (when lsp-mode is absent)
    (when (and (featurep 'flymake)
               (not (featurep 'lsp-mode)))
      (seq-doseq (workspace workspaces)
        (setq diagnostics
              (append diagnostics
                      (eca-editor--flymake-diagnostics
                       uri workspace)))))
    diagnostics))

(defun eca-editor-get-diagnostics (session params)
  "Return all diagnostics for SESSION from PARAMS."
  (let* ((uri (plist-get params :uri))
         (workspaces (eca--session-workspace-folders session)))
    (eca-editor--ensure-diagnostics-fresh uri)
    (list :diagnostics
          (vconcat (eca-editor--collect-all-diagnostics
                    uri workspaces)))))

(provide 'eca-editor)
;;; eca-editor.el ends here
