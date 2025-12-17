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
(require 'ht nil t)

(require 'eca-util)

(declare-function lsp-diagnostics "lsp-mode" (current-workspace?))
(declare-function lsp-get "lsp-mode" (from key))
(declare-function ht-select "ht" (function table))
(declare-function flymake-diagnostic-code "flymake" (diagnostic))

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
       (when (or (null uri) (string= uri diag-uri))
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

(defun eca-editor--get-lsp-diagnostics (uri workspaces)
  "Return lsp diagnostics for URI in WORKSPACES.
If URI is nil, return all workspaces diagnostics."
  (let ((eca-diagnostics '()))
    (seq-doseq (workspace workspaces)
      (cond
       ((featurep 'lsp-mode) (setq eca-diagnostics (append eca-diagnostics (eca-editor--lsp-mode-diagnostics uri workspace))))
       ((featurep 'flymake) (setq eca-diagnostics (append eca-diagnostics (eca-editor--flymake-diagnostics uri workspace))))
       (t nil)))
    eca-diagnostics))

(defun eca-editor-get-diagnostics (session params)
  "Return all diagnostics for SESSION from PARAMS."
  (let* ((uri (plist-get params :uri))
         (lsp-diags (eca-editor--get-lsp-diagnostics uri (eca--session-workspace-folders session))))
    (list :diagnostics (vconcat lsp-diags))))

(provide 'eca-editor)
;;; eca-editor.el ends here
