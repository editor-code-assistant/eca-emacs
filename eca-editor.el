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

(require 'eca-util)

(declare-function lsp-diagnostics "lsp-mode" (current-workspace?))
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
    ('flymake-error "error")
    (':error "error")
    ('flymake-warning "warning")
    (':warning "warning")
    ('flymake-note "information")
    (':note "information")
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
                                        :code (lsp-get it :code)
                                        :range (list :start (list :line (lsp-get start-r :line)
                                                                  :character (lsp-get start-r :character))
                                                     :end (list :line (lsp-get end-r :line)
                                                                :character (lsp-get end-r :character)))
                                        :source (lsp-get it :source)
                                        :message (lsp-get it :message))
                                  eca-diagnostics))
                          lsp-diagnostics)))
               diagnostics))
    eca-diagnostics))

(defun eca-editor--flymake-diagnostics (_uri workspace)
  "Find all flymake diagnostics found for WORKSPACE.
If URI is nil find all diagnostics otherwise filter to that uri."
  (with-current-buffer (find-file-noselect workspace)
    (--map
     (with-current-buffer (flymake-diagnostic-buffer it)
       (save-excursion
         (let* ((beg (flymake-diagnostic-beg it))
                (end (flymake-diagnostic-end it))
                (beg-line (progn (goto-char beg)
                                 (line-number-at-pos)))
                (beg-col (current-column))
                (end-line (progn (goto-char end)
                                 (line-number-at-pos)))
                (end-col (current-column)))
           (list :uri (eca--path-to-uri (buffer-file-name (flymake-diagnostic-buffer it)))
                 :severity (eca-editor--flymake-to-eca-severity (flymake-diagnostic-type it))
                 :code (flymake-diagnostic-code it)
                 :range (list :start (list :line beg-line
                                           :character beg-col)
                              :end (list :line end-line
                                         :character end-col))
                 :source (flymake-diagnostic-backend it)
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
