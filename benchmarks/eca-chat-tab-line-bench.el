;;; eca-chat-tab-line-bench.el --- Benchmark harness for ECA chat tabs -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Focused benchmark harness for ECA chat tab-line performance.
;;
;;  The general chat benchmark covers render hot paths.  This file
;;  isolates tab-list construction, tab label construction, and tab
;;  face selection across several chat counts.
;;
;;  Batch usage with Eask:
;;    eask emacs --batch -L . -l benchmarks/eca-chat-tab-line-bench.el \
;;      -f eca-chat-tab-line-bench-run
;;
;;  Optional profiler correlation report with Eask:
;;    eask emacs --batch -L . -l benchmarks/eca-chat-tab-line-bench.el \
;;      -f eca-chat-tab-line-bench-run-profile
;;
;;; Code:

(require 'benchmark)
(require 'cl-lib)
(require 'subr-x)

;; Prefer source files over stale byte-compiled files in local runs.
(setq load-prefer-newer t)

(defvar eca-chat-tab-line-bench-repo-root nil
  "Repository root that contains this benchmark file.")

(let* ((this-file (or load-file-name buffer-file-name))
       (bench-dir (and this-file (file-name-directory this-file)))
       (repo-root (and bench-dir
                       (file-name-directory
                        (directory-file-name bench-dir)))))
  (setq eca-chat-tab-line-bench-repo-root repo-root)
  (when repo-root
    (add-to-list 'load-path repo-root))
  (when bench-dir
    (add-to-list 'load-path bench-dir)))

(require 'eca-chat-bench)
(require 'tab-line nil t)

(declare-function profiler-start "profiler")
(declare-function profiler-stop "profiler")
(declare-function profiler-cpu-profile "profiler")
(declare-function profiler-write-profile "profiler")

;;;; Configuration

(defvar eca-chat-tab-line-bench-chat-counts '(1 10 50)
  "Number of chat buffers to use for tab-line benchmarks.")

(defvar eca-chat-tab-line-bench-turns-per-chat 20
  "Number of turns to put in each chat fixture.")

(defvar eca-chat-tab-line-bench-iters 2000
  "Number of iterations to use for tab-line benchmarks.")

(defvar eca-chat-tab-line-bench-profile-output-file nil
  "File path for profiler output, or nil to print it.")

(defvar eca-chat-tab-line-bench--results nil
  "Accumulated benchmark result entries.")

;;;; Fixture helpers

(defun eca-chat-tab-line-bench--git-output (&rest args)
  "Run git with ARGS and return trimmed output.
Return nil when git exits with a non-zero status."
  (with-temp-buffer
    (let ((status (apply #'process-file "git" nil t nil args)))
      (when (eq status 0)
        (string-trim (buffer-string))))))

(defun eca-chat-tab-line-bench--clear-cache (session)
  "Clear cached tab-line descriptors for SESSION when available.
This keeps the benchmark compatible with revisions that do not use
that cache."
  (when (boundp 'eca-chat--tab-line-cache-by-session)
    (let ((cache (symbol-value 'eca-chat--tab-line-cache-by-session)))
      (when (hash-table-p cache)
        (remhash session cache)))))

(defun eca-chat-tab-line-bench--configure-chat (session buffer id title pending)
  "Register BUFFER as chat ID with TITLE in SESSION.
When PENDING is non-nil, mark the pending approval cache true."
  (with-current-buffer buffer
    (setq-local eca-chat--id id)
    (setq-local eca-chat--title title)
    (setq-local eca-chat--pending-approvals-cache (and pending t))
    (setq-local eca-chat--chat-loading nil)
    (setq-local eca-chat--prompt-start-time
                (time-subtract (current-time) (seconds-to-time 125)))
    (eca-chat-bench--configure-context))
  (setf (eca--session-chats session)
        (eca-assoc (eca--session-chats session) id buffer)))

(defun eca-chat-tab-line-bench--make-buffer (session index turns pending)
  "Return one benchmark chat buffer for SESSION.
INDEX is used for the chat id and title.  TURNS controls fixture size.
PENDING marks the pending approval cache true when non-nil."
  (let* ((eca-chat-bench--session session)
         (buffer (eca-chat-bench--make-fixture turns))
         (id (format "tab-line-bench-%03d" index))
         (title (format "Tab-line bench chat %03d" index)))
    (eca-chat-tab-line-bench--configure-chat session buffer id title pending)
    buffer))

(defun eca-chat-tab-line-bench--call-with-fixtures
    (chat-count turns pending-step fn)
  "Create CHAT-COUNT chat fixtures and call FN.
Each chat has TURNS turns.  PENDING-STEP marks every nth chat as
pending when it is non-nil.  FN receives SESSION and BUFFERS."
  (let* ((session (eca-create-session (list default-directory)))
         (eca-chat-bench--session session)
         (buffers nil))
    (unwind-protect
        (progn
          (dotimes (i chat-count)
            (push (eca-chat-tab-line-bench--make-buffer
                   session i turns
                   (and pending-step (zerop (mod i pending-step))))
                  buffers))
          (setq buffers (nreverse buffers))
          (setf (eca--session-last-chat-buffer session) (car buffers))
          (funcall fn session buffers))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (eca-delete-session session)
      (setq eca-chat-bench--session nil))))

;;;; Timing and output

(defun eca-chat-tab-line-bench--time (label size turns iters thunk)
  "Run THUNK ITERS times and return a result plist.
LABEL, SIZE, and TURNS identify the workload."
  (garbage-collect)
  (let ((result (benchmark-call thunk iters)))
    (list :label label
          :size size
          :turns turns
          :iters iters
          :elapsed (nth 0 result)
          :gc-count (nth 1 result)
          :gc-elapsed (nth 2 result))))

(defun eca-chat-tab-line-bench--add-result (result)
  "Append RESULT to the benchmark result list."
  (setq eca-chat-tab-line-bench--results
        (append eca-chat-tab-line-bench--results (list result))))

(defun eca-chat-tab-line-bench--format-results ()
  "Return accumulated results as a markdown table."
  (concat
   "| op | chats | turns | iters | gc | gc-ms | wall-ms | per-call-us |\n"
   "|----|------:|------:|------:|---:|------:|--------:|------------:|\n"
   (mapconcat
    (lambda (result)
      (let* ((iters (plist-get result :iters))
             (elapsed (plist-get result :elapsed))
             (gc-elapsed (plist-get result :gc-elapsed))
             (per-us (if (> iters 0)
                         (/ (* elapsed 1000000.0) iters)
                       0.0)))
        (format "| %s | %d | %d | %d | %d | %.2f | %.2f | %.2f |"
                (plist-get result :label)
                (plist-get result :size)
                (plist-get result :turns)
                iters
                (plist-get result :gc-count)
                (* gc-elapsed 1000.0)
                (* elapsed 1000.0)
                per-us)))
    eca-chat-tab-line-bench--results
    "\n")
   "\n"))

(defun eca-chat-tab-line-bench--format-header ()
  "Return a markdown header for the benchmark run."
  (let* ((repo-root (or eca-chat-tab-line-bench-repo-root
                        default-directory))
         (repo-label (abbreviate-file-name
                      (directory-file-name (expand-file-name repo-root))))
         (commit (or (eca-chat-tab-line-bench--git-output
                      "-C" repo-root "rev-parse" "--short" "HEAD")
                     "unknown"))
         (branch (or (eca-chat-tab-line-bench--git-output
                      "-C" repo-root "branch" "--show-current")
                     "unknown")))
    (concat "# ECA chat tab-line benchmark\n\n"
            (format "- Repo: `%s`\n" repo-label)
            (format "- Branch: `%s`\n" branch)
            (format "- Commit: `%s`\n" commit)
            (format "- Emacs: `%s`\n" emacs-version)
            "- Workload: synthetic ECA chat buffers.\n"
            "- Purpose: tab-line performance.\n\n")))

;;;; Tab-line benchmarks

(defun eca-chat-tab-line-bench--bench-tabs-warm (session buffers chat-count turns)
  "Benchmark warm `eca-chat--tab-line-tabs' for SESSION and BUFFERS.
CHAT-COUNT and TURNS identify the fixture size."
  (with-current-buffer (car buffers)
    (setq-local eca--session-id-cache (eca--session-id session))
    (eca-chat--tab-line-tabs)
    (eca-chat-tab-line-bench--time
     'tabs-warm chat-count turns eca-chat-tab-line-bench-iters
     (lambda ()
       (eca-chat--tab-line-tabs)))))

(defun eca-chat-tab-line-bench--bench-tabs-rebuild (session buffers chat-count turns)
  "Benchmark invalidated `eca-chat--tab-line-tabs' for SESSION and BUFFERS.
CHAT-COUNT and TURNS identify the fixture size."
  (with-current-buffer (car buffers)
    (setq-local eca--session-id-cache (eca--session-id session))
    (eca-chat-tab-line-bench--time
     'tabs-rebuild chat-count turns eca-chat-tab-line-bench-iters
     (lambda ()
       (eca-chat-tab-line-bench--clear-cache session)
       (eca-chat--tab-line-tabs)))))

(defun eca-chat-tab-line-bench--bench-tab-name-last
    (_session buffers chat-count turns)
  "Benchmark `eca-chat--tab-line-tab-name' for BUFFERS.
CHAT-COUNT and TURNS identify the fixture size."
  (let ((target (car (last buffers))))
    (eca-chat-tab-line-bench--time
     'tab-name-last chat-count turns eca-chat-tab-line-bench-iters
     (lambda ()
       (eca-chat--tab-line-tab-name target)))))

(defun eca-chat-tab-line-bench--bench-tab-face-all
    (session buffers chat-count turns)
  "Benchmark `eca-chat--tab-line-face' for SESSION and BUFFERS.
CHAT-COUNT and TURNS identify the fixture size."
  (with-current-buffer (car buffers)
    (setq-local eca--session-id-cache (eca--session-id session))
    (let ((tabs (eca-chat--tab-line-tabs)))
      (eca-chat-tab-line-bench--time
       'tab-face-all chat-count turns eca-chat-tab-line-bench-iters
       (lambda ()
         (dolist (tab tabs)
           (eca-chat--tab-line-face tab tabs 'tab-line-tab nil
                                    (cdr (assq 'buffer tab)))))))))

(defun eca-chat-tab-line-bench--run-tab-line ()
  "Run tab-line benchmarks for configured chat counts."
  (dolist (chat-count eca-chat-tab-line-bench-chat-counts)
    (message "[eca-chat-tab-line-bench] running chats=%d ..." chat-count)
    (eca-chat-tab-line-bench--call-with-fixtures
     chat-count eca-chat-tab-line-bench-turns-per-chat 5
     (lambda (session buffers)
       (eca-chat-tab-line-bench--add-result
        (eca-chat-tab-line-bench--bench-tabs-warm
         session buffers chat-count eca-chat-tab-line-bench-turns-per-chat))
       (eca-chat-tab-line-bench--add-result
        (eca-chat-tab-line-bench--bench-tabs-rebuild
         session buffers chat-count eca-chat-tab-line-bench-turns-per-chat))
       (eca-chat-tab-line-bench--add-result
        (eca-chat-tab-line-bench--bench-tab-name-last
         session buffers chat-count eca-chat-tab-line-bench-turns-per-chat))
       (eca-chat-tab-line-bench--add-result
        (eca-chat-tab-line-bench--bench-tab-face-all
         session buffers chat-count eca-chat-tab-line-bench-turns-per-chat))))))

;;;; Driver

;;;###autoload
(defun eca-chat-tab-line-bench-run ()
  "Run chat tab-line benchmarks and print markdown results."
  (interactive)
  (setq eca-chat-tab-line-bench--results nil)
  (eca-chat-tab-line-bench--run-tab-line)
  (let ((report (concat (eca-chat-tab-line-bench--format-header)
                        (eca-chat-tab-line-bench--format-results))))
    (if noninteractive
        (princ report)
      (with-current-buffer (get-buffer-create "*eca-chat-tab-line-bench*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert report))
        (display-buffer (current-buffer))))
    report))

;;;###autoload
(defun eca-chat-tab-line-bench-run-profile ()
  "Run the benchmark with Emacs CPU profiler enabled."
  (interactive)
  (require 'profiler)
  (profiler-start 'cpu)
  (unwind-protect
      (eca-chat-tab-line-bench-run)
    (profiler-stop))
  (let ((profile (profiler-cpu-profile)))
    (if eca-chat-tab-line-bench-profile-output-file
        (profiler-write-profile
         profile eca-chat-tab-line-bench-profile-output-file nil)
      (prin1 profile))))

(provide 'eca-chat-tab-line-bench)
;;; eca-chat-tab-line-bench.el ends here
