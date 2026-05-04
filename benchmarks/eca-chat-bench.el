;;; eca-chat-bench.el --- Benchmark harness for eca-chat -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Benchmark harness for the ECA chat buffer rendering hot paths.
;;  Builds a synthetic chat buffer with N turns of mixed content
;;  (text, code blocks, markdown tables, tool-call expandables) and
;;  times the operations that scale with chat length.
;;
;;  Usage:
;;    Interactive: M-x eca-chat-bench-run
;;    Batch:       emacs -Q --batch -L . -l benchmarks/eca-chat-bench.el \
;;                       -f eca-chat-bench-run
;;
;;  Output: prints a markdown table with op, size, iters, gc, wall-ms,
;;  per-call-µs.  When run in batch the table goes to standard output.
;;
;;  The harness avoids any live ECA server: it constructs a session
;;  via `eca-create-session', binds `eca--chat-init-session', and
;;  drives `eca-chat-mode' directly.  The buffer's overlays, text
;;  properties and faces are built using the same helpers production
;;  uses, so per-turn overlay/property counts match real chats.
;;
;;
;;  Baseline (before optimizations).  Captured via:
;;    eask emacs --batch -l benchmarks/eca-chat-bench.el -f eca-chat-bench-run
;;
;;  | op                     | size | iters  | gc | wall-ms  | per-call-µs |
;;  |------------------------|-----:|-------:|---:|---------:|------------:|
;;  | insertion-point-lookup |   10 |  10000 |  0 |    38.79 |        3.88 |
;;  | expandable-lookup      |   10 |  10000 |  0 |    28.29 |        2.83 |
;;  | expandable-update      |   10 |    100 |  0 |     1.10 |       11.01 |
;;  | stream-chunk           |   10 |   1000 |  0 |     7.43 |        7.43 |
;;  | end-of-stream          |   10 |      5 |  1 |   174.64 |    34928.88 |
;;  | insertion-point-lookup |   50 |  10000 |  2 |   167.59 |       16.76 |
;;  | expandable-lookup      |   50 |  10000 |  2 |   175.07 |       17.51 |
;;  | expandable-update      |   50 |    100 |  0 |     2.19 |       21.93 |
;;  | stream-chunk           |   50 |   1000 |  0 |    18.70 |       18.70 |
;;  | end-of-stream          |   50 |      5 |  5 |   869.10 |   173820.33 |
;;  | insertion-point-lookup |  200 |  10000 |  8 |   650.84 |       65.08 |
;;  | expandable-lookup      |  200 |  10000 |  8 |   668.42 |       66.84 |
;;  | expandable-update      |  200 |    100 |  0 |     6.79 |       67.93 |
;;  | stream-chunk           |  200 |   1000 |  0 |    60.36 |       60.36 |
;;  | end-of-stream          |  200 |      5 | 15 |  6094.75 |  1218949.82 |
;;  | insertion-point-lookup |  500 |  10000 | 17 |  1948.76 |      194.88 |
;;  | expandable-lookup      |  500 |  10000 | 17 |  3240.16 |      324.02 |
;;  | expandable-update      |  500 |    100 |  0 |    43.63 |      436.31 |
;;  | stream-chunk           |  500 |   1000 |  1 |   426.44 |      426.44 |
;;  | end-of-stream          |  500 |      5 | 27 | 12930.83 |  2586165.50 |
;;
;;  Observations:
;;  - insertion-point-lookup, expandable-lookup, stream-chunk, expandable-update
;;    all scale linearly with chat size due to `(overlays-in (point-min)
;;    (point-max))` scans inside the singleton-overlay accessors and the
;;    expandable-id lookup.
;;  - end-of-stream cost is dominated by table align+beautify scanning from
;;    point-min on every assistant turn — at size 500 a single end-of-stream
;;    takes ~2.6 seconds (175ms × ~14 tables = roughly the observed total).
;;
;;
;;  After Tier 1 (singleton-overlay cache + expandable-id hash) and
;;  Tier 2 (region-scoped font-lock + region-scoped end-of-stream
;;  table align/beautify):
;;
;;  | op                     | size | iters  | gc | wall-ms  | per-call-µs |
;;  |------------------------|-----:|-------:|---:|---------:|------------:|
;;  | insertion-point-lookup |   10 |  10000 |  0 |     6.83 |        0.68 |
;;  | expandable-lookup      |   10 |  10000 |  0 |     4.15 |        0.42 |
;;  | expandable-update      |   10 |    100 |  0 |     1.34 |       13.45 |
;;  | stream-chunk           |   10 |   1000 |  0 |     4.96 |        4.96 |
;;  | end-of-stream          |   10 |      5 |  0 |    19.61 |     3921.33 |
;;  | insertion-point-lookup |   50 |  10000 |  0 |     5.18 |        0.52 |
;;  | expandable-lookup      |   50 |  10000 |  0 |     4.02 |        0.40 |
;;  | expandable-update      |   50 |    100 |  0 |     1.35 |       13.50 |
;;  | stream-chunk           |   50 |   1000 |  0 |     5.59 |        5.59 |
;;  | end-of-stream          |   50 |      5 |  0 |    26.05 |     5209.82 |
;;  | insertion-point-lookup |  200 |  10000 |  0 |     5.41 |        0.54 |
;;  | expandable-lookup      |  200 |  10000 |  0 |     4.11 |        0.41 |
;;  | expandable-update      |  200 |    100 |  0 |     1.48 |       14.82 |
;;  | stream-chunk           |  200 |   1000 |  0 |     5.84 |        5.84 |
;;  | end-of-stream          |  200 |      5 |  0 |    28.45 |     5689.84 |
;;  | insertion-point-lookup |  500 |  10000 |  0 |     5.38 |        0.54 |
;;  | expandable-lookup      |  500 |  10000 |  0 |     4.06 |        0.41 |
;;  | expandable-update      |  500 |    100 |  0 |     1.58 |       15.75 |
;;  | stream-chunk           |  500 |   1000 |  0 |     6.01 |        6.01 |
;;  | end-of-stream          |  500 |      5 |  0 |    47.21 |     9442.26 |
;;
;;  Speedups at size 500 (baseline → after):
;;    insertion-point-lookup : 194.88 µs →   0.54 µs (361×)
;;    expandable-lookup      : 324.02 µs →   0.41 µs (790×)
;;    expandable-update      : 436.31 µs →  15.75 µs ( 28×)
;;    stream-chunk           : 426.44 µs →   6.01 µs ( 71×)
;;    end-of-stream          :  2586 ms  →   9.44 ms (274×)
;;
;;  All per-call ops are now flat across chat sizes (10..500); the
;;  remaining minor end-of-stream growth tracks the size of the
;;  current turn (not the chat history) since `font-lock-ensure' and
;;  the table align/beautify are region-scoped from
;;  `eca-chat--last-user-message-pos'.
;;
;;
;;; Code:

(require 'benchmark)
(require 'cl-lib)

;; Always prefer freshly-edited source files over stale byte-compiled
;; ones in the repo root, which would otherwise mask uncommitted edits.
(setq load-prefer-newer t)

;; Allow loading from the repo root regardless of cwd.
(let ((this-file (or load-file-name buffer-file-name)))
  (when this-file
    (add-to-list 'load-path (file-name-directory
                             (directory-file-name
                              (file-name-directory this-file))))))

(require 'eca-util)
(require 'eca-chat)

;;;; Configuration

(defvar eca-chat-bench-sizes '(10 50 200 500)
  "Chat sizes (turn count) to benchmark.")

(defvar eca-chat-bench-iters
  '((insertion-point-lookup . 10000)
    (expandable-lookup      . 10000)
    (expandable-update      . 100)
    (stream-chunk           . 1000)
    (end-of-stream          . 5))
  "Default iteration counts per benchmark op.")

;;;; Fixture

(defvar eca-chat-bench--session nil
  "The session shared across the benchmark run.")

(defun eca-chat-bench--ensure-session ()
  "Return a benchmark session, creating it on first call."
  (or eca-chat-bench--session
      (setq eca-chat-bench--session
            (eca-create-session (list default-directory)))))

(defun eca-chat-bench--sample-table ()
  "Return a small markdown table string."
  (concat "\n"
          "| col1 | col2 | col3 |\n"
          "|------|------|------|\n"
          "| a    | b    | c    |\n"
          "| dd   | ee   | ff   |\n"
          "| ggg  | hhh  | iii  |\n\n"))

(defun eca-chat-bench--sample-code-block ()
  "Return a small markdown code block string."
  (concat "\n```clojure\n"
          "(defn hello [x]\n"
          "  (println \"hi\" x))\n"
          "```\n"))

(defun eca-chat-bench--seed-turn (i)
  "Insert a fake turn I in the current chat buffer.
Adds an echoed user message expandable, mixed assistant text
\(short prose, code block, table) and one tool-call expandable.
Caller must ensure `inhibit-read-only' is set."
  (let* ((user-id (format "user-%d" i))
         (tool-id (format "tool-%d" i))
         (turn-text
          (concat (format "\nThis is assistant turn #%d. " i)
                  "Some lorem ipsum dolor sit amet text to fill in. "
                  "Aliquam tincidunt mauris eu risus. "
                  (eca-chat-bench--sample-code-block)
                  (eca-chat-bench--sample-table)
                  "\n")))
    ;; User echo (uses expandable user-message overlay path)
    (eca-chat--add-expandable-content
     user-id
     (propertize (format "User message %d about benchmarking and ECA." i)
                 'font-lock-face 'eca-chat-user-messages-face)
     "Rollback chat to before this message")
    (eca-chat--mark-header)
    ;; Assistant streaming text
    (eca-chat--add-text-content turn-text)
    ;; Tool call expandable (mimics toolCallRun result)
    (eca-chat--add-expandable-content
     tool-id
     (propertize (format "Calling tool: bench__op%d" i)
                 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
     (concat "Tool: bench\n"
             "Server: local\n"
             "Arguments: {\"i\": " (number-to-string i) "}\n"))))

(defun eca-chat-bench--safe-face-background (orig &rest args)
  "Advice around `face-background': return nil for the `unspecified-bg' sentinel.
In batch mode without a frame `face-background' returns the literal
string \"unspecified-bg\" rather than nil, which breaks downstream
callers like `eca-chat--update-expandable-block-faces' that pass the
result to `color-lighten-name'.  Treat the sentinel as nil so those
guards short-circuit cleanly."
  (let ((res (apply orig args)))
    (if (and (stringp res)
             (or (string= res "unspecified-bg")
                 (string= res "unspecified-fg")))
        nil
      res)))

(defun eca-chat-bench--make-fixture (n-turns)
  "Build a chat buffer pre-populated with N-TURNS turns.
Returns the buffer; caller is responsible for killing it."
  (let* ((session (eca-chat-bench--ensure-session))
         (buf (generate-new-buffer (format "*eca-bench-%d*" n-turns))))
    (advice-add 'face-background :around #'eca-chat-bench--safe-face-background)
    (unwind-protect
        (with-current-buffer buf
          ;; Disable UI features whose setup is deferred via timers (won't
          ;; fire in batch) and which would otherwise pollute timing.
          (let ((eca--chat-init-session session)
                (eca-chat-tab-line nil)
                (eca-chat-override-mode-line nil))
            (eca-chat-mode)
            (setq-local eca-chat--id (format "bench-%d" n-turns))
            (let ((inhibit-read-only t))
              (dotimes (i n-turns)
                (eca-chat-bench--seed-turn i)))))
      (advice-remove 'face-background #'eca-chat-bench--safe-face-background))
    buf))

;;;; Timing primitives

(defun eca-chat-bench--time (label thunk iters)
  "Run THUNK ITERS times and return a result plist tagged with LABEL.
Forces a GC before the run to reduce measurement noise."
  (garbage-collect)
  (let ((result (benchmark-call thunk iters)))
    (list :label label
          :iters iters
          :elapsed (nth 0 result)
          :gc-count (nth 1 result)
          :gc-elapsed (nth 2 result))))

;;;; Individual benchmarks

(defun eca-chat-bench--bench-insertion-point-lookup (buf iters)
  "Time `eca-chat--content-insertion-point' ITERS times in BUF."
  (with-current-buffer buf
    (eca-chat-bench--time
     'insertion-point-lookup
     (lambda () (eca-chat--content-insertion-point))
     iters)))

(defun eca-chat-bench--bench-expandable-lookup (buf iters n-turns)
  "Time `eca-chat--get-expandable-content' ITERS times in BUF.
Looks up the last tool-call id (`tool-(N-1)`)."
  (with-current-buffer buf
    (let ((id (format "tool-%d" (max 0 (1- n-turns)))))
      (eca-chat-bench--time
       'expandable-lookup
       (lambda () (eca-chat--get-expandable-content id))
       iters))))

(defun eca-chat-bench--bench-expandable-update (buf iters n-turns)
  "Time `eca-chat--update-expandable-content' ITERS times in BUF.
Updates the last tool-call expandable with a small body."
  (with-current-buffer buf
    (let ((id (format "tool-%d" (max 0 (1- n-turns)))))
      (eca-chat-bench--time
       'expandable-update
       (lambda ()
         (let ((inhibit-read-only t))
           (eca-chat--update-expandable-content
            id
            (propertize "Updated label" 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
            "Updated body content for benchmarking.")))
       iters))))

(defun eca-chat-bench--bench-stream-chunk (buf iters)
  "Time `eca-chat--add-text-content' with a small chunk ITERS times in BUF.
Mimics the streaming-assistant-text hot path: each iteration
re-resolves the insertion point and inserts a few chars."
  (with-current-buffer buf
    (eca-chat-bench--time
     'stream-chunk
     (lambda ()
       (let ((inhibit-read-only t))
         (eca-chat--add-text-content "x")))
     iters)))

(defun eca-chat-bench--bench-end-of-stream (buf iters)
  "Time the end-of-stream finalize block ITERS times in BUF.
Mirrors the production sequence: a region-scoped `font-lock-ensure'
from `eca-chat--last-user-message-pos' to `point-max', followed by
`eca-chat--align-tables' / `eca-chat--beautify-tables' (which both
default to scanning from `eca-chat--last-user-message-pos')."
  (with-current-buffer buf
    (eca-chat-bench--time
     'end-of-stream
     (lambda ()
       (let ((inhibit-read-only t))
         (font-lock-ensure (or eca-chat--last-user-message-pos (point-min))
                           (point-max))
         (eca-chat--align-tables)
         (eca-chat--beautify-tables)))
     iters)))

;;;; Driver

(defvar eca-chat-bench--results nil
  "Accumulated benchmark results as a list of (size . plist).")

(defun eca-chat-bench--reset ()
  "Reset benchmark state."
  (setq eca-chat-bench--results nil))

(defun eca-chat-bench--add-result (size result)
  "Record RESULT for fixture SIZE."
  (push (cons size result) eca-chat-bench--results))

(defun eca-chat-bench--format-results ()
  "Return a markdown table built from `eca-chat-bench--results'."
  (let ((rows (reverse eca-chat-bench--results)))
    (concat
     "| op                     | size | iters  | gc | wall-ms  | per-call-µs |\n"
     "|------------------------|-----:|-------:|---:|---------:|------------:|\n"
     (mapconcat
      (lambda (entry)
        (let* ((size (car entry))
               (r (cdr entry))
               (label (symbol-name (plist-get r :label)))
               (iters (plist-get r :iters))
               (elapsed (plist-get r :elapsed))
               (gc (plist-get r :gc-count))
               (wall-ms (* elapsed 1000.0))
               (per-us (if (> iters 0)
                           (/ (* elapsed 1000000.0) iters)
                         0.0)))
          (format "| %-22s | %4d | %6d | %2d | %8.2f | %11.2f |"
                  label size iters gc wall-ms per-us)))
      rows
      "\n")
     "\n")))

(defun eca-chat-bench-run-size (size)
  "Build a SIZE-turn fixture and run all benchmarks against it.
Each benchmark uses a fresh fixture so destructive ops do not
contaminate later measurements."
  (let ((iters eca-chat-bench-iters))
    ;; Read-only benches share one fixture for speed.
    (let ((buf (eca-chat-bench--make-fixture size)))
      (unwind-protect
          (progn
            (eca-chat-bench--add-result
             size (eca-chat-bench--bench-insertion-point-lookup
                   buf (alist-get 'insertion-point-lookup iters)))
            (eca-chat-bench--add-result
             size (eca-chat-bench--bench-expandable-lookup
                   buf (alist-get 'expandable-lookup iters) size)))
        (kill-buffer buf)))
    ;; Mutating benches each get a fresh fixture so they measure the
    ;; same starting state.
    (let ((buf (eca-chat-bench--make-fixture size)))
      (unwind-protect
          (eca-chat-bench--add-result
           size (eca-chat-bench--bench-expandable-update
                 buf (alist-get 'expandable-update iters) size))
        (kill-buffer buf)))
    (let ((buf (eca-chat-bench--make-fixture size)))
      (unwind-protect
          (eca-chat-bench--add-result
           size (eca-chat-bench--bench-stream-chunk
                 buf (alist-get 'stream-chunk iters)))
        (kill-buffer buf)))
    (let ((buf (eca-chat-bench--make-fixture size)))
      (unwind-protect
          (eca-chat-bench--add-result
           size (eca-chat-bench--bench-end-of-stream
                 buf (alist-get 'end-of-stream iters)))
        (kill-buffer buf)))))

;;;###autoload
(defun eca-chat-bench-run ()
  "Run all benchmarks at all sizes; print a markdown table.
When run interactively, the table is shown in a results buffer.
When run in batch (e.g. `emacs -Q --batch -f eca-chat-bench-run'),
the table is printed to standard output."
  (interactive)
  (eca-chat-bench--reset)
  (dolist (size eca-chat-bench-sizes)
    (message "[eca-chat-bench] running size=%d ..." size)
    (eca-chat-bench-run-size size))
  (let ((table (eca-chat-bench--format-results)))
    (cond
     (noninteractive
      (princ "\n")
      (princ table)
      (princ "\n"))
     (t
      (with-current-buffer (get-buffer-create "*eca-chat-bench*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert table))
        (display-buffer (current-buffer)))))
    table))

(provide 'eca-chat-bench)
;;; eca-chat-bench.el ends here
