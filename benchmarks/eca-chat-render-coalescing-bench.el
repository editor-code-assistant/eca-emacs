;;; eca-chat-render-coalescing-bench.el --- Render coalescing bench -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Focused benchmark harness for chat render coalescing work.
;;
;;  This benchmark targets high-frequency render paths that can benefit
;;  from batching or coalescing:
;;
;;  - top-level assistant text
;;  - subagent assistant text
;;  - toolCallPrepare argument updates
;;  - mixed text, prepare, and lifecycle events
;;  - history replay
;;
;;  Batch usage with Eask:
;;    eask emacs --batch -L . \
;;      -l benchmarks/eca-chat-render-coalescing-bench.el \
;;      -f eca-chat-render-coalescing-bench-run
;;
;;; Code:

(require 'benchmark)
(require 'cl-lib)
(require 'subr-x)

;; Prefer source files over stale byte-compiled files in local runs.
(setq load-prefer-newer t)

(defvar eca-chat-render-coalescing-bench-repo-root nil
  "Repository root that contains this benchmark file.")

(let* ((this-file (or load-file-name buffer-file-name))
       (bench-dir (and this-file (file-name-directory this-file)))
       (repo-root (and bench-dir
                       (file-name-directory
                        (directory-file-name bench-dir)))))
  (setq eca-chat-render-coalescing-bench-repo-root repo-root)
  (when repo-root
    (add-to-list 'load-path repo-root))
  (when bench-dir
    (add-to-list 'load-path bench-dir)))

(require 'eca-chat-bench)

;;;; Configuration

(defvar eca-chat-render-coalescing-bench-base-turns 25
  "Number of seed turns in the synthetic chat buffer.")

(defvar eca-chat-render-coalescing-bench-text-chunks 1500
  "Number of text chunks in stream benchmark cases.")

(defvar eca-chat-render-coalescing-bench-prepare-chunks 600
  "Number of argument chunks in toolCallPrepare benchmark cases.")

(defvar eca-chat-render-coalescing-bench-history-groups 80
  "Number of generated groups in the history replay benchmark.")

(defvar eca-chat-render-coalescing-bench-parent-count 4
  "Number of parent subagent tool calls for split-parent benchmarks.")

(defvar eca-chat-render-coalescing-bench--results nil
  "Accumulated benchmark result plists.")

;;;; Metadata

(defun eca-chat-render-coalescing-bench--git-output (&rest args)
  "Run git with ARGS and return trimmed output.
Return nil when git exits with a non-zero status."
  (when eca-chat-render-coalescing-bench-repo-root
    (with-temp-buffer
      (let* ((default-directory eca-chat-render-coalescing-bench-repo-root)
             (status (apply #'process-file "git" nil t nil args)))
        (when (eq status 0)
          (string-trim (buffer-string)))))))

(defun eca-chat-render-coalescing-bench--metadata ()
  "Return benchmark metadata as markdown."
  (let ((branch (or (eca-chat-render-coalescing-bench--git-output
                     "branch" "--show-current")
                    "unknown"))
        (commit (or (eca-chat-render-coalescing-bench--git-output
                     "rev-parse" "--short" "HEAD")
                    "unknown"))
        (dirty (eca-chat-render-coalescing-bench--git-output
                "status" "--short"))
        (command (mapconcat #'shell-quote-argument command-line-args " ")))
    (concat
     "# ECA chat render coalescing benchmark\n\n"
     (format "- Emacs: `%s`\n" emacs-version)
     (format "- Branch: `%s`\n" branch)
     (format "- Commit: `%s`\n" commit)
     (format "- Command: `%s`\n" command)
     (format "- Worktree dirty: `%s`\n\n"
             (if (and dirty (not (string-empty-p dirty))) "yes" "no")))))

;;;; Fixture and event helpers

(defun eca-chat-render-coalescing-bench--chunk (i)
  "Return one representative text chunk for index I."
  (format "chunk-%04d with `code`, **bold**, _emphasis_, and prose.\n" i))

(defun eca-chat-render-coalescing-bench--prepare-chunk (i)
  "Return one representative tool argument chunk for index I."
  (format "\"field-%04d\": \"value with `code` and **markdown**\",\n" i))

(defun eca-chat-render-coalescing-bench--tool-prepare-content (id i)
  "Return a toolCallPrepare content plist for ID and chunk index I."
  (list :type "toolCallPrepare"
        :id id
        :name "benchTool"
        :server "benchServer"
        :argumentsText (eca-chat-render-coalescing-bench--prepare-chunk i)
        :details (list :type "generic")))

(defun eca-chat-render-coalescing-bench--tool-running-content (id)
  "Return a toolCallRunning content plist for ID."
  (list :type "toolCallRunning"
        :id id
        :name "benchTool"
        :server "benchServer"
        :arguments "{}"
        :details (list :type "generic")))

(defun eca-chat-render-coalescing-bench--tool-called-content (id)
  "Return a toolCalled content plist for ID."
  (list :type "toolCalled"
        :id id
        :name "benchTool"
        :server "benchServer"
        :arguments "{}"
        :outputs (vector (list :text "done"))
        :details (list :type "generic")
        :totalTimeMs 42))

(defun eca-chat-render-coalescing-bench--subagent-run-content
    (id subagent-chat-id)
  "Return a subagent toolCallRun content plist.
ID is the parent tool-call ID.
SUBAGENT-CHAT-ID identifies the child chat."
  (list :type "toolCallRun"
        :id id
        :name "subagent"
        :server "eca"
        :arguments (list :agent "coder" :task "benchmark subagent stream")
        :details (list :type "subagent"
                       :model "bench-model"
                       :subagentChatId subagent-chat-id
                       :step 1
                       :maxSteps 10)))

(defun eca-chat-render-coalescing-bench--subagent-called-content
    (id subagent-chat-id)
  "Return a subagent toolCalled content plist for ID.
SUBAGENT-CHAT-ID identifies the child chat."
  (list :type "toolCalled"
        :id id
        :name "subagent"
        :server "eca"
        :arguments (list :agent "coder" :task "benchmark subagent stream")
        :outputs (vector (list :text "subagent complete"))
        :details (list :type "subagent"
                       :model "bench-model"
                       :subagentChatId subagent-chat-id
                       :step 10
                       :maxSteps 10)
        :totalTimeMs 1234))

(defun eca-chat-render-coalescing-bench--assistant-content (text)
  "Return assistant text content with TEXT."
  (list :type "text" :text text))

(defun eca-chat-render-coalescing-bench--render (session buffer role content
                                                        &optional parent-id chat-id)
  "Render CONTENT in BUFFER for SESSION.
ROLE, PARENT-ID, and CHAT-ID match `eca-chat--render-content'."
  (eca-chat--render-content
   session buffer role content
   (eca--session-workspace-folders session)
   parent-id chat-id))

(defun eca-chat-render-coalescing-bench--make-buffer ()
  "Return (BUFFER . SESSION) for a benchmark chat buffer."
  (let ((eca-chat-bench--session nil))
    (let ((buffer (eca-chat-bench--make-fixture
                   eca-chat-render-coalescing-bench-base-turns)))
      (cons buffer eca-chat-bench--session))))

(defun eca-chat-render-coalescing-bench--setup-subagent-parent
    (session buffer parent-id chat-id)
  "Create and open a subagent parent block.
SESSION and BUFFER identify the current benchmark chat.
PARENT-ID is the parent tool-call ID.
CHAT-ID is the subagent child chat ID."
  (eca-chat-render-coalescing-bench--render
   session buffer "assistant"
   (eca-chat-render-coalescing-bench--subagent-run-content parent-id chat-id))
  (eca-chat--expandable-content-toggle parent-id t nil))

(defun eca-chat-render-coalescing-bench--setup-prepare-block
    (session buffer tool-id)
  "Create and open a toolCallPrepare block for TOOL-ID."
  (let ((eca-chat-stream-flush-interval nil)
        (eca-chat-tool-call-prepare-throttle 'all))
    (eca-chat-render-coalescing-bench--render
     session buffer "assistant"
     (eca-chat-render-coalescing-bench--tool-prepare-content tool-id 0))
    (eca-chat--expandable-content-toggle tool-id t nil)))

(defun eca-chat-render-coalescing-bench--history-item-count ()
  "Return the generated history item count."
  (* eca-chat-render-coalescing-bench-history-groups 5))

(defun eca-chat-render-coalescing-bench--history-contents ()
  "Return generated history contents for replay benchmarking."
  (let (items)
    (dotimes (i eca-chat-render-coalescing-bench-history-groups)
      (let ((parent-id (format "hist-subagent-parent-%03d" i))
            (child-chat-id (format "hist-subagent-child-%03d" i))
            (root-chat-id (format "hist-root-%03d" i)))
        (push (list :chatId root-chat-id
                    :role "user"
                    :content (list :type "text"
                                   :contentId (format "hist-user-%03d" i)
                                   :text (format "history user %03d" i)))
              items)
        (push (list :chatId root-chat-id
                    :role "assistant"
                    :content (eca-chat-render-coalescing-bench--assistant-content
                              (eca-chat-render-coalescing-bench--chunk i)))
              items)
        (push (list :chatId root-chat-id
                    :role "assistant"
                    :content (eca-chat-render-coalescing-bench--tool-called-content
                              (format "hist-tool-%03d" i)))
              items)
        (push (list :chatId root-chat-id
                    :role "assistant"
                    :content (eca-chat-render-coalescing-bench--subagent-run-content
                              parent-id child-chat-id))
              items)
        (push (list :chatId child-chat-id
                    :parentChatId root-chat-id
                    :role "assistant"
                    :content (eca-chat-render-coalescing-bench--assistant-content
                              (concat "nested "
                                      (eca-chat-render-coalescing-bench--chunk i))))
              items)))
    (nreverse items)))

;;;; Counting and timing

(defvar eca-chat-render-coalescing-bench--counts nil
  "Current benchmark call counters.")

(defun eca-chat-render-coalescing-bench--inc (key)
  "Increment count KEY in the active benchmark counter table."
  (when (hash-table-p eca-chat-render-coalescing-bench--counts)
    (puthash key
             (1+ (gethash key eca-chat-render-coalescing-bench--counts 0))
             eca-chat-render-coalescing-bench--counts)))

(defun eca-chat-render-coalescing-bench--call-with-counts (fn)
  "Call FN while counting important render helper calls."
  (let ((render-content-fn (symbol-function 'eca-chat--render-content))
        (update-expandable-fn
         (symbol-function 'eca-chat--update-expandable-content))
        (add-text-fn (symbol-function 'eca-chat--add-text-content)))
    (cl-letf (((symbol-function 'eca-chat--render-content)
               (lambda (&rest args)
                 (eca-chat-render-coalescing-bench--inc 'render-content)
                 (apply render-content-fn args)))
              ((symbol-function 'eca-chat--update-expandable-content)
               (lambda (&rest args)
                 (eca-chat-render-coalescing-bench--inc 'update-expandable)
                 (apply update-expandable-fn args)))
              ((symbol-function 'eca-chat--add-text-content)
               (lambda (&rest args)
                 (eca-chat-render-coalescing-bench--inc 'add-text)
                 (apply add-text-fn args))))
      (funcall fn))))

(defun eca-chat-render-coalescing-bench--count (counts key)
  "Return COUNTS value for KEY, or zero."
  (gethash key counts 0))

(defun eca-chat-render-coalescing-bench--time-buffer
    (label events setup-fn run-fn)
  "Run one benchmark row.
LABEL names the row.
EVENTS is the number of target events in RUN-FN.
SETUP-FN prepares the fixture outside timing.
RUN-FN executes the measured workload."
  (let* ((fixture (eca-chat-render-coalescing-bench--make-buffer))
         (buffer (car fixture))
         (session (cdr fixture))
         (counts (make-hash-table :test 'eq))
         result buffer-size)
    (advice-add 'face-background
                :around #'eca-chat-bench--safe-face-background)
    (unwind-protect
        (with-current-buffer buffer
          (let ((inhibit-message t)
                (inhibit-read-only t))
            (funcall setup-fn session buffer)
            (garbage-collect)
            (let ((eca-chat-render-coalescing-bench--counts counts))
              (setq result
                    (benchmark-call
                     (lambda ()
                       (let ((inhibit-read-only t))
                         (eca-chat-render-coalescing-bench--call-with-counts
                          (lambda ()
                            (funcall run-fn session buffer)))))
                     1)))
            (setq buffer-size (buffer-size))))
      (advice-remove 'face-background #'eca-chat-bench--safe-face-background)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (list :label label
          :events events
          :iters 1
          :elapsed (nth 0 result)
          :gc-count (nth 1 result)
          :gc-elapsed (nth 2 result)
          :render-calls
          (eca-chat-render-coalescing-bench--count counts 'render-content)
          :update-calls
          (eca-chat-render-coalescing-bench--count counts 'update-expandable)
          :add-text-calls
          (eca-chat-render-coalescing-bench--count counts 'add-text)
          :buffer-size buffer-size)))

;;;; Benchmark cases

(defun eca-chat-render-coalescing-bench--bench-top-level ()
  "Benchmark top-level assistant stream rendering."
  (let ((chunks eca-chat-render-coalescing-bench-text-chunks))
    (eca-chat-render-coalescing-bench--time-buffer
     'top-level-assistant chunks
     (lambda (_session _buffer))
     (lambda (session buffer)
       (let ((eca-chat-stream-flush-interval 60)
             (eca-chat-fontify-debounce-interval nil))
         (dotimes (i chunks)
           (eca-chat-render-coalescing-bench--render
            session buffer "assistant"
            (eca-chat-render-coalescing-bench--assistant-content
             (eca-chat-render-coalescing-bench--chunk i))))
         (eca-chat--stream-flush))))))

(defun eca-chat-render-coalescing-bench--bench-subagent-one-parent ()
  "Benchmark subagent assistant stream under one parent tool call."
  (let ((chunks eca-chat-render-coalescing-bench-text-chunks)
        (parent-id "subagent-parent-1")
        (child-chat-id "subagent-child-1"))
    (eca-chat-render-coalescing-bench--time-buffer
     'subagent-one-parent chunks
     (lambda (session buffer)
       (eca-chat-render-coalescing-bench--setup-subagent-parent
        session buffer parent-id child-chat-id))
     (lambda (session buffer)
       (let ((eca-chat-stream-flush-interval 60))
         (dotimes (i chunks)
           (eca-chat-render-coalescing-bench--render
            session buffer "assistant"
            (eca-chat-render-coalescing-bench--assistant-content
             (eca-chat-render-coalescing-bench--chunk i))
            parent-id child-chat-id))
         (eca-chat--stream-flush))))))

(defun eca-chat-render-coalescing-bench--bench-subagent-split-parents ()
  "Benchmark subagent assistant streams split across parent tool calls."
  (let* ((chunks eca-chat-render-coalescing-bench-text-chunks)
         (parent-count eca-chat-render-coalescing-bench-parent-count)
         (parents (cl-loop for i below parent-count
                           collect (format "subagent-parent-%d" i)))
         (children (cl-loop for i below parent-count
                            collect (format "subagent-child-%d" i))))
    (eca-chat-render-coalescing-bench--time-buffer
     'subagent-split-parents chunks
     (lambda (session buffer)
       (cl-loop for parent in parents
                for child in children
                do (eca-chat-render-coalescing-bench--setup-subagent-parent
                    session buffer parent child)))
     (lambda (session buffer)
       (let ((eca-chat-stream-flush-interval 60))
         (dotimes (i chunks)
           (let ((index (mod i parent-count)))
             (eca-chat-render-coalescing-bench--render
              session buffer "assistant"
              (eca-chat-render-coalescing-bench--assistant-content
               (eca-chat-render-coalescing-bench--chunk i))
              (nth index parents)
              (nth index children))))
         (eca-chat--stream-flush))))))

(defun eca-chat-render-coalescing-bench--bench-tool-prepare (throttle label)
  "Benchmark toolCallPrepare rendering with THROTTLE.
LABEL names the result row."
  (let ((chunks eca-chat-render-coalescing-bench-prepare-chunks)
        (tool-id (format "prepare-%s" label)))
    (eca-chat-render-coalescing-bench--time-buffer
     label chunks
     (lambda (session buffer)
       (eca-chat-render-coalescing-bench--setup-prepare-block
        session buffer tool-id))
     (lambda (session buffer)
       (let ((eca-chat-stream-flush-interval 60)
             (eca-chat-tool-call-prepare-throttle throttle)
             (eca-chat-tool-call-prepare-update-interval 5))
         (dotimes (i chunks)
           (eca-chat-render-coalescing-bench--render
            session buffer "assistant"
            (eca-chat-render-coalescing-bench--tool-prepare-content
             tool-id (1+ i))))
         (eca-chat--stream-flush))))))

(defun eca-chat-render-coalescing-bench--bench-mixed-stream ()
  "Benchmark mixed assistant, prepare, subagent, and lifecycle events."
  (let* ((chunks 500)
         (parent-id "mixed-subagent-parent")
         (child-chat-id "mixed-subagent-child")
         (tool-id "mixed-prepare"))
    (eca-chat-render-coalescing-bench--time-buffer
     'mixed-stream (+ (* chunks 3) 2)
     (lambda (session buffer)
       (eca-chat-render-coalescing-bench--setup-subagent-parent
        session buffer parent-id child-chat-id)
       (eca-chat-render-coalescing-bench--setup-prepare-block
        session buffer tool-id))
     (lambda (session buffer)
       (let ((eca-chat-stream-flush-interval 60)
             (eca-chat-tool-call-prepare-throttle 'smart)
             (eca-chat-tool-call-prepare-update-interval 5))
         (dotimes (i chunks)
           (eca-chat-render-coalescing-bench--render
            session buffer "assistant"
            (eca-chat-render-coalescing-bench--assistant-content
             (eca-chat-render-coalescing-bench--chunk i)))
           (eca-chat-render-coalescing-bench--render
            session buffer "assistant"
            (eca-chat-render-coalescing-bench--tool-prepare-content
             tool-id i))
           (eca-chat-render-coalescing-bench--render
            session buffer "assistant"
            (eca-chat-render-coalescing-bench--assistant-content
             (concat "subagent "
                     (eca-chat-render-coalescing-bench--chunk i)))
            parent-id child-chat-id))
         (eca-chat-render-coalescing-bench--render
          session buffer "assistant"
          (eca-chat-render-coalescing-bench--tool-running-content tool-id))
         (eca-chat-render-coalescing-bench--render
          session buffer "assistant"
          (eca-chat-render-coalescing-bench--subagent-called-content
           parent-id child-chat-id))
         (eca-chat--stream-flush))))))

(defun eca-chat-render-coalescing-bench--bench-history-replay ()
  "Benchmark batch history replay."
  (let ((contents (eca-chat-render-coalescing-bench--history-contents)))
    (eca-chat-render-coalescing-bench--time-buffer
     'history-replay (length contents)
     (lambda (_session _buffer))
     (lambda (session buffer)
       (eca-chat--render-history-contents session buffer contents)))))

;;;; Output

(defun eca-chat-render-coalescing-bench--reset ()
  "Reset benchmark result state."
  (setq eca-chat-render-coalescing-bench--results nil))

(defun eca-chat-render-coalescing-bench--add-result (result)
  "Append RESULT to the benchmark results."
  (setq eca-chat-render-coalescing-bench--results
        (append eca-chat-render-coalescing-bench--results (list result))))

(defun eca-chat-render-coalescing-bench--format-results ()
  "Return benchmark results as a markdown table."
  (concat
   "| op | events | renders | updates | add-text | chars | gc | gc-ms | wall-ms | per-event-us |\n"
   "|----|-------:|--------:|--------:|---------:|------:|---:|------:|--------:|-------------:|\n"
   (mapconcat
    (lambda (result)
      (let* ((events (plist-get result :events))
             (elapsed (plist-get result :elapsed))
             (gc-elapsed (plist-get result :gc-elapsed))
             (per-event-us (if (> events 0)
                               (/ (* elapsed 1000000.0) events)
                             0.0)))
        (format "| %s | %d | %d | %d | %d | %d | %d | %.2f | %.2f | %.2f |"
                (plist-get result :label)
                events
                (plist-get result :render-calls)
                (plist-get result :update-calls)
                (plist-get result :add-text-calls)
                (plist-get result :buffer-size)
                (plist-get result :gc-count)
                (* gc-elapsed 1000.0)
                (* elapsed 1000.0)
                per-event-us)))
    eca-chat-render-coalescing-bench--results
    "\n")
   "\n"))

;;;###autoload
(defun eca-chat-render-coalescing-bench-run ()
  "Run render coalescing benchmarks and print markdown results."
  (interactive)
  (eca-chat-render-coalescing-bench--reset)
  (message "[render-coalescing-bench] top-level assistant ...")
  (eca-chat-render-coalescing-bench--add-result
   (eca-chat-render-coalescing-bench--bench-top-level))
  (message "[render-coalescing-bench] subagent one parent ...")
  (eca-chat-render-coalescing-bench--add-result
   (eca-chat-render-coalescing-bench--bench-subagent-one-parent))
  (message "[render-coalescing-bench] subagent split parents ...")
  (eca-chat-render-coalescing-bench--add-result
   (eca-chat-render-coalescing-bench--bench-subagent-split-parents))
  (message "[render-coalescing-bench] tool prepare all ...")
  (eca-chat-render-coalescing-bench--add-result
   (eca-chat-render-coalescing-bench--bench-tool-prepare
    'all 'tool-prepare-all))
  (message "[render-coalescing-bench] tool prepare smart ...")
  (eca-chat-render-coalescing-bench--add-result
   (eca-chat-render-coalescing-bench--bench-tool-prepare
    'smart 'tool-prepare-smart))
  (message "[render-coalescing-bench] mixed stream ...")
  (eca-chat-render-coalescing-bench--add-result
   (eca-chat-render-coalescing-bench--bench-mixed-stream))
  (message "[render-coalescing-bench] history replay ...")
  (eca-chat-render-coalescing-bench--add-result
   (eca-chat-render-coalescing-bench--bench-history-replay))
  (let ((output (concat (eca-chat-render-coalescing-bench--metadata)
                        (eca-chat-render-coalescing-bench--format-results))))
    (if noninteractive
        (princ output)
      (with-current-buffer
          (get-buffer-create "*eca-chat-render-coalescing-bench*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert output))
        (display-buffer (current-buffer))))
    output))

(provide 'eca-chat-render-coalescing-bench)
;;; eca-chat-render-coalescing-bench.el ends here
