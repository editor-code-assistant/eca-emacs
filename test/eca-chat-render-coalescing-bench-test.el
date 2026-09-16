;;; eca-chat-render-coalescing-bench-test.el --- Tests for render benchmark -*- lexical-binding: t; -*-
;;; Commentary:
;; Tests for the chat render coalescing benchmark harness.
;;; Code:
(require 'buttercup)
(require 'cl-lib)
(require 'subr-x)

(let* ((test-file (or load-file-name buffer-file-name))
       (test-dir (and test-file (file-name-directory test-file)))
       (repo-root (and test-dir
                       (file-name-directory
                        (directory-file-name test-dir))))
       (bench-dir (and repo-root
                       (expand-file-name "benchmarks" repo-root))))
  (when bench-dir
    (add-to-list 'load-path bench-dir)))

(require 'eca-chat-render-coalescing-bench)

(describe "eca-chat-render-coalescing-bench"
  (it "runs git metadata commands in the benchmark repo root"
    (let* ((repo-root eca-chat-render-coalescing-bench-repo-root)
           (expected (let ((default-directory repo-root))
                       (with-temp-buffer
                         (expect (process-file "git" nil t nil
                                               "rev-parse" "--show-toplevel")
                                 :to-equal 0)
                         (string-trim (buffer-string)))))
           (default-directory temporary-file-directory))
      (expect (eca-chat-render-coalescing-bench--git-output
               "rev-parse" "--show-toplevel")
              :to-equal expected)))

  (it "passes the fixture session to setup and workload functions"
    (let* ((fixture-session
            (make-eca--session :id "fixture-session"
                               :workspace-folders '("/fixture")))
           (workload-session
            (make-eca--session :id "workload-session"
                               :workspace-folders '("/workload")))
           (calls 0)
           seen-setup
           seen-workload)
      (cl-letf (((symbol-function 'eca-chat-bench--ensure-session)
                 (lambda ()
                   (setq calls (1+ calls))
                   (setq eca-chat-bench--session
                         (if (= calls 1)
                             fixture-session
                           workload-session)))))
        (eca-chat-render-coalescing-bench--time-buffer
         'session-check 0
         (lambda (session _buffer)
           (setq seen-setup
                 (list :passed-session (eca--session-id session)
                       :buffer-session eca--session-id-cache)))
         (lambda (session _buffer)
           (setq seen-workload
                 (list :passed-session (eca--session-id session)
                       :buffer-session eca--session-id-cache)))))
      (expect seen-setup
              :to-equal
              '(:passed-session "fixture-session"
                :buffer-session "fixture-session"))
      (expect seen-workload
              :to-equal
              '(:passed-session "fixture-session"
                :buffer-session "fixture-session"))))

  (it "creates prepare fixture blocks under stream buffering"
    (let* ((fixture (eca-chat-render-coalescing-bench--make-buffer))
           (buffer (car fixture))
           (session (cdr fixture)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((eca-chat-stream-flush-interval 0.05))
              (eca-chat-render-coalescing-bench--setup-prepare-block
               session buffer "prepare-default"))
            (expect (eca-chat--get-expandable-content "prepare-default")
                    :not :to-be nil)
            (expect (overlay-get
                     (eca-chat--get-expandable-content "prepare-default")
                     'eca-chat--expandable-content-toggle)
                    :to-be-truthy))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (fboundp 'eca-chat--stream-cancel)
              (eca-chat--stream-cancel)))
          (kill-buffer buffer))))))

(provide 'eca-chat-render-coalescing-bench-test)
;;; eca-chat-render-coalescing-bench-test.el ends here
