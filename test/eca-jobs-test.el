;;; eca-jobs-test.el --- Tests for eca-jobs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'buttercup)
(require 'eca-jobs)

(defun eca-jobs-test--make-session (&optional jobs)
  "Create a minimal mock session with JOBS."
  (let ((session (make-eca--session)))
    (setf (eca--session-jobs session) jobs)
    session))

(defun eca-jobs-test--running-job (&optional id)
  "Create a running job plist with ID."
  (list :id (or id "job-1")
        :status "running"
        :label "sleep 30"
        :summary "dev-server"
        :startedAt "2025-01-01T00:00:00Z"
        :elapsed "5s"
        :exitCode nil
        :chatId "chat-1"
        :chatLabel "Test Chat"))

(defun eca-jobs-test--completed-job (&optional id)
  "Create a completed job plist with ID."
  (list :id (or id "job-2")
        :status "completed"
        :label "echo hello"
        :summary "build-check"
        :startedAt "2025-01-01T00:00:00Z"
        :elapsed "1s"
        :exitCode 0
        :chatId "chat-1"
        :chatLabel "Test Chat"))

(defun eca-jobs-test--failed-job (&optional id)
  "Create a failed job plist with ID."
  (list :id (or id "job-3")
        :status "failed"
        :label "bad-command"
        :summary "lint-run"
        :startedAt "2025-01-01T00:00:00Z"
        :elapsed "0s"
        :exitCode 1
        :chatId "chat-2"
        :chatLabel "Other Chat"))

;; has-running-p

(describe "eca-jobs--has-running-p"
  (it "returns non-nil when a running job exists"
    (let ((session (eca-jobs-test--make-session
                    (list (eca-jobs-test--running-job)))))
      (expect (eca-jobs--has-running-p session) :to-be-truthy)))

  (it "returns nil when no running jobs exist"
    (let ((session (eca-jobs-test--make-session
                    (list (eca-jobs-test--completed-job)))))
      (expect (eca-jobs--has-running-p session) :not :to-be-truthy)))

  (it "returns nil when jobs list is empty"
    (let ((session (eca-jobs-test--make-session nil)))
      (expect (eca-jobs--has-running-p session) :not :to-be-truthy))))

;; status-emoji

(describe "eca-jobs--status-emoji"
  (it "returns correct emoji for each status"
    (expect (eca-jobs--status-emoji "running") :to-equal "🟡")
    (expect (eca-jobs--status-emoji "completed") :to-equal "✅")
    (expect (eca-jobs--status-emoji "failed") :to-equal "🔴")
    (expect (eca-jobs--status-emoji "killed") :to-equal "⚫"))

  (it "returns default emoji for unknown status"
    (expect (eca-jobs--status-emoji "unknown") :to-equal "⚫")))

;; elapsed-since

(describe "eca-jobs--elapsed-since"
  (it "returns nil for nil input"
    (expect (eca-jobs--elapsed-since nil) :to-be nil))

  (it "returns nil for non-string input"
    (expect (eca-jobs--elapsed-since 42) :to-be nil))

  (it "returns a string for valid ISO 8601 input"
    (expect (eca-jobs--elapsed-since "2020-01-01T00:00:00Z")
            :to-match "^[0-9]+[dhms]")))

;; group-by-chat

(describe "eca-jobs--group-by-chat"
  (it "groups jobs by chat-id"
    (let* ((job1 (eca-jobs-test--running-job "job-1"))
           (job2 (eca-jobs-test--completed-job "job-2"))
           (job3 (eca-jobs-test--failed-job "job-3"))
           (groups (eca-jobs--group-by-chat (list job1 job2 job3))))
      (expect (length groups) :to-equal 2)
      ;; chat-1 group has 2 jobs
      (let ((chat1-group (assoc "chat-1" groups)))
        (expect chat1-group :to-be-truthy)
        (expect (length (cddr chat1-group)) :to-equal 2))
      ;; chat-2 group has 1 job
      (let ((chat2-group (assoc "chat-2" groups)))
        (expect chat2-group :to-be-truthy)
        (expect (length (cddr chat2-group)) :to-equal 1))))

  (it "returns empty list for empty input"
    (expect (eca-jobs--group-by-chat nil) :to-equal nil)))

;; render

(describe "eca-jobs--render"
  (it "renders 'No background jobs' when empty"
    (let ((session (eca-jobs-test--make-session nil)))
      (with-temp-buffer
        (eca-jobs--render session (current-buffer))
        (expect (buffer-string) :to-match "No background jobs"))))

  (it "renders job entries with status emoji and summary"
    (let ((session (eca-jobs-test--make-session
                    (list (eca-jobs-test--running-job "job-1")
                          (eca-jobs-test--completed-job "job-2")))))
      (with-temp-buffer
        (eca-jobs--render session (current-buffer))
        (expect (buffer-string) :to-match "🟡")
        (expect (buffer-string) :to-match "dev-server")
        (expect (buffer-string) :to-match "✅")
        (expect (buffer-string) :to-match "build-check"))))

  (it "renders kill button only for running jobs"
    (let ((session (eca-jobs-test--make-session
                    (list (eca-jobs-test--running-job "job-1")
                          (eca-jobs-test--completed-job "job-2")))))
      (with-temp-buffer
        (eca-jobs--render session (current-buffer))
        ;; Only one "kill" button for the running job
        (let ((kill-count 0))
          (goto-char (point-min))
          (while (search-forward "kill" nil t)
            (setq kill-count (1+ kill-count)))
          (expect kill-count :to-equal 1)))))

  (it "renders exit code for failed jobs"
    (let ((session (eca-jobs-test--make-session
                    (list (eca-jobs-test--failed-job)))))
      (with-temp-buffer
        (eca-jobs--render session (current-buffer))
        (expect (buffer-string) :to-match "exit:1"))))

  (it "renders command on second line"
    (let ((session (eca-jobs-test--make-session
                    (list (eca-jobs-test--running-job "job-1")))))
      (with-temp-buffer
        (eca-jobs--render session (current-buffer))
        (expect (buffer-string) :to-match "    sleep 30"))))

  (it "sets eca-job-id text property on job lines"
    (let ((session (eca-jobs-test--make-session
                    (list (eca-jobs-test--running-job "job-1")))))
      (with-temp-buffer
        (eca-jobs--render session (current-buffer))
        (goto-char (point-min))
        (search-forward "dev-server")
        (expect (get-text-property (point) 'eca-job-id)
                :to-equal "job-1")))))

(provide 'eca-jobs-test)
;;; eca-jobs-test.el ends here
