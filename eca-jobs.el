;;; eca-jobs.el --- ECA (Editor Code Assistant) background jobs -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) background jobs.
;;
;;; Code:

(require 'compat)
(require 'iso8601)

(require 'eca-util)
(require 'eca-api)
(require 'eca-settings)

;; Faces

(defface eca-jobs-status-running-face
  '((t (:inherit warning :weight bold)))
  "Face for running job status."
  :group 'eca)

(defface eca-jobs-status-completed-face
  '((t (:inherit success :weight bold)))
  "Face for completed job status."
  :group 'eca)

(defface eca-jobs-status-failed-face
  '((t (:inherit error :weight bold)))
  "Face for failed job status."
  :group 'eca)

(defface eca-jobs-status-killed-face
  '((t (:inherit shadow)))
  "Face for killed job status."
  :group 'eca)

(defface eca-jobs-chat-separator-face
  '((t (:inherit bold)))
  "Face for chat group separator headers."
  :group 'eca)

(defface eca-jobs-button-output-face
  '((t (:inherit link)))
  "Face for output button in job cards."
  :group 'eca)

(defface eca-jobs-button-kill-face
  '((t (:inherit error :underline t)))
  "Face for kill button in job cards."
  :group 'eca)

;; Internal

(declare-function eca-api-notify "eca-api")
(declare-function eca-api-request-async "eca-api")

(defvar-local eca-jobs--refresh-timer nil
  "Timer for refreshing elapsed time in the Jobs tab.")

;; Keymap

(defvar eca-jobs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map eca-settings-mode-map)
    (define-key map (kbd "k") #'eca-jobs--kill-at-point)
    (define-key map (kbd "o") #'eca-jobs--view-output)
    (define-key map (kbd "RET") #'eca-jobs--view-output)
    (define-key map (kbd "<return>") #'eca-jobs--view-output)
    map)
  "Keymap for the Jobs settings tab.")

;; Helpers

(defun eca-jobs--elapsed-since (started-at)
  "Compute human-readable elapsed time from STARTED-AT ISO 8601 string."
  (when (and started-at (stringp started-at))
    (condition-case nil
        (let* ((start (parse-iso8601-time-string
                       (replace-regexp-in-string "Z$" "+00:00"
                                                 (replace-regexp-in-string "\\.[0-9]+" "" started-at))))
               (secs (floor (float-time (time-subtract nil start)))))
          (cond
           ((< secs 60) (format "%ds" secs))
           ((< secs 3600) (format "%dm%ds" (/ secs 60) (mod secs 60)))
           (t (format "%dh%dm" (/ secs 3600) (mod (/ secs 60) 60)))))
      (error nil))))

(defun eca-jobs--status-emoji (status)
  "Return a colored emoji circle for STATUS."
  (pcase status
    ("running" "🟡")
    ("completed" "✅")
    ("failed" "🔴")
    ("killed" "⚫")
    (_ "⚫")))

(defun eca-jobs--group-by-chat (jobs)
  "Group JOBS by :chat-id, returning alist of (chat-id . (chat-label . jobs))."
  (let ((groups nil))
    (seq-doseq (job jobs)
      (let* ((chat-id (or (plist-get job :chatId) "unknown"))
             (chat-label (or (plist-get job :chatLabel) "Unknown Chat"))
             (existing (assoc chat-id groups)))
        (if existing
            (setcdr existing (cons chat-label
                                   (append (cddr existing) (list job))))
          (push (cons chat-id (cons chat-label (list job))) groups))))
    (nreverse groups)))

(defun eca-jobs--kill-at-point ()
  "Kill the background job at point."
  (interactive)
  (if-let* ((session (ignore-errors (eca-session)))
            (job-id (get-text-property (point) 'eca-job-id)))
      (when (y-or-n-p (format "Kill job %s? " job-id))
        (eca-api-request-async session
                               :method "jobs/kill"
                               :params (list :job-id job-id)
                               :success-callback
                               (lambda (result)
                                 (if (plist-get result :killed)
                                     (message "Job %s killed" job-id)
                                   (message "Job %s was not running" job-id)))
                               :error-callback
                               (lambda (_err)
                                 (user-error "Failed to kill job %s" job-id))))
    (user-error "No running job at point")))

(defun eca-jobs--view-output-by-id (session job-id)
  "Fetch and display output for JOB-ID using SESSION."
  (eca-api-request-async session
    :method "jobs/readOutput"
    :params (list :job-id job-id)
    :success-callback
    (lambda (result)
      (let* ((buf-name (format "*eca-job-output: %s*" job-id))
             (lines (plist-get result :lines))
             (status (plist-get result :status))
             (exit-code (plist-get result :exitCode))
             (buf (get-buffer-create buf-name)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize (format "Job: %s  Status: %s%s\n\n"
                                       job-id
                                       (or status "unknown")
                                       (if exit-code
                                           (format "  Exit: %s" exit-code)
                                         ""))
                               'font-lock-face 'bold))
            (if (and lines (> (length lines) 0))
                (dolist (line (append lines nil))
                  (let ((text (or (plist-get line :text) ""))
                        (stream (or (plist-get line :stream) "stdout")))
                    (insert (if (string= stream "stderr")
                                (propertize text 'font-lock-face 'eca-jobs-status-failed-face)
                              text))
                    (insert "\n")))
              (insert (propertize "No output" 'font-lock-face 'shadow))))
          (special-mode)
          (goto-char (point-max)))
        (let ((win (display-buffer buf '((display-buffer-reuse-window
                                          display-buffer-below-selected)
                                         (window-height . 0.4)))))
          (when win
            (with-selected-window win
              (goto-char (point-max))
              (recenter -1))))))
    :error-callback
    (lambda (_err)
      (user-error "Failed to read output for job %s" job-id))))

(defun eca-jobs--view-output ()
  "View output of the background job at point."
  (interactive)
  (if-let* ((session (ignore-errors (eca-session)))
            (job-id (get-text-property (point) 'eca-job-id)))
      (eca-jobs--view-output-by-id session job-id)
    (user-error "No job at point")))

;; Timer

(defun eca-jobs--has-running-p (session)
  "Return non-nil if SESSION has any running jobs."
  (seq-some (lambda (j) (string= "running" (plist-get j :status)))
            (eca--session-jobs session)))

(defun eca-jobs--stop-timer (buffer)
  "Stop the refresh timer for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when eca-jobs--refresh-timer
        (cancel-timer eca-jobs--refresh-timer)
        (setq eca-jobs--refresh-timer nil)))))

(defun eca-jobs--ensure-timer (session buffer)
  "Start or stop the refresh timer for BUFFER based on running jobs in SESSION."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (eca-jobs--has-running-p session)
          (unless eca-jobs--refresh-timer
            (setq eca-jobs--refresh-timer
                  (run-with-timer 1 1
                                  (lambda ()
                                    (cond
                                     ((not (buffer-live-p buffer))
                                      (eca-jobs--stop-timer buffer))
                                     ((get-buffer-window buffer)
                                      (eca-jobs--render session buffer)))))))
        (eca-jobs--stop-timer buffer)))))

;; Rendering

(defun eca-jobs--render (session buffer)
  "Render background jobs for SESSION into BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (keymap (or (current-local-map) eca-settings-mode-map))
            (jobs (eca--session-jobs session))
            (saved-pos (point)))
        (erase-buffer)
        (insert "\n")
        (insert (propertize "Background Jobs" 'font-lock-face 'eca-settings-heading))
        (insert "\n\n")
        (if (or (null jobs) (seq-empty-p jobs))
            (insert (propertize "No background jobs" 'font-lock-face font-lock-doc-face))
          (let ((groups (eca-jobs--group-by-chat jobs))
                (first-group t))
            (dolist (group groups)
              (let ((chat-label (cadr group))
                    (group-jobs (cddr group)))
                (unless first-group
                  (insert "\n"))
                (setq first-group nil)
                ;; Chat separator
                (insert (propertize (concat "── " chat-label " "
                                           (make-string (max 0 (- 60 (length chat-label) 4)) ?─))
                                   'font-lock-face 'eca-jobs-chat-separator-face))
                (insert "\n")
                ;; Jobs in this group
                (dolist (job group-jobs)
                  (let* ((id (plist-get job :id))
                         (status (plist-get job :status))
                         (label (or (plist-get job :label) ""))
                         (summary (or (plist-get job :summary) id))
                         (elapsed (or (when (string= status "running")
                                       (eca-jobs--elapsed-since (plist-get job :startedAt)))
                                     (plist-get job :elapsed) ""))
                         (exit-code (plist-get job :exitCode))
                         (truncated-label (if (> (length label) 80)
                                              (concat (substring label 0 77) "...")
                                            label))
                         (start (point)))
                    ;; Line 1: status + summary + elapsed + buttons
                    (insert (propertize (eca-jobs--status-emoji status)
                                       'help-echo status))
                    (insert " ")
                    (insert (propertize summary 'font-lock-face 'bold))
                    (insert "  ")
                    (insert (propertize elapsed 'font-lock-face 'shadow))
                    (when (and (string= status "failed") exit-code)
                      (insert "  ")
                      (insert (propertize (format "exit:%s" exit-code)
                                         'font-lock-face 'eca-jobs-status-failed-face)))
                    (insert "  ")
                    (insert (eca-buttonize
                             keymap
                             (propertize "output"
                                         'font-lock-face 'eca-jobs-button-output-face)
                             (lambda ()
                               (when-let* ((s (ignore-errors (eca-session))))
                                 (eca-jobs--view-output-by-id s id)))))
                    (when (string= status "running")
                      (insert "  ")
                      (insert (eca-buttonize
                               keymap
                               (propertize "kill"
                                           'font-lock-face 'eca-jobs-button-kill-face)
                               (lambda ()
                                 (when-let* ((s (ignore-errors (eca-session))))
                                   (eca-api-request-async s
                                                          :method "jobs/kill"
                                                          :params (list :job-id id)
                                                          :success-callback
                                                          (lambda (result)
                                                            (if (plist-get result :killed)
                                                                (message "Job %s killed" id)
                                                              (message "Job %s was not running" id)))
                                                          :error-callback
                                                          (lambda (_err)
                                                            (user-error "Failed to kill job %s" id))))))))
                    (insert "\n")
                    ;; Line 2: command (indented)
                    (insert (propertize (concat "    " truncated-label)
                                       'font-lock-face 'shadow))
                    (insert "\n")
                    (put-text-property start (point) 'eca-job-id id)))))))
        (insert "\n")
        (goto-char (min saved-pos (point-max)))
        (eca-jobs--ensure-timer session buffer)))))

;; Notification handler

(defun eca-jobs--handle-jobs-updated (session params)
  "Update jobs list in SESSION from PARAMS and refresh tab + modeline."
  (setf (eca--session-jobs session)
        (append (plist-get params :jobs) nil))
  (eca-settings-refresh-tab "jobs" session)
  ;; Manage timer in any live jobs buffer
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (string-match-p "eca-settings.*:jobs:" (buffer-name buf)))
      (eca-jobs--ensure-timer session buf)))
  ;; Update modeline in chat buffers
  (dolist (chat-entry (eca--session-chats session))
    (when-let* ((chat-buf (cdr chat-entry)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (force-mode-line-update))))))

;; Settings tab

(defun eca-jobs--create-settings-buffer (session)
  "Create the Jobs settings tab buffer for SESSION."
  (let ((buf (eca-settings--create-buffer "jobs" session)))
    (with-current-buffer buf
      (eca-settings-mode)
      (use-local-map eca-jobs-mode-map)
      (visual-line-mode)
      (eca-settings--setup-tab-line "jobs" session)
      (eca-jobs--render session buf)
      (add-hook 'kill-buffer-hook
                (lambda () (eca-jobs--stop-timer buf))
                nil t)
      (eca-api-request-async session
        :method "jobs/list"
        :params nil
        :success-callback
        (lambda (result)
          (setf (eca--session-jobs session)
                (append (plist-get result :jobs) nil))
          (when (buffer-live-p buf)
            (eca-jobs--render session buf)
            (eca-jobs--ensure-timer session buf)))))
    buf))

;; Public

;;;###autoload
(defun eca-jobs ()
  "List background jobs with their status and options."
  (interactive)
  (eca-settings "jobs"))

(eca-settings-register-tab
 "jobs" "⚡ Jobs"
 #'eca-jobs--create-settings-buffer
 #'eca-jobs--render)

(provide 'eca-jobs)
;;; eca-jobs.el ends here
