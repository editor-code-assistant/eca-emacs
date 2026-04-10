;;; eca-chat-performance-test.el --- Chat performance tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for chat rendering hot paths.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'eca-chat)

(defun eca-chat-performance-test--with-chat-buffer (fn)
  "Create a temporary chat buffer and call FN inside it."
  (let ((buf (generate-new-buffer "*eca-chat-performance*")))
    (unwind-protect
        (with-current-buffer buf
          (gfm-mode)
          (eca-chat--initialize-buffer-state)
          (eca-chat--insert "\n")
          (eca-chat--insert-prompt-string)
          (funcall fn buf))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun eca-chat-performance-test--overlay-count (predicate)
  "Count overlays matching PREDICATE in the current buffer."
  (cl-count-if predicate (overlays-in (point-min) (point-max))))

(defun eca-chat-performance-test--count-pipe-face-props ()
  "Count pipe chars styled with `eca-table-pipe-face'."
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "|" nil t)
        (when (get-text-property (1- (point)) 'eca-table-pipe-face)
          (setq count (1+ count)))))
    count))

(defun eca-chat-performance-test--count-pipes ()
  "Count pipe chars in the current buffer."
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "|" nil t)
        (setq count (1+ count))))
    count))

(defun eca-chat-performance-test--buffer-contains-p (needle)
  "Return non-nil when current buffer text contains NEEDLE."
  (not (null (string-match-p (regexp-quote needle)
                             (buffer-substring-no-properties
                              (point-min)
                              (point-max))))))

(describe "eca-chat performance helpers"
  (it "stores tool-call prepare content as chunks"
    (with-temp-buffer
      (eca-chat--initialize-buffer-state)
      (eca-chat--tool-call-prepare-append "tool-1" "alpha")
      (eca-chat--tool-call-prepare-append "tool-1" "beta")
      (eca-chat--tool-call-prepare-append "tool-1" nil)
      (eca-chat--tool-call-prepare-append "tool-1" "")
      (expect (gethash "tool-1" eca-chat--tool-call-prepare-content-cache)
              :to-equal '("beta" "alpha"))
      (expect (eca-chat--tool-call-prepare-content "tool-1")
              :to-equal "alphabeta")))

  (it "materializes many prepare chunks in append order"
    (with-temp-buffer
      (eca-chat--initialize-buffer-state)
      (let ((expected ""))
        (dotimes (i 50)
          (let ((chunk (format "[%d]" i)))
            (setq expected (concat expected chunk))
            (eca-chat--tool-call-prepare-append "tool-1" chunk)))
        (expect (length (gethash "tool-1"
                                 eca-chat--tool-call-prepare-content-cache))
                :to-equal 50)
        (expect (eca-chat--tool-call-prepare-content "tool-1")
                :to-equal expected))))

  (it "creates independent mutable state per chat buffer"
    (let (first-value second-value)
      (with-temp-buffer
        (eca-chat--initialize-buffer-state)
        (eca-chat--tool-call-prepare-append "tool-1" "alpha")
        (setq first-value
              (eca-chat--tool-call-prepare-content "tool-1")))
      (with-temp-buffer
        (eca-chat--initialize-buffer-state)
        (setq second-value
              (gethash "tool-1" eca-chat--tool-call-prepare-content-cache)))
      (expect first-value :to-equal "alpha")
      (expect second-value :to-be nil)))

  (it "merges deferred fontify ranges behind one timer"
    (with-temp-buffer
      (eca-chat--initialize-buffer-state)
      (insert "abcdefghij")
      (eca-chat--schedule-fontify-range 4 6)
      (let ((timer eca-chat--fontify-idle-timer))
        (eca-chat--schedule-fontify-range 2 9)
        (expect (eq timer eca-chat--fontify-idle-timer) :to-equal t)
        (expect (marker-position eca-chat--pending-fontify-start-marker)
                :to-equal 2)
        (expect (marker-position eca-chat--pending-fontify-end-marker)
                :to-equal 9)
        (eca-chat--clear-pending-fontify-range))))

  (it "runs pending fontify once for the merged range"
    (with-temp-buffer
      (eca-chat--initialize-buffer-state)
      (insert "abcdefghij")
      (eca-chat--schedule-fontify-range 3 5)
      (eca-chat--schedule-fontify-range 1 8)
      (let (flush-call)
        (cl-letf (((symbol-function 'eca-chat--flush-fontify-range)
                   (lambda (beg end)
                     (setq flush-call (list beg end)))))
          (eca-chat--run-pending-fontify))
        (expect flush-call :to-equal '(1 8))
        (expect eca-chat--fontify-idle-timer :to-be nil)
        (expect eca-chat--pending-fontify-start-marker :to-be nil)
        (expect eca-chat--pending-fontify-end-marker :to-be nil))))

  (it "keeps expandable content in stored chunks across toggles"
    (eca-chat-performance-test--with-chat-buffer
     (lambda (_buf)
       (eca-chat--add-expandable-content "block-1" "Tool call" "alpha")
       (eca-chat--update-expandable-content "block-1" nil "beta" t)
       (eca-chat--update-expandable-content "block-1" nil nil t)
       (eca-chat--update-expandable-content "block-1" nil "" t)
       (expect (eca-chat--expandable-content-raw "block-1")
               :to-equal "alphabeta")
       (eca-chat--expandable-content-toggle "block-1" t nil)
       (expect (eca-chat-performance-test--buffer-contains-p "alphabeta")
               :to-equal t)
       (eca-chat--expandable-content-toggle "block-1" t t)
       (expect (eca-chat--expandable-content-raw "block-1")
               :to-equal "alphabeta"))))

  (it "drops stale expandable index entries on lookup"
    (eca-chat-performance-test--with-chat-buffer
     (lambda (_buf)
       (eca-chat--add-expandable-content "block-1" "Tool call" "alpha")
       (let ((ov (eca-chat--get-expandable-content "block-1")))
         (delete-overlay ov)
         (expect (eca-chat--get-expandable-content "block-1") :to-be nil)
         (expect (gethash "block-1" eca-chat--expandable-content-index)
                 :to-be nil)))))

  (it "preserves nested child content across parent toggles"
    (eca-chat-performance-test--with-chat-buffer
     (lambda (_buf)
       (eca-chat--add-expandable-content "parent" "Parent" "parent text")
       (eca-chat--expandable-content-toggle "parent" t nil)
       (eca-chat--add-expandable-content "child" "Child" "alpha" "parent")
       (eca-chat--update-expandable-content "child" nil "beta" t "parent")
       (eca-chat--expandable-content-toggle "child" t nil)
       (expect (eca-chat-performance-test--buffer-contains-p "alphabeta")
               :to-equal t)
       (eca-chat--expandable-content-toggle "parent" t t)
       (expect (eca-chat--get-expandable-content "child") :to-be nil)
       (expect (gethash "child" eca-chat--expandable-content-storage)
               :to-be nil)
       (eca-chat--expandable-content-toggle "parent" t nil)
       (expect (overlayp (eca-chat--get-expandable-content "child"))
               :to-equal t)
       (eca-chat--expandable-content-toggle "child" t nil)
       (expect (eca-chat--expandable-content-raw "child")
               :to-equal "alphabeta")
       (expect (eca-chat-performance-test--buffer-contains-p "alphabeta")
               :to-equal t))))

  (it "updates hidden nested child specs before they are rendered"
    (eca-chat-performance-test--with-chat-buffer
     (lambda (_buf)
       (eca-chat--add-expandable-content "parent" "Parent" "parent text")
       (eca-chat--add-expandable-content "child" "Child" "alpha" "parent")
       (expect (eca-chat--get-expandable-content "child") :to-be nil)
       (eca-chat--update-expandable-content "child" nil "beta" t "parent")
       (eca-chat--expandable-content-toggle "parent" t nil)
       (expect (overlayp (eca-chat--get-expandable-content "child"))
               :to-equal t)
       (eca-chat--expandable-content-toggle "child" t nil)
       (expect (eca-chat--expandable-content-raw "child")
               :to-equal "alphabeta")
       (expect (eca-chat-performance-test--buffer-contains-p "alphabeta")
               :to-equal t))))

  (it "finalizes only the tracked response region"
    (eca-chat-performance-test--with-chat-buffer
     (lambda (_buf)
       (eca-chat--add-text-content "old response\n")
       (let ((response-start (eca-chat--content-insertion-point))
             align-call
             beautify-call)
         (eca-chat--add-text-content "| a |\n|---|\n| b |\n")
         (eca-chat--record-response-range response-start
                                          (eca-chat--content-insertion-point))
         (cl-letf (((symbol-function 'eca-table-align)
                    (lambda (from end)
                      (setq align-call (list from end))))
                   ((symbol-function 'eca-table-beautify)
                    (lambda (from end)
                      (setq beautify-call (list from end))))
                   ((symbol-function 'eca-chat--flush-fontify-range)
                    (lambda (_beg _end) nil)))
           (eca-chat--finalize-response-region))
         (expect align-call :not :to-be nil)
         (expect beautify-call :not :to-be nil)
         (expect (car align-call) :to-equal response-start)
         (expect (car beautify-call) :to-equal response-start)
         (expect (car align-call) :not :to-equal (point-min))))))

  (it "cleans up timers and markers for transient state"
    (with-temp-buffer
      (eca-chat--initialize-buffer-state)
      (setq-local eca-chat--pending-fontify-start-marker (copy-marker 1))
      (setq-local eca-chat--pending-fontify-end-marker (copy-marker 1 t))
      (setq-local eca-chat--response-start-marker (copy-marker 1))
      (setq-local eca-chat--response-end-marker (copy-marker 1 t))
      (setq-local eca-chat--spinner-timer (run-with-timer 60 nil #'ignore))
      (puthash "tool-1" (current-time) eca-chat--tool-call-elapsed-times)
      (setq-local eca-chat--tool-call-elapsed-timer
                  (run-with-timer 60 nil #'ignore))
      (setq-local eca-chat--fontify-idle-timer
                  (run-with-idle-timer 60 nil #'ignore))
      (setq-local eca-chat--table-resize-timer
                  (run-with-idle-timer 60 nil #'ignore))
      (setq-local eca-chat--modeline-timer (run-with-timer 60 nil #'ignore))
      (setq-local eca-chat--stopping-safety-timer
                  (run-with-timer 60 nil #'ignore))
      (eca-chat--cleanup-buffer-state)
      (expect eca-chat--spinner-timer :to-be nil)
      (expect eca-chat--tool-call-elapsed-timer :to-be nil)
      (expect (hash-table-count eca-chat--tool-call-elapsed-times)
              :to-equal 0)
      (expect eca-chat--fontify-idle-timer :to-be nil)
      (expect eca-chat--table-resize-timer :to-be nil)
      (expect eca-chat--modeline-timer :to-be nil)
      (expect eca-chat--stopping-safety-timer :to-be nil)
      (expect eca-chat--pending-fontify-start-marker :to-be nil)
      (expect eca-chat--pending-fontify-end-marker :to-be nil)
      (expect eca-chat--response-start-marker :to-be nil)
      (expect eca-chat--response-end-marker :to-be nil)))

  (it "activates prompt long-line mitigation for oversized prompt input"
    (eca-chat-performance-test--with-chat-buffer
     (lambda (_buf)
       (cl-letf (((symbol-function 'font-lock-mode)
                  (lambda (arg)
                    (setq-local font-lock-mode (> arg 0)))))
         (let ((eca-chat-prompt-long-line-threshold 10))
           (setq-local font-lock-mode t)
           (setq-local truncate-lines nil)
           (setq-local word-wrap t)
           (eca-chat--set-prompt "12345678901")
           (eca-chat--refresh-prompt-long-line-mitigation)
           (expect eca-chat--prompt-long-line-mitigation-active
                   :to-equal t)
           (expect font-lock-mode :to-be nil)
           (expect truncate-lines :to-equal t)
           (expect word-wrap :to-be nil))))))

  (it "activates mitigation from prompt after-change hooks"
    (eca-chat-performance-test--with-chat-buffer
     (lambda (_buf)
       (cl-letf (((symbol-function 'font-lock-mode)
                  (lambda (arg)
                    (setq-local font-lock-mode (> arg 0)))))
         (let ((eca-chat-prompt-long-line-threshold 10))
           (setq-local font-lock-mode t)
           (setq-local truncate-lines nil)
           (setq-local word-wrap t)
           (add-hook 'after-change-functions #'eca-chat--prompt-after-change nil t)
           (goto-char (point-max))
           (insert "12345678901")
           (expect eca-chat--prompt-long-line-mitigation-active
                   :to-equal t)
           (expect font-lock-mode :to-be nil)
           (expect truncate-lines :to-equal t)
           (expect word-wrap :to-be nil))))))

  (it "restores prompt fontification after prompt becomes short again"
    (eca-chat-performance-test--with-chat-buffer
     (lambda (_buf)
       (cl-letf (((symbol-function 'font-lock-mode)
                  (lambda (arg)
                    (setq-local font-lock-mode (> arg 0)))))
         (let ((eca-chat-prompt-long-line-threshold 10))
           (setq-local font-lock-mode t)
           (setq-local truncate-lines nil)
           (setq-local word-wrap t)
           (eca-chat--set-prompt "12345678901")
           (eca-chat--refresh-prompt-long-line-mitigation)
           (eca-chat--set-prompt "short")
           (eca-chat--refresh-prompt-long-line-mitigation)
           (expect eca-chat--prompt-long-line-mitigation-active
                   :to-be nil)
           (expect font-lock-mode :to-equal t)
           (expect truncate-lines :to-be nil)
           (expect word-wrap :to-equal t))))))

  (it "resets performance state and prompt overlays on clear"
    (eca-chat-performance-test--with-chat-buffer
     (lambda (_buf)
       (let ((eca-chat-prompt-long-line-threshold 10))
         (setq-local font-lock-mode t)
         (eca-chat--set-prompt "12345678901")
         (eca-chat--refresh-prompt-long-line-mitigation))
       (eca-chat--tool-call-prepare-append "tool-1" "alpha")
       (eca-chat--add-expandable-content "block-1" "Tool call" "alpha")
       (eca-chat--schedule-fontify-range 1 2)
       (eca-chat--record-response-range 1 2)
       (eca-chat--clear "hello")
       (expect (hash-table-count eca-chat--tool-call-prepare-content-cache)
               :to-equal 0)
       (expect (hash-table-count eca-chat--expandable-content-index)
               :to-equal 0)
       (expect (hash-table-count eca-chat--expandable-content-storage)
               :to-equal 0)
       (expect eca-chat--fontify-idle-timer :to-be nil)
       (expect eca-chat--response-start-marker :to-be nil)
       (expect eca-chat--prompt-long-line-mitigation-active :to-be nil)
       (expect (overlayp (eca-chat--prompt-area-ov)) :to-equal t)
       (expect (overlayp (eca-chat--loading-area-ov)) :to-equal t)
       (expect (eca-chat-performance-test--buffer-contains-p "hello")
               :to-equal t))))

  (it "refreshes structural overlay caches when cached overlays die"
    (eca-chat-performance-test--with-chat-buffer
     (lambda (_buf)
       (let* ((original (eca-chat--loading-area-ov))
              (start (overlay-start original))
              (end (overlay-end original)))
         (delete-overlay original)
         (let ((replacement (make-overlay start end (current-buffer))))
           (overlay-put replacement 'eca-chat-loading-area t)
           (expect (eq (eca-chat--loading-area-ov) replacement) :to-equal t)
           (expect (eq eca-chat--loading-area-overlay replacement)
                   :to-equal t)
           (delete-overlay replacement))))))

  (it "resets loading state without leaking timers"
    (eca-chat-performance-test--with-chat-buffer
     (lambda (_buf)
       (eca-chat--schedule-fontify-range 1 2)
       (eca-chat--record-response-range 1 2)
       (eca-chat--set-chat-loading nil t)
       (expect eca-chat--response-start-marker :to-be nil)
       (expect eca-chat--pending-fontify-start-marker :to-be nil)
       (expect (timerp eca-chat--modeline-timer) :to-equal t)
       (eca-chat--set-chat-loading nil 'stopping)
       (expect eca-chat--modeline-timer :to-be nil)
       (expect (timerp eca-chat--stopping-safety-timer) :to-equal t)
       (eca-chat--set-chat-loading nil nil)
       (expect eca-chat--stopping-safety-timer :to-be nil)
       (expect eca-chat--response-start-marker :to-be nil)
       (expect eca-chat--pending-fontify-start-marker :to-be nil))))

  (it "beautifies tables with pipe text properties, not pipe overlays"
    (with-temp-buffer
      (markdown-mode)
      (setq-local eca-chat-table-beautify t)
      (insert "| a | b |\n")
      (insert "|---|---|\n")
      (insert "| 1 | 2 |\n")
      (insert "| 3 | 4 |\n")
      (insert "| 5 | 6 |\n")
      (eca-table-beautify (point-min) (point-max))
      (expect (eca-chat-performance-test--count-pipe-face-props)
              :to-equal (eca-chat-performance-test--count-pipes))
      (expect (eca-chat-performance-test--overlay-count
               (lambda (ov) (overlay-get ov 'eca-table-overlay)))
              :to-equal 4)
      (eca-table-remove-overlays (point-min) (point-max))
      (expect (eca-chat-performance-test--count-pipe-face-props)
              :to-equal 0)
      (expect (eca-chat-performance-test--overlay-count
               (lambda (ov) (overlay-get ov 'eca-table-overlay)))
              :to-equal 0))))

(provide 'eca-chat-performance-test)
;;; eca-chat-performance-test.el ends here
