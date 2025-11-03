;;; eca-diff.el --- Diff helpers for ECA -*- lexical-binding: t; -*-
;;
;; Utilities to show unified diffs via text, ediff or smerge.
;; This file is extracted from eca-chat.el to keep diff rendering
;; logic isolated and reusable.
;;
;;; Commentary:
;;
;; Diff helpers for ECA: functions to parse and
;; display unified diffs via text, ediff or smerge.
;; This file is extracted from eca-chat.el to keep diff rendering
;; logic isolated and reusable.
;;
;;; Code:

(require 'f)
(require 'ediff)
(require 'smerge-mode)
(require 'seq)
(require 'subr-x)

(defun eca-diff-parse-unified-diff (diff-text)
  "Parse DIFF-TEXT and return a plist with :original and :new strings.
Only hunks (lines between @@ ... @@) are considered for the content.
This mirrors the original parser used by the chat UI."
  (let ((orig '()) (new '()) in-hunk)
    (dolist (l (split-string diff-text "\n"))
      (cond
       ((string-match "^@@.*@@$" l) (setq in-hunk t))
       ((and in-hunk (string-prefix-p " " l))
        (push (substring l 1) orig) (push (substring l 1) new))
       ((and in-hunk (string-prefix-p "-" l))
        (push (substring l 1) orig))
       ((and in-hunk (string-prefix-p "+" l))
        (push (substring l 1) new))))
    (list :original (string-join (nreverse orig) "\n")
          :new      (string-join (nreverse new) "\n"))))

(defun eca-diff--default-redisplay-fn (chat-buf)
  "Default redisplay function used when a chat RE-DISPLAY-FN is not provided.
Shows CHAT-BUF using `display-buffer' if it's still alive."
  (when (buffer-live-p chat-buf)
    (ignore-errors (display-buffer chat-buf))))

(defun eca-diff-show-ediff (path diff &optional chat-buf redisplay-fn _)
  "Show DIFF for file at PATH using Ediff side-by-side in windows.
If CHAT-BUF is provided it will be used to attempt to re-display the
chat buffer after Ediff quits.  If REDISPLAY-FN is provided it will be
called with CHAT-BUF to perform that re-display (default: display-buffer).
ROOTS may be passed for path relativization if desired.

This function tries to be Doom-compatible when Emacs runs Doom popup
system (it ignores those popup rules for the generated ediff buffers)."
  (let* (;; ediff expects `\n` line endings.
         (diff (replace-regexp-in-string "\r\n" "\n" diff))
         (parsed        (eca-diff-parse-unified-diff diff))
         (orig          (plist-get parsed :original))
         (new           (plist-get parsed :new))
         (buf-orig      (generate-new-buffer (format "*eca-diff-orig:%s*" path)))
         (buf-new       (generate-new-buffer (format "*eca-diff-new:%s*" path)))
         (chat-buf      chat-buf)
         (redisplay-fn  (or redisplay-fn #'eca-diff--default-redisplay-fn))
         ;; Doom-specific: Check if we're in a popup context
         (doom-popup-p   (and (boundp '+popup-buffer-mode) +popup-buffer-mode))
         (in-doom-p      (featurep 'doom))
         ;; Don't save window config if in Doom popup - causes conflicts
         (cwc            (unless doom-popup-p (current-window-configuration)))
         (orig-selected  (selected-window))
         (orig-buffer    chat-buf)
         (ediff-buffers-before
          (seq-filter (lambda (b)
                        (string-match-p "\\*\\(ediff-\\|Ediff Control\\)"
                                        (buffer-name b)))
                      (buffer-list)))
         session-ediff-buffers
         cleanup-fn
         after-setup-fn)

    ;; Doom-specific: Set popup rules to prevent interference
    (when in-doom-p
      (when (fboundp 'set-popup-rule!)
        ;; Prevent ECA diff buffers from being managed by Doom's popup system
        (set-popup-rule! "^\\*eca-diff-" :ignore t)
        (set-popup-rule! "^\\*Ediff Control Panel" :ignore t)))

    ;; Fill temporary buffers
    (with-current-buffer buf-orig
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert orig)
        (set-buffer-modified-p nil)))
    (with-current-buffer buf-new
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert new)
        (set-buffer-modified-p nil)))

    ;; Only manipulate windows if not in Doom popup context
    (unless doom-popup-p
      ;; Temporarily relax side-window protections so Ediff can split
      (let ((frame (selected-frame)))
        (dolist (w (seq-filter (lambda (w)
                                 (and (eq (window-frame w) frame)
                                      (or (window-parameter w 'no-delete-other-windows)
                                          (window-parameter w 'window-side))))
                               (window-list)))
          (when (window-live-p w)
            (set-window-parameter w 'no-delete-other-windows nil)
            (set-window-parameter w 'window-side nil))))
      ;; Ensure Ediff has a single full window to manage
      (unless (one-window-p t)
        (delete-other-windows)))

    ;; Enhanced cleanup with Doom compatibility
    (setq cleanup-fn
          (lambda ()
            ;; Kill temp buffers first to avoid conflicts
            (when (buffer-live-p buf-orig) (kill-buffer buf-orig))
            (when (buffer-live-p buf-new)  (kill-buffer buf-new))
            ;; Kill any additional Ediff-generated buffers
            (dolist (b session-ediff-buffers)
              (when (and b (buffer-live-p b))
                (kill-buffer b)))

            ;; Doom-aware window restoration
            (cond
             ;; If we were in a Doom popup, use Doom's restoration
             (doom-popup-p
              (cond
               ((and (fboundp '+popup/restore) (buffer-live-p orig-buffer))
                (+popup/restore))
               ((buffer-live-p orig-buffer)
                (switch-to-buffer orig-buffer))))

             ;; Standard window config restoration with Doom-compatible delay
             (cwc
              ;; Use longer delay for Doom compatibility
              (run-with-timer (if in-doom-p 0.3 0.1) nil
                              (lambda ()
                                (condition-case err
                                    (progn
                                      (set-window-configuration cwc)
                                      (when (window-live-p orig-selected)
                                        (select-window orig-selected))
                                      ;; Re-display chat buffer in side window if needed
                                      (when (and (buffer-live-p chat-buf)
                                                 (or (not (get-buffer-window chat-buf t))
                                                     (not (window-parameter (get-buffer-window chat-buf t) 'window-side))))
                                        (ignore-errors (funcall redisplay-fn chat-buf))))
                                  (error
                                   (message "ECA: Could not restore window config: %s" err)
                                   ;; Fallback: ensure chat buffer is visible
                                   (when (buffer-live-p chat-buf)
                                     (ignore-errors (funcall redisplay-fn chat-buf))))))))

             ;; Fallback for cases without saved config
             (t
              (when (and (buffer-live-p chat-buf)
                         (or (not (get-buffer-window chat-buf t))
                             (not (window-parameter (get-buffer-window chat-buf t) 'window-side))))
                (ignore-errors (funcall redisplay-fn chat-buf)))))

            (remove-hook 'ediff-quit-hook cleanup-fn)))

    ;; After-setup hook: capture ediff buffers and move to first diff
    (setq after-setup-fn
          (lambda ()
            (let ((ediff-buffers-after
                   (seq-filter (lambda (b)
                                 (string-match-p "\\*\\(ediff-\\|Ediff\n Control\\)" (buffer-name b)))
                               (buffer-list))))
              (setq session-ediff-buffers
                    (seq-filter (lambda (b)
                                  (and (not (member b ediff-buffers-before))
                                       (not (string-match-p "*Ediff\n Registry*" (buffer-name b)))))
                                ediff-buffers-after)))
            (condition-case _err
                (progn
                  (setq ediff-current-difference -1)
                  (ediff-next-difference))
              (error nil))
            (remove-hook 'ediff-after-setup-windows-hook after-setup-fn)))

    ;; Install hooks
    (add-hook 'ediff-quit-hook cleanup-fn)
    (add-hook 'ediff-after-setup-windows-hook after-setup-fn)

    ;; Start Ediff with error handling
    (condition-case err
	(let ((ediff-window-setup-function
	       (if (daemonp)
		   'ediff-setup-windows-plain
		 ediff-window-setup-function)))
        (ediff-buffers buf-orig buf-new))
      (error
       ;; On error, remove hooks and restore windows
       (remove-hook 'ediff-quit-hook cleanup-fn)
       (remove-hook 'ediff-after-setup-windows-hook after-setup-fn)

       ;; Doom-aware error recovery
       (cond
        (cwc (set-window-configuration cwc))
        (doom-popup-p
         (when (buffer-live-p orig-buffer)
           (switch-to-buffer orig-buffer))))

       ;; Re-display chat buffer on error
       (when (and (buffer-live-p chat-buf)
                  (or (not (get-buffer-window chat-buf t))
                      (not (window-parameter (get-buffer-window chat-buf t) 'window-side))))
         (ignore-errors (funcall redisplay-fn chat-buf)))

       ;; Kill temp buffers
       (when (buffer-live-p buf-orig) (kill-buffer buf-orig))
       (when (buffer-live-p buf-new)  (kill-buffer buf-new))
       (message "eca-diff: error starting ediff: %s" err)))))

(defun eca-diff-show-smerge (path diff &optional chat-buf redisplay-fn _)
  "Show DIFF for file at PATH using Smerge in a dedicated window.
If CHAT-BUF is provided, REDISPLAY-FN (called with CHAT-BUF) will be used
to re-show the chat window when the smerge buffer is killed."
  (let* ((parsed (eca-diff-parse-unified-diff diff))
         (orig   (plist-get parsed :original))
         (new    (plist-get parsed :new))
         (buf    (generate-new-buffer (format "*eca-smerge:%s*" path)))
         (cwc    (current-window-configuration))
         (orig-selected (selected-window))
         (frame  (selected-frame))
         (cleanup-running nil)
         cleanup-fn
         window-config-hook
         (redisplay-fn (or redisplay-fn #'eca-diff--default-redisplay-fn)))

    ;; Fill buffer with conflict markers
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (concat "<<<<<<< Original\n" orig "\n=======\n" new "\n>>>>>>> New\n"))
        (diff-mode)
        (smerge-mode 1)
        (goto-char (point-min))))

    ;; Define cleanup that restores window configuration and kills the buffer
    (setq cleanup-fn
          (lambda ()
            (unless cleanup-running
              (setq cleanup-running t)
              (when (window-configuration-p cwc)
                (ignore-errors (set-window-configuration cwc)))
              (when (window-live-p orig-selected)
                (select-window orig-selected))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (remove-hook 'kill-buffer-hook cleanup-fn t)))
              (when (functionp window-config-hook)
                (remove-hook 'window-configuration-change-hook window-config-hook))
              (when (buffer-live-p buf)
                (kill-buffer buf))
              ;; Re-display chat buffer if needed
              (when (and (buffer-live-p chat-buf)
                         (or (not (get-buffer-window chat-buf t))
                             (not (window-parameter (get-buffer-window chat-buf t) 'window-side))))
                (ignore-errors (funcall redisplay-fn chat-buf))))))

    ;; If buffer is no longer visible, run cleanup
    (setq window-config-hook
          (lambda ()
            (unless (get-buffer-window buf t)
              (funcall cleanup-fn))))
    (add-hook 'window-configuration-change-hook window-config-hook)

    ;; Add local keymap and kill hook to the smerge buffer
    (with-current-buffer buf
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map (current-local-map))
        (define-key map (kbd "q") (lambda () (interactive) (funcall cleanup-fn)))
        (use-local-map map))
      (add-hook 'kill-buffer-hook cleanup-fn nil t))

    ;; Temporarily relax side-window protections in the current frame so
    ;; `delete-other-windows' can succeed. The original window
    ;; configuration is restored by `cleanup-fn' (which calls `set-window-configuration').
    (dolist (w (seq-filter (lambda (w)
                             (and (eq (window-frame w) frame)
                                  (or (window-parameter w 'no-delete-other-windows)
                                      (window-parameter w 'window-side))))
                           (window-list)))
      (when (window-live-p w)
        (set-window-parameter w 'no-delete-other-windows nil)
        (set-window-parameter w 'window-side nil)))
    ;; Present the buffer full-frame
    (when (window-live-p (frame-root-window frame))
      (select-window (frame-root-window frame)))
    (unless (one-window-p t)
      (delete-other-windows))
    (switch-to-buffer buf)

    ;; Return nil explicitly
    nil))


(provide 'eca-diff)
;;; eca-diff.el ends here
