;;; eca-rewrite.el --- ECA (Editor Code Assistant) rewrite -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) rewrite feature.
;;
;;; Code:

(require 'eca-util)
(require 'eca-api)

(defcustom eca-rewrite-prompt-prefix ""
  "The prefix to append to the prompt instructions sent to LLM."
  :group 'eca
  :type 'string)


(defcustom eca-rewrite-finished-action nil
  "Action to take after finishing rewriting a text region using ECA."
  :group 'eca
  :type '(choice
          (const :tag "Wait user interaction" nil)
          (const :tag "Present choices menu" menu)
          (const :tag "Accept" accept)
          (const :tag "Diff" diff)))

(defface eca-rewrite-highlight-face
  '((((class color) (min-colors 88) (background dark))
     :background "#041117" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "light goldenrod yellow" :extend t)
    (t :inherit secondary-selection))
  "Face to highlight pending rewrite regions."
  :group 'eca)

(defface eca-rewrite-in-progress-prefix-face
  '((t (:inherit shadow)))
  "Face to show in the in progress prefix text."
  :group 'eca)

(defface eca-rewrite-ready-prefix-face
  '((t (:foreground "turquoise" :bold t)))
  "Face to show in the ready prefix text."
  :group 'eca)

(defface eca-rewrite-model-face
  '((t (:inherit shadow)))
  "Face to show in the model text for rewrites."
  :group 'eca)

;; Internal

(defvar eca-rewrite--last-prompt nil)
(defvar eca-rewrite--id->buffer '())

(defvar-local eca-rewrite--overlays nil
  "Active rewrite overlays in this buffer.")

(defvar-keymap eca-rewrite-actions-map
  :doc "Keymap for rewrite overlay actions."
  "RET" #'eca-rewrite--dispatch
  "<return>" #'eca-rewrite--dispatch
  "C-c C-a" #'eca-rewrite--accept-at-point
  "C-c C-r" #'eca-rewrite-reject-at-point
  "C-c C-d" #'eca-rewrite--diff
  "C-c C-m" #'eca-rewrite--merge
  "C-c C-t" #'eca-rewrite--retry)

(defun eca-rewrite--get-buffer (id)
  "Get buffer for rewrite ID."
  (eca-get eca-rewrite--id->buffer id))

(defun eca-rewrite--overlay-at-point (&optional pt)
  "Return the overlay at PT or point if found."
  (--first (overlay-get it 'eca-rewrite--id)
           (overlays-in (line-beginning-position) (or pt (point)))))

(defun eca-rewrite--overlay-from-id (id)
  "Return the overlay with ID."
  (--first (string= id (overlay-get it 'eca-rewrite--id))
           (overlays-in (point-min) (point-max))))

(defun eca-rewrite--overlay-menu-str (ov label)
  "Return the before-string for overlay OV.
LABEL is the base text prefix."
  (let ((model-str (concat "[" (overlay-get ov 'eca-rewrite--model) "]\n")))
    (concat
     (unless (eq (char-before (overlay-start ov)) ?\n)
       "\n")
     label
     (propertize " " 'display `(space :align-to (- right ,(1+ (length model-str)))))
     (propertize model-str 'face 'eca-rewrite-model-face))))

(defun eca-rewrite--setup-overlay (id start end text prompt model)
  "Create an overlay for ID from START to END.
TEXT is the original selected text
PROMPT is the instructions user sent
MODEL is the LLM model used."
  (let ((ov (make-overlay start end nil t)))
    (overlay-put ov 'category 'eca-rewrite)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'eca-rewrite--id id)
    (overlay-put ov 'eca-rewrite--original-text text)
    (overlay-put ov 'eca-rewrite--new-text nil)
    (overlay-put ov 'eca-rewrite--temp-buffer (generate-new-buffer "*eca-rewrite*"))
    (overlay-put ov 'eca-rewrite--last-pos nil)
    (overlay-put ov 'eca-rewrite--prompt prompt)
    (overlay-put ov 'eca-rewrite--model model)
    (overlay-put ov 'face 'eca-rewrite-highlight-face)
    (overlay-put ov 'priority 2000)
    (overlay-put ov 'keymap eca-rewrite-actions-map)
    (overlay-put ov 'help-echo "ECA rewrite")
    (overlay-put ov 'before-string (eca-rewrite--overlay-menu-str
                                    ov
                                    (propertize "ECA rewriting..." 'face 'eca-rewrite-in-progress-prefix-face)))
    (push ov eca-rewrite--overlays)
    ov))

(defun eca-rewrite--show-actions (ov)
  "Display the ready action menu for rewrite overlay OV.

This updates the overlay header to show available
keys (accept/reject/merge/diff),briefly highlights the
region, and disposes of the temporary buffer used during streaming."
  (let ((choices '((?a "accept") (?r "reject") (?m "merge") (?d "diff"))))
    (overlay-put
     ov
     'before-string (eca-rewrite--overlay-menu-str
                     ov
                     (concat
                      (propertize "ECA rewrite: " 'face 'eca-rewrite-ready-prefix-face)
                      (when (fboundp #'rmc--add-key-description) ;; > Emacs 29
                        (mapconcat (lambda (e) (cdr e))
                                   (mapcar #'rmc--add-key-description choices)
                                   ", ")))))
    (pulse-momentary-highlight-region (overlay-start ov) (overlay-end ov))
    (kill-buffer (overlay-get ov 'eca-rewrite--temp-buffer))))

(defun eca-rewrite--reject (ovs)
  "Reject the rewrite overlays OVS."
  (dolist (ov (ensure-list ovs))
    (setq eca-rewrite--overlays (delq ov eca-rewrite--overlays))
    (delete-overlay ov)))

(defun eca-rewrite--accept (ov)
  "Accept rewrite overlay OV."
  (let ((ov-buf (overlay-buffer ov)))
    (with-current-buffer ov-buf
      (goto-char (overlay-start ov))
      (delete-region (overlay-start ov) (overlay-end ov))
      (insert (overlay-get ov 'eca-rewrite--new-text)))))

(defun eca-rewrite--send (session text path start end prompt)
  "Send rewrite request for SESSION and TEXT with PROMPT.
Creating overlay at START to END.
PATH is the optional file path."
  (let ((start-line (line-number-at-pos start))
        (end-line (line-number-at-pos end))
        (start-char (save-excursion
                      (goto-char start)
                      (current-column)))
        (end-char (save-excursion
                    (goto-char end)
                    (current-column)))
        (id (number-to-string (random 100000000))))
    (eca-api-request-async
     session
     :method "rewrite/prompt"
     :params (list :id id
                   :text text
                   :prompt prompt
                   :path path
                   :range (list :start (list :line start-line :character start-char)
                                :end (list :line end-line :character end-char)))
     :success-callback
     (lambda (res)
       (eca-info "Rewriting...")
       (setq eca-rewrite--id->buffer (eca-assoc eca-rewrite--id->buffer id (current-buffer)))
       (eca-rewrite--setup-overlay id start end text prompt (plist-get res :model)))
     :error-callback
     (lambda (err) (eca-error "Rewrite error: %s" err)))))

(defun eca-rewrite--add-text (ov text)
  "Add TEXT as the rewritten content to overlay OV.
This updates the overlay's associated temporary buffer with the new text,
sets overlay properties, and ensures display reflects the latest content."
  (let ((ov-buf (overlay-buffer ov))
        (temp-buf (overlay-get ov 'eca-rewrite--temp-buffer)))
    (with-current-buffer temp-buf
      (let ((inhibit-modification-hooks nil)
            (inhibit-read-only t))
        (when (= 0 (buffer-size))
          (buffer-disable-undo)
          (insert-buffer-substring ov-buf (overlay-start ov) (overlay-end ov))
          (delay-mode-hooks (funcall (buffer-local-value 'major-mode ov-buf)))
          (add-text-properties (point-min) (point-max) '(face shadow font-lock-face shadow))
          (goto-char (point-min)))
        (insert text)
        (unless (eobp)
          (ignore-errors (delete-char (length text))))
        (font-lock-ensure))
      (overlay-put ov 'eca-rewrite--new-text (buffer-string))
      (overlay-put ov 'display (buffer-string)))))

;; Public

(defun eca-rewrite-content-received (_session params)
  "Handle content update for a rewrite operation from LLM.
SESSION is the ECA session.
PARAMS is a plist containing:
  :rewriteId -- the identifier of the rewrite overlay,
  :content   -- a plist with at least a :type and optionally :text.

Update the corresponding overlay with content (partial or finished),
accepts, shows menu, or diffs according to `eca-rewrite-finished-action',
 or prompts user if not set."
  (let* ((id (plist-get params :rewriteId))
         (content (plist-get params :content))
         (buffer (eca-rewrite--get-buffer id)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((ov (eca-rewrite--overlay-from-id id)))
          (pcase (plist-get content :type)
            ("text" (eca-rewrite--add-text ov (plist-get content :text)))
            ("finished" (if eca-rewrite-finished-action
                            (pcase eca-rewrite-finished-action
                              ('accept (eca-rewrite--accept ov))
                              ('diff (eca-rewrite--diff ov))
                              ('menu (eca-rewrite--menu ov))
                              (_ (user-error (format "Uknown rewrite action '%s' for eca-rewrite-finished-action" eca-rewrite-finished-action))))
                          (eca-rewrite--show-actions ov)))))))))

;;;###autoload
(defun eca-rewrite (prompt)
  "Rewrite a text with a new LLM generated one.
- If on an existing rewrite overlay, re-iterate
- Else if region active, rewrite that region
- Else try to detect defun/paragraph boundaries

PROMPT is the instructions prompt for the LLM."
  (interactive
   (list (read-string "Rewrite prompt: " (or eca-rewrite--last-prompt
                                             eca-rewrite-prompt-prefix))))
  (eca-assert-session-running (eca-session))
  (let* ((session (eca-session))
         (existing-ov (eca-rewrite--overlay-at-point))
         (start (cond (existing-ov (overlay-start existing-ov))
                      ((use-region-p) (region-beginning))
                      (t (or (car (bounds-of-thing-at-point 'defun))
                             (car (bounds-of-thing-at-point 'paragraph))
                             (user-error "No region selected or existing-ov rewrite overlay")))))
         (end (cond (existing-ov (overlay-end existing-ov))
                    ((use-region-p) (region-end))
                    (t (or (cdr (bounds-of-thing-at-point 'defun))
                           (cdr (bounds-of-thing-at-point 'paragraph))
                           start))))
         (text (if existing-ov
                   (overlay-get existing-ov 'eca-text)
                 (buffer-substring-no-properties start end))))
    (setq eca-rewrite--last-prompt prompt)
    (when existing-ov
      (eca-rewrite--reject existing-ov))
    (eca-rewrite--send session text (buffer-file-name) start end prompt)
    (unless (get-char-property (point) 'eca-rewrite--id)
      (when (= (point) (region-end)) (backward-char 1)))
    (deactivate-mark)))

;;;###autoload
(defun eca-rewrite-reject-at-point ()
  "Reject rewrite overlay at point."
  (interactive)
  (when-let ((ov (eca-rewrite--overlay-at-point)))
    (eca-rewrite--reject (list ov))))

;;;###autoload
(defun eca-rewrite--accept-at-point ()
  "Accept rewrite overlay at point."
  (interactive)
  (when-let ((ov (eca-rewrite--overlay-at-point)))
    (eca-rewrite--accept ov)))


(provide 'eca-rewrite)
;;; eca-rewrite.el ends here
