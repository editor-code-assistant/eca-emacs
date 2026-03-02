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

(defcustom eca-rewrite-finish-prefix "Rewrite: "
  "The prefix text to show after a rewrite was done."
  :group 'eca
  :type 'string)

(defcustom eca-rewrite-diff-tool 'ediff
  "Diff tool to use when calling diff in a rewrite overlay."
  :group 'eca
  :type '(choice
          (const :tag "Simple diff" simple-diff)
          (const :tag "Ediff" ediff)))

(defcustom eca-rewrite-finished-action 'show-overlay-actions
  "Action to take after finishing rewriting a text region using ECA."
  :group 'eca
  :type '(choice
          (const :tag "Show actions above overlay" show-overlay-actions)
          (const :tag "Accept" accept)
          (const :tag "Diff" diff)
          (const :tag "Merge" merge)))

(defcustom eca-rewrite-on-finished-hook '()
  "Hook to run after a ECA rewrite is finished, the overlay is the arg."
  :type 'hook
  :group 'eca)

(defface eca-rewrite-overlay-face
  '((((class color) (min-colors 88) (background dark))
     :background "#041117" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "light goldenrod yellow" :extend t)
    (t :inherit secondary-selection))
  "Face to highlight pending rewrite regions."
  :group 'eca)

(defface eca-rewrite-overlay-hover-face
  '((t (:inherit eca-rewrite-overlay-face :weight semi-bold)))
  "Face used for rewrite overlay content when point is inside it."
  :group 'eca)

(defface eca-rewrite-in-progress-prefix-face
  '((t (:inherit shadow)))
  "Face to show in the in progress prefix text."
  :group 'eca)

(defface eca-rewrite-ready-prefix-face
  '((((background dark))  (:foreground "turquoise" :bold t))
    (((background light)) (:foreground "dark cyan" :bold t)))
  "Face to show in the ready prefix text."
  :group 'eca)

(defface eca-rewrite-model-face
  '((t (:inherit shadow)))
  "Face to show in the model text for rewrites."
  :group 'eca)

;; Internal

(declare-function diff-no-select "diff")

(defvar eca-rewrite--last-prompt nil)
(defvar eca-rewrite--id->buffer '())

(defvar-local eca-rewrite--overlays nil
  "Active rewrite overlays in this buffer.")

(defvar-local eca-rewrite--hovered-ov nil
  "Currently hovered rewrite overlay (point inside), or nil.")

(defvar-local eca-rewrite--restore-paren-mode-after-clean nil)

(defvar-keymap eca-rewrite-actions-map
  :doc "Keymap for rewrite overlay actions."
  "a" #'eca-rewrite-accept
  "r" #'eca-rewrite-reject
  "d" #'eca-rewrite-diff
  "m" #'eca-rewrite-merge
  "t" #'eca-rewrite-retry)

(defun eca-rewrite--get-buffer (id)
  "Get buffer for rewrite ID."
  (eca-get eca-rewrite--id->buffer id))

(defun eca-rewrite--overlay-at-point (&optional pt)
  "Return the overlay at PT or point if found."
  (--first (overlay-get it 'eca-rewrite--id)
           (overlays-in (line-beginning-position) (or pt (1+ (point))))))

(defun eca-rewrite--overlay-from-id (id)
  "Return the overlay with ID."
  (--first (string= id (overlay-get it 'eca-rewrite--id))
           (overlays-in (point-min) (point-max))))

(defun eca-rewrite--time->presentable-time (ms)
  "Return a presentable time for MS."
  (let ((secs (/ (float ms) 1000)))
    (format "%.2f s" secs)))

(defun eca-rewrite--normalize-start-region (start)
  "Normalize START region.
If chars from START until bol are all spaces return bol,
otherwise START."
  (save-excursion
    (goto-char start)
    (let ((bol (line-beginning-position)))
      (if (string-match-p "\\`[ \t]*\\'"
                          (buffer-substring bol start))
          bol
        start))))

(defun eca-rewrite--overlay-menu-str (ov label)
  "Return the before-string for overlay OV.
LABEL is the base text prefix."
  (let* ((ms (overlay-get ov 'eca-rewrite--total-time-ms))
         (model-str (concat
                     (when ms
                       (concat "[" (eca-rewrite--time->presentable-time ms) "] "))
                     "[" (overlay-get ov 'eca-rewrite--model) "]\n")))
    (concat
     (unless (eq (char-before (overlay-start ov)) ?\n)
       "\n")
     label
     (propertize " " 'display `(space :align-to (- right ,(1+ (length model-str)))))
     (propertize model-str 'face 'eca-rewrite-model-face))))

(defun eca-rewrite--apply-display-face (ov face)
  "Apply FACE to OV's display string, preserving syntax faces.
If the overlay is currently hidden (no display), do nothing."
  (when (and (overlay-get ov 'display)
             (overlay-get ov 'eca-rewrite--display-base))
    (let* ((base (overlay-get ov 'eca-rewrite--display-base))
           (disp (copy-sequence base)))
      (add-face-text-property 0 (length disp) face 'append disp)
      (overlay-put ov 'display disp))))

(defun eca-rewrite--apply-normal-face (ov)
  "Restore normal highlight face to OV."
  (if (overlay-get ov 'eca-rewrite--display-base)
      (eca-rewrite--apply-display-face ov 'eca-rewrite-overlay-face)
    (overlay-put ov 'face 'eca-rewrite-overlay-face)))

(defun eca-rewrite--apply-hover-face (ov)
  "Apply hover face to OV."
  (if (overlay-get ov 'eca-rewrite--display-base)
      (eca-rewrite--apply-display-face ov 'eca-rewrite-overlay-hover-face)
    (overlay-put ov 'face 'eca-rewrite-overlay-hover-face)))

(defun eca-rewrite--hover-update ()
  "Update hover styling based on point location."
  (let ((current-ov (eca-rewrite--overlay-at-point)))
    (unless (eq current-ov eca-rewrite--hovered-ov)
      ;; Remove hover face from previous overlay
      (when (overlayp eca-rewrite--hovered-ov)
        (eca-rewrite--apply-normal-face eca-rewrite--hovered-ov))
      ;; Update hovered overlay reference
      (setq eca-rewrite--hovered-ov current-ov)
      ;; Apply hover face to the new overlay, if present
      (when (overlayp current-ov)
        (eca-rewrite--apply-hover-face current-ov)))))

(defun eca-rewrite--setup-overlay (id start end text path prompt model)
  "Create an overlay for ID from START to END.
TEXT is the original selected text
PATH is the buffer file name
PROMPT is the instructions user sent
MODEL is the LLM model used."
  (let ((ov (make-overlay start end nil t)))
    (overlay-put ov 'category 'eca-rewrite)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'eca-rewrite--id id)
    (overlay-put ov 'eca-rewrite--original-text text)
    (overlay-put ov 'eca-rewrite--new-text nil)
    (overlay-put ov 'eca-rewrite--new-text-acc "")
    (overlay-put ov 'eca-rewrite--temp-buffer (generate-new-buffer "*eca-rewrite*"))
    (overlay-put ov 'eca-rewrite--path path)
    (overlay-put ov 'eca-rewrite--prompt prompt)
    (overlay-put ov 'eca-rewrite--model model)
    (overlay-put ov 'face 'eca-rewrite-overlay-face)
    (overlay-put ov 'priority 2000)
    (overlay-put ov 'keymap eca-rewrite-actions-map)
    (overlay-put ov 'help-echo "ECA rewrite")
    (eca-rewrite--refresh-overlay-actions ov 'started)
    (push ov eca-rewrite--overlays)
    (add-hook 'post-command-hook #'eca-rewrite--hover-update nil t)
    ov))

(defun eca-rewrite--refresh-overlay-actions (ov status)
  "Display the actions overlay above rewrite overlay OV given STATUS."
  (let ((choices (pcase status
                   ('started '((?r "reject")))
                   ('reasoning '((?r "reject")))
                   ('finished '((?a "accept") (?r "reject") (?d "diff") (?m "merge") (?t "retry")))))
        (label (pcase status
                 ('started (propertize "Requesting LLM... " 'face 'eca-rewrite-in-progress-prefix-face))
                 ('reasoning (propertize "LLM reasoning... " 'face 'eca-rewrite-in-progress-prefix-face))
                 ('finished (propertize eca-rewrite-finish-prefix 'face 'eca-rewrite-ready-prefix-face)))))
    (overlay-put
     ov
     'before-string (eca-rewrite--overlay-menu-str
                     ov
                     (concat
                      label
                      (when (fboundp #'rmc--add-key-description) ;; > Emacs 29
                        (mapconcat (lambda (e) (cdr e))
                                   (mapcar #'rmc--add-key-description choices)
                                   ", ")))))))

(defun eca-rewrite--reject (ovs)
  "Reject (discard) one or more rewrite overlays.

OVS may be a single overlay or a list of overlays. For each
overlay, remove it from the internal tracking list
`eca-rewrite--overlays' and delete it with `delete-overlay'.
No confirmation is asked; a nil OVS is ignored. Returns nil."
  (when eca-rewrite--restore-paren-mode-after-clean
    (setq eca-rewrite--restore-paren-mode-after-clean nil)
    (show-paren-mode 1))
  (dolist (ov (ensure-list ovs))
    (setq eca-rewrite--overlays (delq ov eca-rewrite--overlays))
    (when (eq ov eca-rewrite--hovered-ov)
      (setq eca-rewrite--hovered-ov nil))
    (delete-overlay ov))
  (when (null eca-rewrite--overlays)
    (remove-hook 'post-command-hook #'eca-rewrite--hover-update t)
    (setq eca-rewrite--hovered-ov nil)))

(defun eca-rewrite--accept (ov)
  "Accept rewrite overlay OV."
  (when eca-rewrite--restore-paren-mode-after-clean
    (setq eca-rewrite--restore-paren-mode-after-clean nil)
    (show-paren-mode 1))
  (let ((ov-buf (overlay-buffer ov)))
    (with-current-buffer ov-buf
      (goto-char (overlay-start ov))
      (delete-region (overlay-start ov) (overlay-end ov))
      (insert (overlay-get ov 'eca-rewrite--new-text)))))

(defun eca-rewrite--clone-buffer-with-rewrite (ov name)
  "Return a clone of the buffer NAME with the rewrite overlay OV applied."
  (let ((buf (overlay-buffer ov)))
    (with-current-buffer buf
      (let ((min (point-min))
            (max (point-max))
            (pt  (point))
            (newb (get-buffer-create name)))
        (save-restriction
          (widen)
          (with-current-buffer newb
            (erase-buffer)
            (insert-buffer-substring buf)))
        (with-current-buffer newb
          (narrow-to-region min max)
          (goto-char pt)
          (eca-rewrite--accept ov))
        newb))))

(defun eca-rewrite--merge-simple (beg end new-text model)
  "Produce a merge conflict region between BEG and END for NEW-TEXT.
MODEL is the model used."
  (goto-char end)
  (unless (bolp) (insert "\n"))
  (insert "=======\n" new-text "\n>>>>>>> " model)
  (goto-char beg)
  (unless (bolp)
    (insert "\n"))
  (insert-before-markers "<<<<<<< original\n"))

(defun eca-rewrite--merge-git (beg end new-text model)
  "Produce a merge conflict region between BEG and END.

Merge the region with NEW-TEXT using git merge-file.
MODEL is the model used."
  (let ((original-temp-file (make-temp-file "eca-rewrite-merge-"))
        (empty-temp-file (make-temp-file "eca-rewrite-merge-"))
        (new-temp-file (make-temp-file "eca-rewrite-merge-")))
    (unwind-protect
        (progn
          (write-region beg end original-temp-file)
          (with-temp-file empty-temp-file (insert ""))
          (with-temp-file new-temp-file (insert new-text))
          (goto-char beg)
          (delete-region beg end)
          (call-process
           "git" nil (list (current-buffer) nil) nil
           "merge-file" "--no-diff3" "-L" "original" "-L" "Empty" "-L"
           model "-p"
           original-temp-file empty-temp-file new-temp-file)
          (goto-char beg))
      (delete-file original-temp-file)
      (delete-file empty-temp-file)
      (delete-file new-temp-file))))

(defun eca-rewrite--merge (ov)
  "Merge between original text and rewrite overlay OV."
  (when-let* ((ov-buf (overlay-buffer ov))
              ((buffer-live-p ov-buf)))
    (with-current-buffer ov-buf
      (let ((model (overlay-get ov 'eca-rewrite--model))
            (changed))
        (save-excursion
          (when-let* ((new-text (overlay-get ov 'eca-rewrite--new-text)))
            (if (executable-find "git")
                (eca-rewrite--merge-git (overlay-start ov) (overlay-end ov) new-text model)
              (eca-rewrite--merge-simple (overlay-start ov) (overlay-end ov) new-text model))
            (setq changed t)))
        (when changed (smerge-mode 1)))
      (eca-rewrite--reject (list ov)))))

(defun eca-rewrite--simple-diff (ov)
  "Show diff between original text and rewrite overlay OV."
  (let* ((ov-buf (overlay-buffer ov))
         (newbuf (eca-rewrite--clone-buffer-with-rewrite ov "*eca-rewrite-diff*"))
         (diff-buf (diff-no-select ov-buf newbuf)))
    (with-current-buffer diff-buf
      (setq-local diff-jump-to-old-file t))
    (display-buffer diff-buf)))

(defun eca-rewrite--ediff (ov)
  "Show diff between original text and rewrite overlay OV."
  (letrec ((ov-buf (overlay-buffer ov))
           (newbuf (eca-rewrite--clone-buffer-with-rewrite ov "*eca-rewrite-ediff*"))
           (orig-win (current-window-configuration))
           (hide-show-fn (lambda (&optional restore)
                           (let ((disp (overlay-get ov 'display))
                                 (stored (overlay-get ov 'eca-rewrite--ediff)))
                             ;; Do not toggle overlay-level face; the highlight is carried by the display string.
                             (overlay-put ov 'display (and restore stored))
                             (overlay-put ov 'eca-rewrite--ediff (unless restore disp)))))
           (on-quit-fn (lambda ()
                         (when (window-configuration-p orig-win)
                           (set-window-configuration orig-win))
                         (funcall hide-show-fn t)
                         (remove-hook 'ediff-quit-hook on-quit-fn))))
    (funcall hide-show-fn)
    (add-hook 'ediff-quit-hook on-quit-fn t)
    (ediff-buffers ov-buf newbuf)))

(defun eca-rewrite--diff (ov)
  "Show diff between original text and rewrite overlay OV."
  (pcase eca-rewrite-diff-tool
    ('simple-diff (eca-rewrite--simple-diff ov))
    ('ediff (eca-rewrite--ediff ov))
    (_ (user-error (concat "Unknown diff tool: " eca-rewrite-diff-tool)))))

(defun eca-rewrite--retry (ov session)
  "Retry rewrite overlay OV for SESSION."
  (let ((text (overlay-get ov 'eca-rewrite--original-text))
        (path (overlay-get ov 'eca-rewrite--path))
        (prompt (overlay-get ov 'eca-rewrite--prompt))
        (start (overlay-start ov))
        (end (overlay-end ov)))
    (eca-rewrite--rewrite ov session text path start end prompt)))

(defun eca-rewrite--rewrite (ov session text path start end prompt)
  "Start the rewrite for OV, SESSION, TEXT, PATH, START, END and PROMPT."
  (setq eca-rewrite--last-prompt prompt)
  (when ov (eca-rewrite--reject ov))
  (eca-rewrite--send session text path start end prompt)
  (goto-char start)
  (deactivate-mark))

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
       (eca-rewrite--setup-overlay id start end text path prompt (plist-get res :model)))
     :error-callback
     (lambda (err)
       (eca-error "Rewrite error: %s" (plist-get err :message))))))

(defun eca-rewrite--add-text (ov text)
  "Add TEXT as the rewritten content to overlay OV.
This replaces the overlay preview entirely with the accumulated new text,
so shorter rewrites don't leave leftover original text in the overlay."
  (let* ((ov-buf (overlay-buffer ov))
         (temp-buf (overlay-get ov 'eca-rewrite--temp-buffer))
         (mode (and ov-buf (buffer-local-value 'major-mode ov-buf)))
         (acc (concat (overlay-get ov 'eca-rewrite--new-text-acc) text)))
    (overlay-put ov 'eca-rewrite--new-text-acc acc)
    (with-current-buffer ov-buf
      (setq eca-rewrite--restore-paren-mode-after-clean t)
      (when show-paren-mode
        (show-paren-mode -1)))
    (with-current-buffer temp-buf
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (erase-buffer)
        (insert acc)
        (when mode
          (delay-mode-hooks (funcall mode)))
        (font-lock-ensure))
      (let ((plain (buffer-substring-no-properties (point-min) (point-max)))
            (propertized (buffer-substring (point-min) (point-max))))
        (overlay-put ov 'eca-rewrite--new-text plain)
        ;; Move the highlight to the display string so show-paren-mode
        ;; overlays on buffer text don't repaint the whole overlay.
        (let ((disp (copy-sequence propertized)))
          ;; Keep a base version (syntax faces only) to compose with our faces.
          (overlay-put ov 'eca-rewrite--display-base disp)
          ;; Initial (non-hover) display uses the normal highlight face.
          (let ((initial (copy-sequence disp)))
            (add-face-text-property 0 (length initial) 'eca-rewrite-overlay-face 'append initial)
            (overlay-put ov 'display initial)))))))

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
        (when-let ((ov (eca-rewrite--overlay-from-id id)))
          (pcase (plist-get content :type)
            ("reasoning" (eca-rewrite--refresh-overlay-actions ov 'reasoning))
            ("text" (eca-rewrite--add-text ov (plist-get content :text)))
            ("error" (progn
                       (eca-rewrite--reject (list ov))
                       (eca-error "Rewrite error: %s" (plist-get content :message))))
            ("replace" (progn
                         (overlay-put ov 'eca-rewrite--new-text-acc "")
                         (eca-rewrite--add-text ov (plist-get content :text))))
            ("finished" (progn
                          (overlay-put ov 'eca-rewrite--total-time-ms (plist-get content :totalTimeMs))
                          (pcase eca-rewrite-finished-action
                            ('accept (eca-rewrite--accept ov))
                            ('diff (eca-rewrite--diff ov))
                            ('merge (eca-rewrite--merge ov))
                            ('show-overlay-actions (progn
                                                     (eca-rewrite--refresh-overlay-actions ov 'finished)
                                                     (pulse-momentary-highlight-region (overlay-start ov) (overlay-end ov))
                                                     (kill-buffer (overlay-get ov 'eca-rewrite--temp-buffer))))
                            (_ (user-error (format "Uknown rewrite action '%s' for eca-rewrite-finished-action" eca-rewrite-finished-action))))
                          (with-demoted-errors "eca-rewrite-on-finished-hook: %S"
                            (run-hook-with-args 'eca-rewrite-on-finished-hook ov))))))))))

;;;###autoload
(defun eca-rewrite (&optional prompt)
  "Rewrite a text with a new LLM generated one.
- If on an existing rewrite overlay, re-iterate
- Else if region active, rewrite that region
- Else try to detect defun/paragraph boundaries

PROMPT is the instructions prompt for the LLM."
  (interactive)
  (eca-assert-session-running (eca-session))
  (let* ((prompt (or prompt
                     (read-string "Rewrite prompt: " (or eca-rewrite--last-prompt
                                                         eca-rewrite-prompt-prefix))))
         (existing-ov (eca-rewrite--overlay-at-point))
         (start (cond (existing-ov (overlay-start existing-ov))
                      ((use-region-p) (eca-rewrite--normalize-start-region (region-beginning)))
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
    (eca-rewrite--rewrite existing-ov (eca-session) text (buffer-file-name) start end prompt)))

;;;###autoload
(defun eca-rewrite-reject ()
  "Reject rewrite overlay at point."
  (interactive)
  (if-let ((ov (eca-rewrite--overlay-at-point)))
    (eca-rewrite--reject (list ov))
    (eca-error "No rewrite overlay found at point")))

;;;###autoload
(defun eca-rewrite-accept ()
  "Accept rewrite overlay at point."
  (interactive)
  (if-let ((ov (eca-rewrite--overlay-at-point)))
    (eca-rewrite--accept ov)
    (eca-error "No rewrite overlay found at point")))

;;;###autoload
(defun eca-rewrite-retry ()
  "Retry rewrite overlay at point."
  (interactive)
  (eca-assert-session-running (eca-session))
  (if-let ((ov (eca-rewrite--overlay-at-point)))
    (eca-rewrite--retry ov (eca-session))
    (eca-error "No rewrite overlay found at point")))

;;;###autoload
(defun eca-rewrite-diff ()
  "Apply diff between original text and rewrite overlay at point."
  (interactive)
  (if-let ((ov (eca-rewrite--overlay-at-point)))
    (eca-rewrite--diff ov)
    (eca-error "No rewrite overlay found at point")))

;;;###autoload
(defun eca-rewrite-merge ()
  "Merge between original text and rewrite overlay OV."
  (interactive)
  (if-let ((ov (eca-rewrite--overlay-at-point)))
    (eca-rewrite--merge ov)
    (eca-error "No rewrite overlay found at point")))

(provide 'eca-rewrite)
;;; eca-rewrite.el ends here
