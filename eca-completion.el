;;; eca-completion.el --- ECA (Editor Code Assistant) completion -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) completion.
;;
;;; Code:

(require 'cl-lib)
(require 'color)
(require 'map)

(require 'eca-util)
(require 'eca-api)

;; Variables

(defcustom eca-completion-idle-delay 0.2
  "Time in seconds to wait before starting completion.

Complete immediately if set to 0.
Disable idle completion if set to nil."
  :type '(choice
          (number :tag "Seconds of delay")
          (const :tag "Idle completion disabled" nil))
  :group 'eca)

(defcustom eca-completion-syntax-highlight t
  "Whether to syntax-highlight the inline completion overlay.
When non-nil, the suggestion is fontified using the buffer's
`major-mode' before being displayed as ghost text, and
`eca-completion-overlay-face' is composed on top of those
font-lock faces (rather than overriding them) so syntax colors
remain visible."
  :type 'boolean
  :group 'eca)

(defcustom eca-completion-overlay-dim-ratio 0.5
  "How far to blend each fontified span toward the default background.
A float in [0.0, 1.0]: 1.0 keeps the original syntax color, 0.0
collapses it to the background, and intermediate values produce
a dimmed \"ghost\" look similar to Cursor or Copilot.  Set to nil
to disable dimming and show fully saturated syntax colors.
Effective only when `eca-completion-syntax-highlight' is non-nil."
  :type '(choice
          (float :tag "Blend ratio (0.0..1.0)")
          (const :tag "Disable dimming" nil))
  :group 'eca)

(defface eca-completion-overlay-face
  '((t :inherit shadow))
  "Face for the ECA completion inline overlay.
Composed with APPEND priority on top of any font-lock faces
already applied to the suggestion text, so syntax-highlighted
foregrounds win over this face's foreground.  The face's other
attributes (e.g. italic from `shadow') still apply across the
whole suggestion, including spans that have no font-lock face."
  :group 'eca)

;; Internal

(defvar-local eca-completion--real-posn nil
  "Posn information without overlay.
To work around posn problems with after-string property.")

(defvar-local eca-completion--overlay nil
  "Overlay for ECA completion.")
(defvar-local eca-completion--keymap-overlay nil
  "Overlay used to surround point and make eca-completion-keymap activate.")

(defvar-local eca-completion--doc-version 0
  "The document version of the current buffer.
Incremented after each change.")

(defvar-local eca-completion--last-doc-version 0
  "The document version of the last completion.")

(defvar-local eca-completion--line-bias 1
  "Line bias for completion.")

(defvar eca-completion--post-command-timer nil)

(defvar eca-completion-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'eca-completion-accept)
    (define-key map (kbd "TAB") #'eca-completion-accept)
    map)
  "Keymap for ECA completion overlay.")

(defun eca-completion--string-common-prefix (str1 str2)
  "Find the common prefix of STR1 and STR2 directly."
  (let ((min-len (min (length str1) (length str2)))
        (i 0))
    (while (and (< i min-len)
                (= (aref str1 i) (aref str2 i)))
      (setq i (1+ i)))
    (substring str1 0 i)))

(defun eca-completion--get-or-create-keymap-overlay ()
  "Make or return the local keymap overlay."
  (unless (overlayp eca-completion--keymap-overlay)
    (setq eca-completion--keymap-overlay (make-overlay 1 1 nil nil t))
    (overlay-put eca-completion--keymap-overlay 'keymap eca-completion-map)
    (overlay-put eca-completion--keymap-overlay 'local-map eca-completion-map)
    (overlay-put eca-completion--keymap-overlay 'priority 101))
  eca-completion--keymap-overlay)

(defun eca-completion--get-overlay ()
  "Create or get overlay for ECA completion."
  (unless (overlayp eca-completion--overlay)
    (setq eca-completion--overlay (make-overlay 1 1 nil nil t))
    (overlay-put eca-completion--overlay 'keymap-overlay (eca-completion--get-or-create-keymap-overlay)))
  eca-completion--overlay)

(defun eca-completion--fontify-as-mode (text mode)
  "Return a fresh copy of TEXT fontified in a temp buffer using MODE.
MODE is a major-mode symbol.  Returns a copy of TEXT unmodified
when MODE is nil, unbound, or signals an error during activation
or fontification."
  (if (and mode (fboundp mode))
      (condition-case nil
          (with-temp-buffer
            (let ((inhibit-modification-hooks t))
              (insert text)
              (delay-mode-hooks (funcall mode))
              (font-lock-ensure))
            (buffer-substring (point-min) (point-max)))
        (error (copy-sequence text)))
    (copy-sequence text)))

(defun eca-completion--blend-color (fg bg ratio)
  "Blend foreground color FG toward background color BG by RATIO.
RATIO is a float in [0.0, 1.0]: 1.0 returns FG, 0.0 returns BG.
Returns a hex string like \"#rrggbb\", or nil when either color
cannot be resolved (e.g. on low-color terminals)."
  (let ((fg-rgb (and fg (color-name-to-rgb fg)))
        (bg-rgb (and bg (color-name-to-rgb bg))))
    (when (and fg-rgb bg-rgb)
      (apply #'color-rgb-to-hex
             (append (cl-mapcar (lambda (f b)
                                  (+ (* f ratio) (* b (- 1.0 ratio))))
                                fg-rgb bg-rgb)
                     ;; 2 digits per component for portable #rrggbb output.
                     '(2))))))

(defun eca-completion--resolve-foreground (face)
  "Return the foreground color implied by FACE, or nil.
FACE may be a face name symbol, a list of faces (symbols and/or
anonymous face plists like (:foreground COLOR)), or a single
anonymous face plist.  Anonymous plists are inspected directly
because `face-foreground' rejects them on Emacs 28."
  (cond
   ((null face) nil)
   ((and (listp face) (keywordp (car face)))
    (plist-get face :foreground))
   ((listp face)
    (let (result)
      (while (and face (not result))
        (setq result (eca-completion--resolve-foreground (car face))
              face (cdr face)))
      result))
   ((symbolp face)
    (face-foreground face nil 'default))))

(defun eca-completion--dim-fontified (text bg ratio)
  "Return a copy of TEXT with each face span's foreground dimmed.
TEXT is expected to already carry font-lock face properties.
Each span's resolved foreground (falling back to the `default'
face's foreground) is blended toward BG by RATIO via
`eca-completion--blend-color', and the result is layered on the
span as a higher-priority anonymous face so the original
font-lock face still contributes its non-color attributes.
Spans whose colors cannot be resolved are left untouched."
  (let* ((result (copy-sequence text))
         (len (length result))
         (default-fg (face-foreground 'default))
         (pos 0))
    (while (< pos len)
      (let* ((next (or (next-single-property-change pos 'face result)
                       len))
             (face-prop (get-text-property pos 'face result))
             (fg (or (eca-completion--resolve-foreground face-prop)
                     default-fg))
             (blended (and fg bg
                           (eca-completion--blend-color fg bg ratio))))
        (when blended
          (add-face-text-property pos next
                                  (list :foreground blended)
                                  nil result))
        (setq pos next)))
    result))

(defun eca-completion--prepare-display-text (text)
  "Return TEXT prepared for the inline overlay.
Applies syntax fontification and foreground dimming according to
`eca-completion-syntax-highlight' and
`eca-completion-overlay-dim-ratio'.  The returned string is safe
to mutate and does not yet carry `eca-completion-overlay-face'."
  (let ((mode major-mode)
        (bg (face-background 'default nil 'default)))
    (cond
     ((and eca-completion-syntax-highlight
           (numberp eca-completion-overlay-dim-ratio))
      (eca-completion--dim-fontified
       (eca-completion--fontify-as-mode text mode)
       bg
       eca-completion-overlay-dim-ratio))
     (eca-completion-syntax-highlight
      (eca-completion--fontify-as-mode text mode))
     (t (copy-sequence text)))))

(defun eca-completion--set-overlay-text (ov text)
  "Set overlay OV with TEXT."
  (move-overlay ov (point) (line-end-position))
  (move-overlay (overlay-get ov 'keymap-overlay) (point) (min (point-max) (+ 1 (point))))

  (let* ((display-text (eca-completion--prepare-display-text text))
         (tail (buffer-substring (- (line-end-position) (overlay-get ov 'tail-length)) (line-end-position)))
         (text-p (concat display-text tail)))
    ;; Layer the overlay face with APPEND priority so any font-lock
    ;; foregrounds set during fontification remain visible.  Apply only
    ;; to the suggestion portion; the trailing buffer text keeps its
    ;; own faces untouched.
    (add-face-text-property 0 (length display-text)
                            'eca-completion-overlay-face t text-p)
    (if (eolp)
        (progn
          (overlay-put ov 'after-string "")
          (setq eca-completion--real-posn (cons (point) (posn-at-point)))
          (put-text-property 0 1 'cursor t text-p)
          (overlay-put ov 'display "")
          (overlay-put ov 'after-string text-p))
      (overlay-put ov 'display (substring text-p 0 1))
      (overlay-put ov 'after-string (substring text-p 1)))
    (overlay-put ov 'completion text)
    (overlay-put ov 'start (point))))

(defun eca-completion--display-overlay-completion (text id start end)
  "Show TEXT with ID between START and END."
  (eca-completion--clear-overlay)
  (when (and (not (string-blank-p text))
             (or (<= start (point))))
    (let* ((ov (eca-completion--get-overlay)))
      (overlay-put ov 'tail-length (- (line-end-position) end))
      (eca-completion--set-overlay-text ov text)
      (overlay-put ov 'id id)
      (overlay-put ov 'completion-start start))))

(defun eca-completion--overlay-visible ()
  "Return whether completion overlay is being shown."
  (and (overlayp eca-completion--overlay)
       (overlay-buffer eca-completion--overlay)))

(defun eca-completion--clear-overlay ()
  "Clear ECA completion overlay."
  (interactive)
  (when (eca-completion--overlay-visible)
    (delete-overlay eca-completion--overlay)
    (delete-overlay eca-completion--keymap-overlay)
    (setq eca-completion--real-posn nil)))

(declare-function vterm-delete-region "ext:vterm.el")
(declare-function vterm-insert "ext:vterm.el")

(defun eca-completion-accept ()
  "Accept completion."
  (interactive)
  (when (eca-completion--overlay-visible)
    (let* ((text (overlay-get eca-completion--overlay 'completion))
           (start (overlay-get eca-completion--overlay 'start))
           (end (- (line-end-position) (overlay-get eca-completion--overlay 'tail-length)))
           (completion-start (overlay-get eca-completion--overlay 'completion-start)))
      ;; If there is extra indentation before the point, delete it and shift the completion
      (when (and (< completion-start (point))
                 ;; Region we are about to delete contains only blanks …
                 (string-blank-p (buffer-substring-no-properties completion-start (point)))
                 ;; … *and* everything from BOL to completion-start is blank
                 ;; as well — i.e. we are really inside the leading indentation.
                 (string-blank-p (buffer-substring-no-properties (line-beginning-position) completion-start)))
        (setq start completion-start)
        (setq end (- end (- (point) completion-start)))
        (delete-region completion-start (point)))
      (eca-completion--clear-overlay)
      (if (derived-mode-p 'vterm-mode)
          (progn
            (vterm-delete-region start end)
            (vterm-insert text))
        (delete-region start end)
        (insert text))
      t)))

(cl-defun eca-completion--find-completion (&key on-success on-error)
  "Find completion requesting server calling ON-SUCCESS for the response.
Call ON-ERROR when error."
  (let ((line (+ (1- (line-number-at-pos)) eca-completion--line-bias))
        (character (1+ (- (point) (line-beginning-position)))))
    (when-let ((session (eca-session)))
      (eca-api-request-async session
                             :method "completion/inline"
                             :params (list :doc-text (buffer-substring-no-properties (point-min) (point-max))
                                           :doc-version eca-completion--doc-version
                                           :position (list :line line :character character))
                             :success-callback (-lambda ((&plist :items items :error error))
                                                 (if error
                                                     (funcall on-error error)
                                                   (funcall on-success items)))
                             :error-callback (-lambda (error)
                                               (let ((type (plist-get error :type))
                                                     (msg (plist-get error :message)))
                                                 (pcase type
                                                   ("error" (eca-error msg))
                                                   ("warning" (eca-warn msg))
                                                   ("info" (eca-info msg)))))))))

(defun eca-completion--post-command-debounce (buffer)
  "Complete in BUFFER."
  (when (and (buffer-live-p buffer)
             (equal (current-buffer) buffer)
             (boundp 'eca-completion-mode)
             eca-completion-mode
             (not (derived-mode-p 'eca-chat-mode))
             (eca-session))
    (eca-complete)))

(defun eca-completion--inserted-next-overlay-char-p (command)
  "Handle the case where the char just inserted is the start of the completion.
If so, update the overlays and continue.  COMMAND is the command that triggered
in `post-command-hook'."
  (when (and (symbolp this-command)
             (eq command 'self-insert-command)
             (eca-completion--overlay-visible))
    (let* ((ov eca-completion--overlay)
           (completion (overlay-get ov 'completion)))
      ;; The char just inserted is the next char of completion
      (when (eq last-command-event (elt completion 0))
        (if (= (length completion) 1)
            ;; If there is only one char in the completion, accept it
            (eca-completion-accept)
          (eca-completion--set-overlay-text ov (substring completion 1)))))))

(defun eca-completion--post-command ()
  "Auto complete when idle."
  (unless (eca-completion--inserted-next-overlay-char-p this-command)
    (eca-completion--clear-overlay)
    (when eca-completion--post-command-timer
      (cancel-timer eca-completion--post-command-timer))
    (when (eq 'self-insert-command this-command)
      (when (numberp eca-completion-idle-delay)
        (setq eca-completion--post-command-timer
              (run-with-idle-timer eca-completion-idle-delay
                                   nil
                                   #'eca-completion--post-command-debounce
                                   (current-buffer)))))))

(defun eca-completion--mode-activate ()
  "Initial setup for eca-completion-mode."
  (add-hook 'post-command-hook #'eca-completion--post-command nil 'local))

(defun eca-completion--mode-deactivate ()
  "Teardown for eca-completion-mode."
  (when eca-completion--post-command-timer
    (cancel-timer eca-completion--post-command-timer))
  (remove-hook 'post-command-hook #'eca-completion--post-command 'local))

(defun eca-completion--posn-advice (&rest args)
  "Remap posn if in eca-completion-mode with ARGS."
  (when (derived-mode-p 'eca-completion-mode)
    (let ((pos (or (car-safe args) (point))))
      (when (and eca-completion--real-posn
                 (eq pos (car eca-completion--real-posn)))
        (cdr eca-completion--real-posn)))))

(defun eca-completion--show-completion (completion)
  "Show COMPLETION."
  (-let* (((&plist :text text :id id :range range :docVersion doc-version) completion)
          ((&plist :start start :end end) range)
          ((&plist :line start-line :character start-char) start)
          ((&plist :character end-char) end))
    (when (= doc-version eca-completion--doc-version)
      (save-excursion
        (save-restriction
          (widen)
          (let* ((p (point))
                 (goto-line! (lambda ()
                               (goto-char (point-min))
                               (forward-line (1- (+ (1- start-line) eca-completion--line-bias)))))
                 (start (progn
                          (funcall goto-line!)
                          (forward-char (1- start-char))
                          (let* ((cur-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                                 (common-prefix-len (length (eca-completion--string-common-prefix text cur-line))))
                            (setq text (substring text common-prefix-len))
                            (forward-char common-prefix-len)
                            (point))))
                 (end (progn
                        (funcall goto-line!)
                        (forward-char (1- end-char))
                        (point))))
            (goto-char p)
            (eca-completion--display-overlay-completion text id start end)))))))

;; Public

;;;###autoload
(define-minor-mode eca-completion-mode
  "Minor mode for ECA completions."
  :init-value nil
  :lighter " ECA completion"
  (eca-completion--clear-overlay)
  (advice-add 'posn-at-point :before-until #'eca-completion--posn-advice)
  (if eca-completion-mode
      (eca-completion--mode-activate)
    (eca-completion--mode-deactivate)))

;;;###autoload
(defun eca-complete ()
  "Complete at the current point."
  (interactive)
  (when (derived-mode-p 'eca-chat-mode)
    (user-error "Completion is not available in eca-chat buffers"))
  (eca-assert-session-running (eca-session))
  (setq eca-completion--last-doc-version eca-completion--doc-version)

  ;; TODO add cache
  ;; (setq eca-completion--completion-cache nil)

  (let ((called-interactively (called-interactively-p 'interactive)))
    (eca-completion--find-completion
     :on-success
     (lambda (items)
       (let ((item (if (seq-empty-p items) nil (seq-elt items 0))))
         (if item
             (eca-completion--show-completion item)
           (when called-interactively
             (eca-warn "No completion is available."))))))))

(provide 'eca-completion)
;;; eca-completion.el ends here
