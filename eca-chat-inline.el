;;; eca-chat-inline.el --- ECA inline chat prompts -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Ask ECA questions from any buffer and stream the answer into an
;;  overlay at point, similar to gptel-inline.
;;
;;  `eca-chat-inline-prompt' is the entry point.  It sends prompts via
;;  the server's `chat/inlinePrompt' method with a client-minted chat
;;  id.  The first time it is used in a buffer it asks which chat to
;;  attach to: picking an existing chat forks its history server-side
;;  into the inline chat (keeping the original clean), while picking
;;  "New chat" starts fresh.  The chosen chat becomes sticky for the
;;  buffer, so later invocations reuse it directly.  The answer
;;  streams into an overlay anchored at the region or current line;
;;  the backing chat is a regular ECA chat that can be opened at any
;;  time (`m o' on the overlay), which is also where tool call
;;  questions are answered.
;;
;;; Code:

(require 'compat)
(require 'dash)
(require 'markdown-mode)
(require 'pulse)

(require 'eca-util)
(require 'eca-api)
(require 'eca-chat)

;; Variables

(defcustom eca-chat-inline-max-lines 20
  "Max answer lines the inline overlay viewport shows at once.
Longer answers are scrollable (\\`n' / \\`p' on the overlay, or
\\`C-M-v' / \\`C-M-S-v' from anywhere in the buffer); by default
the viewport follows the tail while streaming and jumps to the
head when finished.  nil means no limit."
  :type '(choice integer (const nil))
  :group 'eca)

(defcustom eca-chat-inline-wrap-column nil
  "Column to hard-wrap the inline answer at.
When nil, wrap at the width of the window showing the source
buffer, falling back to 80 when it is not displayed."
  :type '(choice integer (const nil))
  :group 'eca)

(defcustom eca-chat-inline-dwim-contexts t
  "Whether to attach DWIM contexts to inline prompts.
When non-nil the active region (as a file lines-range) or the
current file is sent as context with each inline prompt."
  :type 'boolean
  :group 'eca)

(defcustom eca-chat-inline-model nil
  "Model for inline chats, or nil to let the server decide.
When nil the server falls back to its `chatInline.model' config,
then the forked chat's model, then the default chat model."
  :type '(choice string (const nil))
  :group 'eca)

(defcustom eca-chat-inline-agent nil
  "Agent for inline prompts, or nil to let the server decide.
When nil the server falls back to its `chatInline.agent' config,
then the default chat agent."
  :type '(choice string (const nil))
  :group 'eca)

(defcustom eca-chat-inline-variant nil
  "Model variant for inline chats, or nil to let the server decide.
When nil the server falls back to its `chatInline.variant'
config, then the forked chat's variant."
  :type '(choice string (const nil))
  :group 'eca)

(defface eca-chat-inline-anchor-face
  '((((class color) (min-colors 88) (background dark))
     :background "#041117" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "light goldenrod yellow" :extend t)
    (t :inherit secondary-selection))
  "Face to highlight the anchor text of an inline prompt."
  :group 'eca)

(defface eca-chat-inline-answer-face
  '((((class color) (min-colors 88) (background dark))
     :background "#0a1520" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "cornsilk" :extend t)
    (t :inherit secondary-selection))
  "Face used as background for the streamed inline answer."
  :group 'eca)

(defface eca-chat-inline-in-progress-face
  '((t (:inherit shadow)))
  "Face for the inline header while a prompt is in flight."
  :group 'eca)

(defface eca-chat-inline-ready-face
  '((((background dark))  (:foreground "turquoise" :bold t))
    (((background light)) (:foreground "dark cyan" :bold t)))
  "Face for the inline header when the answer is ready."
  :group 'eca)

(defface eca-chat-inline-key-face
  '((t (:inherit help-key-binding)))
  "Face for key hints in the inline header."
  :group 'eca)

;; Internal

(defvar eca-chat-inline--chat-id->overlay '()
  "Alist of chat-id to its live inline overlay.")

(defvar eca-chat-inline--overlays-hidden nil
  "Whether all inline overlays are currently hidden.
Toggled by `eca-chat-inline-toggle-overlays'.  Hidden overlays
keep streaming in the background and are shown back on toggle or
on the next inline prompt.")

(defvar eca-chat-inline--prompt-history '()
  "Minibuffer history for inline prompts.")

(defvar-local eca-chat-inline--chat-id nil
  "Chat id sticky-associated with this source buffer.")

(defvar-keymap eca-chat-inline-actions-map
  :doc "Keymap active on the inline overlay anchor text.
Plain letters, like the `eca-rewrite' overlay: they shadow
self-insertion only while point is on the anchor; dismiss with
\\`q' to get normal typing back."
  "r" #'eca-chat-inline-reply
  "q" #'eca-chat-inline-dismiss
  "s" #'eca-chat-inline-stop
  "a" #'eca-chat-inline-approve-tool-call
  "d" #'eca-chat-inline-reject-tool-call
  "m" #'eca-chat-inline-menu
  "n" #'eca-chat-inline-scroll-up
  "p" #'eca-chat-inline-scroll-down)

(defvar-keymap eca-chat-inline-viewport-map
  :doc "Keymap active buffer-wide while an inline overlay is shown."
  "C-M-v" #'eca-chat-inline-scroll-up
  "C-M-S-v" #'eca-chat-inline-scroll-down)

(define-minor-mode eca-chat-inline--viewport-mode
  "Buffer-wide keys while an ECA inline overlay is displayed.
Internal; enabled and disabled by the inline overlay lifecycle.
The scroll keys fall back to their regular bindings when there is
nothing to scroll."
  :keymap eca-chat-inline-viewport-map
  :lighter nil)

(defun eca-chat-inline--session ()
  "Return the running session for the current buffer or error."
  (let ((session (eca-session)))
    (eca-assert-session-running session)
    session))

(defun eca-chat-inline--overlay-at-point ()
  "Return the inline overlay at point, if any."
  (--first (eq (overlay-get it 'category) 'eca-chat-inline)
           (overlays-in (line-beginning-position) (1+ (point)))))

(defun eca-chat-inline--buffer-overlay ()
  "Return the inline overlay at point or anywhere in the buffer."
  (or (eca-chat-inline--overlay-at-point)
      (--first (eq (overlay-get it 'category) 'eca-chat-inline)
               (overlays-in (point-min) (point-max)))))

(defun eca-chat-inline--live-overlay (chat-id)
  "Return the live overlay registered for CHAT-ID, or nil.
Lazily cleans up registry entries whose overlay was deleted."
  (when-let* ((ov (eca-get eca-chat-inline--chat-id->overlay chat-id)))
    (if (overlay-buffer ov)
        ov
      (eca-chat-inline--delete-overlay ov)
      nil)))

(defun eca-chat-inline--chat-buffer (session chat-id)
  "Return the live chat buffer for CHAT-ID in SESSION, or nil."
  (when chat-id
    (when-let* ((buf (eca-chat--get-chat-buffer session chat-id)))
      (when (and (buffer-live-p buf)
                 (not (buffer-local-value 'eca-chat--closed buf)))
        buf))))

(defun eca-chat-inline--select-chat (session)
  "Ask which chat of SESSION to attach the inline prompt to.
Returns a chat buffer, or the symbol `new' for a fresh chat.
When the session has no chats yet, returns `new' directly."
  (let* ((buf-by-label (make-hash-table :test 'equal))
         (labels
          (-keep (lambda (buffer)
                   (when (and (buffer-live-p buffer)
                              (not (buffer-local-value 'eca-chat--closed
                                                       buffer)))
                     (with-current-buffer buffer
                       (let* ((label (concat (eca-chat--chat-status-prefix)
                                             (eca-chat-title)))
                              (label (if (gethash label buf-by-label)
                                         (concat label " (" eca-chat--id ")")
                                       label)))
                         (puthash label buffer buf-by-label)
                         label))))
                 (eca-vals (eca--session-chats session)))))
    (if (null labels)
        'new
      (let ((choice (completing-read "Select a chat for this prompt: "
                                     (append labels
                                             (list eca-chat-new-chat-label))
                                     nil t)))
        (or (gethash choice buf-by-label) 'new)))))

(defun eca-chat-inline--read-prompt (target-desc)
  "Read the inline prompt from the minibuffer for TARGET-DESC."
  (let ((prompt (string-trim
                 (read-string (format "Inline prompt (%s): " target-desc)
                              nil 'eca-chat-inline--prompt-history))))
    (when (string-empty-p prompt)
      (user-error "Inline prompt is empty"))
    prompt))

(defun eca-chat-inline--dwim-contexts ()
  "Return raw DWIM contexts for point, or nil when disabled."
  (when eca-chat-inline-dwim-contexts
    (eca-chat--get-contexts-dwim)))

(defun eca-chat-inline--anchor-bounds ()
  "Return full-line (START . END) bounds for the overlay anchor.
Uses the active region when set, otherwise the current line.  The
trailing newline is covered so the anchor keymap stays active at
end of line (keymaps follow the char after point) and a zero-width
anchor (empty line) survives evaporation."
  (let ((bounds
         (if (use-region-p)
             (let* ((rb (region-beginning))
                    (re (region-end))
                    ;; A region ending at bol (whole-lines selection)
                    ;; should not drag the next line into the anchor.
                    (re (if (and (> re rb)
                                 (save-excursion (goto-char re) (bolp)))
                            (1- re)
                          re)))
               (cons (save-excursion (goto-char rb)
                                     (line-beginning-position))
                     (save-excursion (goto-char re)
                                     (line-end-position))))
           (cons (line-beginning-position) (line-end-position)))))
    (if (< (cdr bounds) (point-max))
        (cons (car bounds) (1+ (cdr bounds)))
      bounds)))

;;;; Overlay rendering

(defun eca-chat-inline--setup-overlay (session start end)
  "Create the inline overlay for SESSION between START and END.
START and END are clamped to the buffer limits: they can be stale
when the buffer changed while the prompt minibuffer was open
\(e.g. a tool call editing this file)."
  (let* ((start (max (point-min) (min start (point-max))))
         (end (min (max start end) (point-max)))
         (ov (make-overlay start end nil t)))
    (overlay-put ov 'category 'eca-chat-inline)
    ;; Evaporating a zero-width overlay (empty buffer) would delete
    ;; it right away, so only evaporate real anchors.
    (when (< start end)
      (overlay-put ov 'evaporate t))
    (overlay-put ov 'eca-chat-inline--chat-id nil)
    (overlay-put ov 'eca-chat-inline--session-id (eca--session-id session))
    (overlay-put ov 'eca-chat-inline--source-buffer (current-buffer))
    (overlay-put ov 'eca-chat-inline--text-acc "")
    (overlay-put ov 'eca-chat-inline--status "Sending prompt...")
    (overlay-put ov 'eca-chat-inline--state 'running)
    (overlay-put ov 'eca-chat-inline--pending-tools '())
    (overlay-put ov 'eca-chat-inline--pending-summaries '())
    (overlay-put ov 'eca-chat-inline--temp-buffer
                 (generate-new-buffer " *eca-chat-inline*"))
    (overlay-put ov 'face 'eca-chat-inline-anchor-face)
    ;; Just below eca-rewrite's 2000 so a rewrite over the same text
    ;; (which replaces its display) deterministically wins.
    (overlay-put ov 'priority 1999)
    (overlay-put ov 'keymap eca-chat-inline-actions-map)
    (overlay-put ov 'help-echo "ECA inline prompt")
    (add-hook 'kill-buffer-hook
              #'eca-chat-inline--on-source-buffer-killed nil t)
    (eca-chat-inline--viewport-mode 1)
    (eca-chat-inline--refresh ov)
    ov))

(defun eca-chat-inline--on-source-buffer-killed ()
  "Clean up the inline overlays of a source buffer being killed.
Releases their temp buffers and routing entries, which outlive
the overlays otherwise."
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (eq (overlay-get ov 'category) 'eca-chat-inline)
        (eca-chat-inline--delete-overlay ov)))))

(defun eca-chat-inline--bind-overlay (ov chat-id)
  "Bind overlay OV to CHAT-ID, registering routing and stickiness.
Replaces any previous overlay bound to the same chat."
  (when-let* ((old (eca-chat-inline--live-overlay chat-id)))
    (unless (eq old ov)
      (eca-chat-inline--delete-overlay old)))
  (overlay-put ov 'eca-chat-inline--chat-id chat-id)
  (setq eca-chat-inline--chat-id->overlay
        (eca-assoc eca-chat-inline--chat-id->overlay chat-id ov))
  (when-let* ((src (overlay-get ov 'eca-chat-inline--source-buffer))
              ((buffer-live-p src)))
    (with-current-buffer src
      (setq-local eca-chat-inline--chat-id chat-id))))

(defun eca-chat-inline--delete-overlay (ov)
  "Delete inline overlay OV together with its routing entry."
  (when-let* ((chat-id (overlay-get ov 'eca-chat-inline--chat-id)))
    (setq eca-chat-inline--chat-id->overlay
          (eca-dissoc eca-chat-inline--chat-id->overlay chat-id)))
  (when-let* ((temp (overlay-get ov 'eca-chat-inline--temp-buffer))
              ((buffer-live-p temp)))
    (kill-buffer temp))
  (let ((buf (overlay-buffer ov)))
    (delete-overlay ov)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (unless (eca-chat-inline--buffer-overlay)
          (eca-chat-inline--viewport-mode -1))))))

(defun eca-chat-inline--key-hint (key label)
  "Return a propertized KEY + LABEL hint string."
  (concat (propertize key 'face 'eca-chat-inline-key-face) " " label))

(defun eca-chat-inline--header (ov)
  "Return the header line string for OV."
  (let* ((state (overlay-get ov 'eca-chat-inline--state))
         (status (overlay-get ov 'eca-chat-inline--status))
         (pending (overlay-get ov 'eca-chat-inline--pending-tools))
         (label-face (if (eq state 'finished)
                         'eca-chat-inline-ready-face
                       'eca-chat-inline-in-progress-face))
         (hints (cond
                 (pending '(("a" . "approve")
                            ("d" . "reject")
                            ("m" . "more")))
                 ((eq state 'finished) '(("r" . "reply")
                                         ("q" . "dismiss")
                                         ("m" . "more")))
                 (t '(("s" . "stop")
                      ("q" . "dismiss")
                      ("m" . "more"))))))
    (concat
     (propertize "ECA: " 'face label-face)
     (propertize (or status "") 'face (if pending 'warning label-face))
     "  "
     (mapconcat (lambda (hint)
                  (eca-chat-inline--key-hint (car hint) (cdr hint)))
                hints
                "  "))))

(defun eca-chat-inline--remap-invisible (text)
  "Return TEXT with `markdown-markup' invisibility made portable.
`markdown-mode' marks markup with the `invisible' value
`markdown-markup', which only hides in buffers whose invisibility
spec lists it.  Rewrite it to t (hides anywhere) when
`eca-chat-hide-markdown-markup' is non-nil, else drop it so the
markup stays visible everywhere."
  (let ((pos 0)
        (len (length text)))
    (while (< pos len)
      (let ((next (or (next-single-property-change pos 'invisible text)
                      len)))
        (when (eq (get-text-property pos 'invisible text) 'markdown-markup)
          (if eca-chat-hide-markdown-markup
              (put-text-property pos next 'invisible t text)
            (remove-text-properties pos next '(invisible nil) text)))
        (setq pos next)))
    text))

(defun eca-chat-inline--wrap-width (ov)
  "Return the column to wrap OV's answer at."
  (or eca-chat-inline-wrap-column
      (let ((win (get-buffer-window (overlay-buffer ov))))
        (max 20 (- (if win (window-body-width win) 80) 2)))))

(defun eca-chat-inline--wrap-lines (width)
  "Hard-wrap lines longer than WIDTH columns in the current buffer.
Lines are only split, never joined, so code blocks keep their
structure.  Splits happen at the last whitespace before WIDTH or
mid-token when there is none.  Invisible chars (hidden markup)
count as zero width."
  (goto-char (point-min))
  (let ((col 0)
        (break-pos nil))
    (while (not (eobp))
      (cond
       ((eq (char-after) ?\n)
        (forward-char 1)
        (setq col 0 break-pos nil))
       ((invisible-p (point))
        (forward-char 1))
       (t
        (when (memq (char-after) '(?\s ?\t))
          (setq break-pos (point)))
        (setq col (1+ col))
        (forward-char 1)
        (when (and (>= col width)
                   (not (eobp))
                   (not (eq (char-after) ?\n)))
          (if break-pos
              (progn
                (goto-char break-pos)
                (delete-char 1)
                (insert "\n"))
            (insert "\n"))
          (setq col 0 break-pos nil)))))))

(defun eca-chat-inline--fontified-body (ov)
  "Return the markdown-fontified answer body for OV, or nil.
Rendered like the chat buffer: fenced code blocks are natively
highlighted and, per `eca-chat-hide-markdown-markup', the
markdown markup is hidden.  Long lines are hard-wrapped to
`eca-chat-inline-wrap-column' since overlay strings cannot be
hscrolled."
  (let ((acc (overlay-get ov 'eca-chat-inline--text-acc))
        (temp (overlay-get ov 'eca-chat-inline--temp-buffer)))
    (when (and (buffer-live-p temp)
               (not (string-empty-p (string-trim acc))))
      (with-current-buffer temp
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          (erase-buffer)
          (insert (string-trim acc))
          (unless (derived-mode-p 'gfm-mode)
            (delay-mode-hooks (gfm-mode)))
          ;; Both are consulted at fontification time.
          (setq-local markdown-fontify-code-blocks-natively t)
          (setq-local markdown-hide-markup
                      (and eca-chat-hide-markdown-markup t))
          (font-lock-ensure)
          ;; Normalize the spec so `invisible-p' during wrapping
          ;; sees exactly what the overlay will hide.
          (setq buffer-invisibility-spec
                (when eca-chat-hide-markdown-markup '(markdown-markup)))
          (eca-chat-inline--wrap-lines (eca-chat-inline--wrap-width ov)))
        (eca-chat-inline--remap-invisible (buffer-string))))))

(defun eca-chat-inline--scroll-step ()
  "Return the line step used by the viewport scroll commands."
  (max 1 (/ (or eca-chat-inline-max-lines 10) 2)))

(defun eca-chat-inline--viewport (ov body finished)
  "Return the visible window of BODY for OV.
Shows at most `eca-chat-inline-max-lines' lines: the tail while
streaming, the head when FINISHED, or the manual offset stored in
the `eca-chat-inline--scroll' overlay property.  Hidden parts are
announced by indicator lines carrying the scroll keys."
  (let* ((lines (split-string body "\n"))
         (total (length lines))
         (max-lines eca-chat-inline-max-lines))
    (if (or (not (integerp max-lines)) (<= total max-lines))
        body
      (let* ((max-offset (- total max-lines))
             (offset (or (overlay-get ov 'eca-chat-inline--scroll)
                         (if finished 0 max-offset)))
             (offset (min (max offset 0) max-offset)))
        (concat
         (when (> offset 0)
           (propertize (format "↑ +%d lines (p)\n" offset)
                       'face 'eca-chat-inline-in-progress-face))
         (mapconcat #'identity (-take max-lines (-drop offset lines)) "\n")
         (when (< offset max-offset)
           (propertize (format "\n↓ +%d lines (n)" (- max-offset offset))
                       'face 'eca-chat-inline-in-progress-face)))))))

(defun eca-chat-inline--refresh (ov)
  "Refresh the before-string display of OV.
The header and the answer viewport are rendered above the anchor.
While overlays are hidden, the display, anchor face and keymap
are dropped so the buffer behaves as if the overlay was not
there; the state keeps updating in the background."
  (when-let* ((buf (overlay-buffer ov)))
    (if eca-chat-inline--overlays-hidden
        (progn
          (overlay-put ov 'before-string nil)
          (overlay-put ov 'face nil)
          (overlay-put ov 'keymap nil))
      (overlay-put ov 'face 'eca-chat-inline-anchor-face)
      (overlay-put ov 'keymap eca-chat-inline-actions-map)
      (eca-chat-inline--render ov buf))))

(defun eca-chat-inline--render (ov buf)
  "Render OV's header and answer viewport into its before-string.
BUF is the overlay's buffer."
  (let* ((finished (eq (overlay-get ov 'eca-chat-inline--state) 'finished))
         (full-body (eca-chat-inline--fontified-body ov))
         (body (and full-body
                    (eca-chat-inline--viewport ov full-body finished)))
         (body (when body
                 (let ((b (copy-sequence body)))
                   (add-face-text-property
                    0 (length b) 'eca-chat-inline-answer-face 'append b)
                   b))))
    (overlay-put ov 'eca-chat-inline--body-lines
                 (if full-body
                     (length (split-string full-body "\n"))
                   0))
    (overlay-put ov 'before-string
                 (concat
                  (with-current-buffer buf
                    (unless (or (= (overlay-start ov) (point-min))
                                (eq (char-before (overlay-start ov)) ?\n))
                      "\n"))
                  (eca-chat-inline--header ov)
                  (when body (concat "\n" body))
                  "\n"))))

(defun eca-chat-inline--set-status (ov status &optional state)
  "Set STATUS text (and optionally STATE) on OV and refresh it."
  (overlay-put ov 'eca-chat-inline--status status)
  (when state
    (overlay-put ov 'eca-chat-inline--state state))
  (eca-chat-inline--refresh ov))

;;;; Content routing

(defun eca-chat-inline--finalize (ov)
  "Mark OV as finished and show the final actions."
  (overlay-put ov 'eca-chat-inline--pending-tools '())
  (overlay-put ov 'eca-chat-inline--pending-summaries '())
  (eca-chat-inline--set-status ov "Done" 'finished)
  (when-let* ((buf (overlay-buffer ov)))
    (with-current-buffer buf
      (pulse-momentary-highlight-region (overlay-start ov)
                                        (overlay-end ov)))))

(defun eca-chat-inline--start-turn (ov)
  "Reset OV to mirror a fresh turn."
  (overlay-put ov 'eca-chat-inline--text-acc "")
  (overlay-put ov 'eca-chat-inline--scroll nil)
  (eca-chat-inline--set-status ov "Waiting model..." 'running))

(defun eca-chat-inline--track-pending-tool (ov id summary)
  "Track tool call ID with SUMMARY as pending approval in OV.
Return the status text for all pending tools."
  (overlay-put ov 'eca-chat-inline--pending-tools
               (cons id (overlay-get ov 'eca-chat-inline--pending-tools)))
  (overlay-put ov 'eca-chat-inline--pending-summaries
               (eca-assoc (overlay-get ov 'eca-chat-inline--pending-summaries)
                          id summary))
  ;; Newest are prepended, so reverse to show them in arrival order.
  (mapconcat #'cdr
             (reverse (overlay-get ov 'eca-chat-inline--pending-summaries))
             ", "))

(defun eca-chat-inline--drop-pending-tool (ov id)
  "Drop tool call ID from OV's pending approval tracking.
Return the status text for the remaining pending tools, or nil
when none is left."
  (overlay-put ov 'eca-chat-inline--pending-tools
               (delete id (overlay-get ov 'eca-chat-inline--pending-tools)))
  (overlay-put ov 'eca-chat-inline--pending-summaries
               (eca-dissoc (overlay-get ov 'eca-chat-inline--pending-summaries)
                           id))
  (when-let* ((summaries (overlay-get ov 'eca-chat-inline--pending-summaries)))
    (mapconcat #'cdr (reverse summaries) ", ")))

(defun eca-chat-inline--handle-content (ov role content)
  "Update OV given chat CONTENT owned by ROLE."
  (let ((type (plist-get content :type)))
    (pcase type
      ("text"
       (pcase role
         ;; A user message starts a turn: reset the answer area so the
         ;; overlay always shows the answer to the last question.
         ("user" (eca-chat-inline--start-turn ov))
         ("assistant"
          (overlay-put ov 'eca-chat-inline--text-acc
                       (concat (overlay-get ov 'eca-chat-inline--text-acc)
                               (or (plist-get content :text) "")))
          (eca-chat-inline--set-status ov "Streaming..." 'running))
         ;; System text carries errors and notices (e.g. provider
         ;; failures after the prompt started); surface it in the
         ;; answer area so failed turns don't render as a clean Done.
         ("system"
          (overlay-put ov 'eca-chat-inline--text-acc
                       (concat (overlay-get ov 'eca-chat-inline--text-acc)
                               (or (plist-get content :text) "")))
          (eca-chat-inline--refresh ov))
         (_ nil)))
      ("progress"
       (pcase (plist-get content :state)
         ("running"
          ;; While tool calls wait for approval, keep their summaries
          ;; as the status instead of the generic server progress text
          ;; ("Waiting for tool call approval").
          (unless (overlay-get ov 'eca-chat-inline--pending-tools)
            (eca-chat-inline--set-status
             ov (or (plist-get content :text) "Running...") 'running)))
         ("finished" (eca-chat-inline--finalize ov))))
      ("reasonStarted"
       (eca-chat-inline--set-status ov "Thinking..." 'running))
      ("reasonFinished"
       (eca-chat-inline--set-status ov "Waiting model..." 'running))
      ("toolCallPrepare"
       (eca-chat-inline--set-status
        ov
        (or (plist-get content :summary)
            (format "Preparing tool %s..." (plist-get content :name)))
        'running))
      ("toolCallRun"
       (if (plist-get content :manualApproval)
           (eca-chat-inline--set-status
            ov (eca-chat-inline--track-pending-tool
                ov (plist-get content :id)
                (or (plist-get content :summary)
                    (format "Tool %s needs approval"
                            (plist-get content :name)))))
         (eca-chat-inline--set-status
          ov
          (or (plist-get content :summary)
              (format "Running tool %s..." (plist-get content :name)))
          'running)))
      ("toolCallRunning"
       ;; Approved (possibly from another client): no longer pending.
       (eca-chat-inline--drop-pending-tool ov (plist-get content :id))
       (eca-chat-inline--set-status
        ov
        (or (plist-get content :summary)
            (format "Running tool %s..." (plist-get content :name)))
        'running))
      ("toolCalled"
       (if-let* ((remaining (eca-chat-inline--drop-pending-tool
                             ov (plist-get content :id))))
           (eca-chat-inline--set-status ov remaining)
         (eca-chat-inline--set-status ov "Waiting model..." 'running)))
      ("toolCallRejected"
       (if-let* ((remaining (eca-chat-inline--drop-pending-tool
                             ov (plist-get content :id))))
           (eca-chat-inline--set-status ov remaining)
         (eca-chat-inline--set-status ov "Tool call rejected")))
      (_ nil))))

(defun eca-chat-inline--content-received (session params)
  "Mirror chat PARAMS from SESSION into inline overlays.
Subscribed to `eca-chat-content-received-functions'."
  (let ((chat-id (plist-get params :chatId)))
    (unless (plist-get params :parentChatId)
      (when-let* ((ov (eca-chat-inline--live-overlay chat-id))
                  ((equal (overlay-get ov 'eca-chat-inline--session-id)
                          (eca--session-id session))))
        (eca-chat-inline--handle-content ov
                                         (plist-get params :role)
                                         (plist-get params :content))))))

(defun eca-chat-inline--chat-deleted (_session chat-id)
  "Drop overlay and stickiness tied to a deleted CHAT-ID.
Stickiness is cleared in every buffer associated to CHAT-ID, even
when its overlay was already dismissed, so the next prompt asks
for a chat again instead of reviving the dead id.
Subscribed to `eca-chat-deleted-functions'."
  (when-let* ((ov (eca-get eca-chat-inline--chat-id->overlay chat-id)))
    (eca-chat-inline--delete-overlay ov))
  (dolist (buf (buffer-list))
    (when (equal (buffer-local-value 'eca-chat-inline--chat-id buf) chat-id)
      (with-current-buffer buf
        (setq-local eca-chat-inline--chat-id nil)))))

(defun eca-chat-inline--session-status-changed (session)
  "Show pending questions of SESSION chats in their inline overlays.
Also sweeps registry entries whose overlay died without cleanup,
e.g. an evaporated anchor, releasing their temp buffers.
Subscribed to `eca-chat-session-status-changed-functions'."
  (dolist (entry (copy-sequence eca-chat-inline--chat-id->overlay))
    (-let* (((chat-id . ov) entry))
      (cond
       ((not (overlay-buffer ov))
        (eca-chat-inline--delete-overlay ov))
       ((equal (overlay-get ov 'eca-chat-inline--session-id)
               (eca--session-id session))
        (when-let* ((buf (eca-chat-inline--chat-buffer session chat-id)))
          (when (buffer-local-value 'eca-chat--pending-question buf)
            (eca-chat-inline--set-status
             ov "Question pending, open the chat to answer"))))))))

;;;; Sending

(defun eca-chat-inline--discard-failed-chat (session ov created-buffer)
  "Roll back state pre-created for OV's chat by a failed first prompt.
No-op unless CREATED-BUFFER is non-nil (the mirror buffer created
by the failed request): deregisters it from SESSION and kills it,
and clears the source buffer stickiness so the next prompt picks
\(and re-forks) a chat again, since the server never created this
one."
  (when created-buffer
    (let ((chat-id (overlay-get ov 'eca-chat-inline--chat-id)))
      (when (and (buffer-live-p created-buffer)
                 (eq created-buffer (eca-chat--get-chat-buffer session chat-id)))
        (setf (eca--session-chats session)
              (eca-dissoc (eca--session-chats session) chat-id))
        (with-current-buffer created-buffer
          (setq-local eca-chat--closed t))
        (kill-buffer created-buffer)
        (eca-chat--notify-status-changed session))
      (when-let* ((src (overlay-get ov 'eca-chat-inline--source-buffer))
                  ((buffer-live-p src)))
        (with-current-buffer src
          (when (equal eca-chat-inline--chat-id chat-id)
            (setq-local eca-chat-inline--chat-id nil)))))))

(defun eca-chat-inline--send (session ov text contexts
                                      &optional source-chat-id created-chat-buffer)
  "Send TEXT with raw CONTEXTS for OV's chat via SESSION.
Uses the `chat/inlinePrompt' method with the overlay's chat-id.
SOURCE-CHAT-ID asks the server to fork that chat's history into
the inline chat when it is being created.  Request errors are
surfaced on the overlay; when CREATED-CHAT-BUFFER is non-nil (the
mirror pre-created by this request), a failure also rolls it back
so no ghost chat lingers in the session."
  (let ((chat-id (overlay-get ov 'eca-chat-inline--chat-id))
        (refined (->> contexts
                      (-map #'eca-chat--refine-context)
                      (-keep #'eca-chat--materialize-context))))
    (eca-api-request-async
     session
     :method "chat/inlinePrompt"
     :params (append (list :chatId chat-id
                           :message text
                           :contexts (vconcat refined))
                     (when source-chat-id
                       (list :sourceChatId source-chat-id))
                     (when eca-chat-inline-model
                       (list :model eca-chat-inline-model))
                     (when eca-chat-inline-agent
                       (list :agent eca-chat-inline-agent))
                     (when eca-chat-inline-variant
                       (list :variant eca-chat-inline-variant)))
     :success-callback
     (lambda (res)
       ;; Param validation failures and internal server errors come
       ;; back as a normal response with an error status instead of a
       ;; jsonrpc error.
       (when (equal "error" (plist-get res :status))
         (eca-chat-inline--discard-failed-chat session ov created-chat-buffer)
         (when (overlay-buffer ov)
           (eca-chat-inline--set-status
            ov "Prompt failed, open the chat for details" 'finished))))
     :error-callback
     (lambda (err)
       (eca-chat-inline--discard-failed-chat session ov created-chat-buffer)
       (when (overlay-buffer ov)
         (eca-chat-inline--set-status
          ov
          (format "Error: %s" (or (plist-get err :message) err))
          'finished))))))

;;;; Prompt flows

(defun eca-chat-inline--chat-desc (chat-buffer)
  "Return a plain description of CHAT-BUFFER for minibuffer use."
  (with-current-buffer chat-buffer
    (substring-no-properties (eca-chat-title))))

(defun eca-chat-inline--assert-idle (chat-buffer)
  "Error when CHAT-BUFFER still has a prompt in flight."
  (when (buffer-local-value 'eca-chat--chat-loading chat-buffer)
    (user-error "Inline chat is busy; stop it first (s on the overlay)")))

(defun eca-chat-inline--start-prompt (session chat-id &optional source-chat-id desc)
  "Read a prompt and send it to CHAT-ID of SESSION.
SOURCE-CHAT-ID asks the server to fork that chat into a brand-new
inline chat.  DESC labels the minibuffer prompt."
  (let* ((contexts (eca-chat-inline--dwim-contexts))
         (bounds (eca-chat-inline--anchor-bounds))
         (text (eca-chat-inline--read-prompt (or desc "new chat")))
         ;; Ensure the mirror buffer before creating the overlay so an
         ;; error here leaks no unbound overlay; remember when this
         ;; request created it, to roll it back if the request fails.
         (created-chat-buffer
          (unless (eca-chat-inline--chat-buffer session chat-id)
            (eca-chat-ensure-chat-buffer session chat-id)))
         (ov (eca-chat-inline--setup-overlay session
                                             (car bounds) (cdr bounds))))
    (eca-chat-inline--bind-overlay ov chat-id)
    (deactivate-mark)
    (eca-chat-inline--set-status ov "Waiting model...")
    (eca-chat-inline--send session ov text contexts source-chat-id
                           created-chat-buffer)))

(defun eca-chat-inline--ensure-chat-buffer (session chat-id)
  "Return CHAT-ID's live chat buffer in SESSION, recreating it if killed.
The chat lives server-side, so a killed mirror buffer is not a
reason to lose the inline session."
  (or (eca-chat-inline--chat-buffer session chat-id)
      (eca-chat-ensure-chat-buffer session chat-id)))

(defun eca-chat-inline--reply (session ov)
  "Read a reply prompt for OV and send it via SESSION."
  (let* ((chat-id (overlay-get ov 'eca-chat-inline--chat-id))
         (chat-buffer (and chat-id
                           (eca-chat-inline--ensure-chat-buffer session
                                                                chat-id))))
    (unless chat-buffer
      (user-error "The inline chat no longer exists"))
    (eca-chat-inline--assert-idle chat-buffer)
    (let* ((contexts (eca-chat-inline--dwim-contexts))
           (text (eca-chat-inline--read-prompt
                  (eca-chat-inline--chat-desc chat-buffer))))
      (overlay-put ov 'eca-chat-inline--text-acc "")
      (overlay-put ov 'eca-chat-inline--scroll nil)
      (eca-chat-inline--set-status ov "Waiting model..." 'running)
      (eca-chat-inline--send session ov text contexts))))

(defun eca-chat-inline--ov-or-error ()
  "Return the inline overlay at point or signal a user error."
  (or (eca-chat-inline--overlay-at-point)
      (user-error "No ECA inline overlay at point")))

;; Public

(defun eca-chat-inline--prompt-selecting (session)
  "Ask which chat to use and start an inline prompt via SESSION."
  (let* ((target (eca-chat-inline--select-chat session))
         (source-buffer (unless (eq target 'new) target))
         (source-chat-id
          (when source-buffer
            (buffer-local-value 'eca-chat--id source-buffer)))
         (desc (if source-buffer
                   (format "fork of %s"
                           (eca-chat-inline--chat-desc source-buffer))
                 "new chat")))
    (eca-chat-inline--start-prompt session (eca-uuid)
                                   source-chat-id desc)))

;;;###autoload
(defun eca-chat-inline-prompt (&optional force-select)
  "Ask ECA a question, streaming the answer into an overlay at point.
When the buffer has no inline chat yet, asks which chat to use:
an existing chat has its history forked server-side into the
inline chat (keeping the original clean) while `New chat' starts
fresh.  The chat becomes sticky for the buffer so later
invocations reuse it directly; with a prefix argument
FORCE-SELECT the selection is always asked again.  With an active
region, it is sent as context and used as the overlay anchor.  On
an existing overlay this sends a reply to the same chat."
  (interactive "P")
  (let ((session (eca-chat-inline--session)))
    (when eca-chat-inline--overlays-hidden
      (eca-chat-inline--set-overlays-hidden nil))
    (cond
     (force-select
      (when-let* ((existing-ov (eca-chat-inline--overlay-at-point)))
        (eca-chat-inline--delete-overlay existing-ov))
      (setq-local eca-chat-inline--chat-id nil)
      (eca-chat-inline--prompt-selecting session))
     ((eca-chat-inline--overlay-at-point)
      (eca-chat-inline--reply session (eca-chat-inline--overlay-at-point)))
     (t
      (let ((sticky-buffer (and eca-chat-inline--chat-id
                                (eca-chat-inline--ensure-chat-buffer
                                 session eca-chat-inline--chat-id))))
        (if sticky-buffer
            (progn
              (eca-chat-inline--assert-idle sticky-buffer)
              (eca-chat-inline--start-prompt
               session
               (buffer-local-value 'eca-chat--id sticky-buffer)
               nil
               (eca-chat-inline--chat-desc sticky-buffer)))
          (setq-local eca-chat-inline--chat-id nil)
          (eca-chat-inline--prompt-selecting session)))))))

;;;###autoload
(defun eca-chat-inline-reply ()
  "Send a reply inline prompt for the overlay at point."
  (interactive)
  (when eca-chat-inline--overlays-hidden
    (eca-chat-inline--set-overlays-hidden nil))
  (eca-chat-inline--reply (eca-chat-inline--session)
                          (eca-chat-inline--ov-or-error)))

(defun eca-chat-inline--set-overlays-hidden (hidden)
  "Set all inline overlays visibility to the opposite of HIDDEN.
Dead overlays found on the way are cleaned up."
  (setq eca-chat-inline--overlays-hidden hidden)
  (dolist (entry (copy-sequence eca-chat-inline--chat-id->overlay))
    (-let* (((_chat-id . ov) entry))
      (if (overlay-buffer ov)
          (eca-chat-inline--refresh ov)
        (eca-chat-inline--delete-overlay ov)))))

;;;###autoload
(defun eca-chat-inline-toggle-overlays ()
  "Toggle visibility of all inline overlays.
Hidden overlays keep streaming in the background and their
anchors get their keys back; they are shown again on the next
toggle or inline prompt."
  (interactive)
  (eca-chat-inline--set-overlays-hidden
   (not eca-chat-inline--overlays-hidden))
  (eca-info "Inline overlays %s"
            (if eca-chat-inline--overlays-hidden "hidden" "shown")))

;;;###autoload
(defun eca-chat-inline-open-chat ()
  "Open the chat buffer behind the inline overlay at point."
  (interactive)
  (let* ((session (eca-chat-inline--session))
         (ov (eca-chat-inline--ov-or-error))
         (chat-id (overlay-get ov 'eca-chat-inline--chat-id))
         (chat-buffer (and chat-id
                           (eca-chat-inline--ensure-chat-buffer session
                                                                chat-id))))
    (unless chat-buffer
      (user-error "The inline chat no longer exists"))
    (eca-chat--switch-to-buffer chat-buffer session)))

;;;###autoload
(defun eca-chat-inline-dismiss ()
  "Dismiss the inline overlay at point, keeping its chat."
  (interactive)
  (eca-chat-inline--delete-overlay (eca-chat-inline--ov-or-error)))

;;;###autoload
(defun eca-chat-inline-stop ()
  "Stop the prompt running behind the inline overlay at point."
  (interactive)
  (let* ((session (eca-chat-inline--session))
         (ov (eca-chat-inline--ov-or-error))
         (chat-id (overlay-get ov 'eca-chat-inline--chat-id))
         (chat-buffer (and chat-id
                           (eca-chat-inline--chat-buffer session chat-id))))
    (if (and chat-buffer
             (or (buffer-local-value 'eca-chat--chat-loading chat-buffer)
                 (buffer-local-value 'eca-chat--pending-question chat-buffer)))
        ;; The buffer path also cancels a pending question client-side.
        (eca-chat--with-current-buffer chat-buffer
          (eca-chat--stop-prompt session))
      ;; No mirror buffer, or it does not know the prompt started yet
      ;; (statusChanged still in flight): stop straight on the server,
      ;; which ignores chats that are not running.
      (eca-api-notify session
                      :method "chat/promptStop"
                      :params (list :chatId chat-id)))
    (eca-chat-inline--set-status ov "Stopping...")))

(defun eca-chat-inline--scroll-viewport (ov delta)
  "Scroll OV's answer viewport by DELTA lines, clamped."
  (let ((total (or (overlay-get ov 'eca-chat-inline--body-lines) 0))
        (max-lines eca-chat-inline-max-lines))
    (unless (and (integerp max-lines) (> total max-lines))
      (user-error "Nothing to scroll"))
    (let* ((finished (eq (overlay-get ov 'eca-chat-inline--state) 'finished))
           (max-offset (- total max-lines))
           (current (or (overlay-get ov 'eca-chat-inline--scroll)
                        (if finished 0 max-offset)))
           (offset (min (max (+ current delta) 0) max-offset)))
      (overlay-put ov 'eca-chat-inline--scroll offset)
      (eca-chat-inline--refresh ov))))

(defun eca-chat-inline--fallback-command ()
  "Return the pressed key's binding outside the viewport mode."
  (let ((keys (this-command-keys-vector)))
    (when (> (length keys) 0)
      (let ((eca-chat-inline--viewport-mode nil))
        (key-binding keys t)))))

(defun eca-chat-inline--scroll-or-fallback (delta)
  "Scroll the buffer's inline viewport by DELTA lines.
When there is nothing to scroll, run whatever the pressed key is
bound to outside `eca-chat-inline--viewport-mode' (e.g. the usual
`scroll-other-window'), so the keys stay useful."
  (let ((ov (eca-chat-inline--buffer-overlay)))
    (unless ov
      ;; Stale mode left by an overlay that evaporated: self-heal.
      (eca-chat-inline--viewport-mode -1))
    (if (and ov
             (not eca-chat-inline--overlays-hidden)
             (integerp eca-chat-inline-max-lines)
             (> (or (overlay-get ov 'eca-chat-inline--body-lines) 0)
                eca-chat-inline-max-lines))
        (eca-chat-inline--scroll-viewport ov delta)
      (let ((fallback (eca-chat-inline--fallback-command)))
        (if (and fallback (not (eq fallback this-command)))
            (call-interactively fallback)
          (user-error "Nothing to scroll"))))))

;;;###autoload
(defun eca-chat-inline-scroll-up ()
  "Scroll the inline overlay viewport forward.
Works from anywhere in the buffer showing the overlay; falls back
to the key's regular binding when there is nothing to scroll."
  (interactive)
  (eca-chat-inline--scroll-or-fallback (eca-chat-inline--scroll-step)))

;;;###autoload
(defun eca-chat-inline-scroll-down ()
  "Scroll the inline overlay viewport backward.
Works from anywhere in the buffer showing the overlay; falls back
to the key's regular binding when there is nothing to scroll."
  (interactive)
  (eca-chat-inline--scroll-or-fallback (- (eca-chat-inline--scroll-step))))

(defun eca-chat-inline--answer-tool-calls (method status)
  "Send METHOD for each pending tool call at point, showing STATUS."
  (let* ((session (eca-chat-inline--session))
         (ov (eca-chat-inline--ov-or-error))
         (chat-id (overlay-get ov 'eca-chat-inline--chat-id))
         (pending (overlay-get ov 'eca-chat-inline--pending-tools)))
    (unless pending
      (user-error "No tool call awaiting approval here"))
    (dolist (tool-id pending)
      (eca-api-notify session
                      :method method
                      :params (list :chatId chat-id :toolCallId tool-id)))
    (overlay-put ov 'eca-chat-inline--pending-tools '())
    (overlay-put ov 'eca-chat-inline--pending-summaries '())
    (eca-chat-inline--set-status ov status)))

;;;###autoload
(defun eca-chat-inline-approve-tool-call ()
  "Approve every pending tool call of the inline overlay at point."
  (interactive)
  (eca-chat-inline--answer-tool-calls "chat/toolCallApprove"
                                      "Approved, waiting..."))

;;;###autoload
(defun eca-chat-inline-reject-tool-call ()
  "Reject every pending tool call of the inline overlay at point."
  (interactive)
  (eca-chat-inline--answer-tool-calls "chat/toolCallReject"
                                      "Rejected, waiting..."))

;;;###autoload
(defun eca-chat-inline-detach ()
  "Forget the inline chat association of the current buffer.
Also dismisses its overlay when present.  The chat itself is kept
and can still be reached via `eca-chat-select'."
  (interactive)
  (when-let* ((chat-id eca-chat-inline--chat-id)
              (ov (eca-chat-inline--live-overlay chat-id)))
    (eca-chat-inline--delete-overlay ov))
  (setq-local eca-chat-inline--chat-id nil)
  (eca-info "Inline chat association removed"))

(defun eca-chat-inline--select-setting (prompt current options)
  "Read via completion one of OPTIONS for the setting named PROMPT.
CURRENT is shown in the prompt; empty input returns nil, meaning
the server decides."
  (let ((choice (string-trim
                 (completing-read
                  (format "%s (current: %s, empty for server default): "
                          prompt (or current "server default"))
                  (append options nil)))))
    (unless (string-empty-p choice) choice)))

;;;###autoload
(defun eca-chat-inline-select-model ()
  "Select the model to use for inline prompting.
Empty input resets to the server default."
  (interactive)
  (let ((session (eca-chat-inline--session)))
    (setq eca-chat-inline-model
          (eca-chat-inline--select-setting "Inline model"
                                           eca-chat-inline-model
                                           (eca--session-models session)))
    (eca-info "Inline model: %s" (or eca-chat-inline-model "server default"))))

;;;###autoload
(defun eca-chat-inline-select-agent ()
  "Select the agent to use for inline prompting.
Empty input resets to the server default."
  (interactive)
  (let ((session (eca-chat-inline--session)))
    (setq eca-chat-inline-agent
          (eca-chat-inline--select-setting "Inline agent"
                                           eca-chat-inline-agent
                                           (eca--session-chat-agents session)))
    (eca-info "Inline agent: %s" (or eca-chat-inline-agent "server default"))))

;;;###autoload
(defun eca-chat-inline-select-variant ()
  "Select the model variant to use for inline prompting.
Empty input resets to the server default."
  (interactive)
  (let ((session (eca-chat-inline--session)))
    (setq eca-chat-inline-variant
          (eca-chat-inline--select-setting "Inline variant"
                                           eca-chat-inline-variant
                                           (eca--session-chat-variants session)))
    (eca-info "Inline variant: %s"
              (or eca-chat-inline-variant "server default"))))

(with-eval-after-load 'transient
  (transient-define-prefix eca-chat-inline--menu-prefix ()
    "ECA inline prompt menu."
    [["Settings (next prompts)"
      ("m" "Select model" eca-chat-inline-select-model)
      ("v" "Select variant" eca-chat-inline-select-variant)
      ("a" "Select agent" eca-chat-inline-select-agent)]
     ["Actions"
      ("o" "Open backing chat" eca-chat-inline-open-chat)
      ("t" "Toggle overlays visibility" eca-chat-inline-toggle-overlays)
      ("d" "Detach buffer chat" eca-chat-inline-detach)]]))

;;;###autoload
(defun eca-chat-inline-menu ()
  "Open the inline prompt menu with settings and extra actions.
Requires the `transient' package."
  (interactive)
  (unless (require 'transient nil t)
    (user-error "Install the `transient' package to use the ECA inline menu"))
  (transient-setup 'eca-chat-inline--menu-prefix))

(add-hook 'eca-chat-content-received-functions
          #'eca-chat-inline--content-received)
(add-hook 'eca-chat-deleted-functions
          #'eca-chat-inline--chat-deleted)
(add-hook 'eca-chat-session-status-changed-functions
          #'eca-chat-inline--session-status-changed)

(provide 'eca-chat-inline)
;;; eca-chat-inline.el ends here
