;;; eca-workspaces-test.el --- Tests for eca-workspaces -*- lexical-binding: t; -*-
;;; Commentary:
;; Tests for the `eca-workspaces' tree buffer: folding/unfolding,
;; switching to a chat, workspace navigation and the clickable-label
;; affordance gating.
;;; Code:
(require 'buttercup)
(require 'eca)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defvar eca-workspaces-test--opened nil
  "Names of chats \"opened\" by the fake chat action, most recent first.")

(defun eca-workspaces-test--make-buffer (spec)
  "Build an *eca-workspaces*-like buffer from SPEC.
SPEC is a list of (WORKSPACE CHAT...) entries, all strings.  The
buffer mirrors the real rendering: a root \"ECA\" node, each
workspace as a foldable session label and each chat as an
`eca-buttonize' button whose action pushes its name onto
`eca-workspaces-test--opened'.  Returns the buffer; the caller
must kill it."
  (setq eca-workspaces-test--opened nil)
  (let ((chat->ws (make-hash-table :test 'equal))
        (ws-set '()))
    (dolist (grp spec)
      (push (car grp) ws-set)
      (dolist (c (cdr grp))
        (puthash c (car grp) chat->ws)))
    (let* ((parent-fn (lambda (item)
                        (cond
                         ((gethash item chat->ws) (gethash item chat->ws))
                         ((member item ws-set) 'root)
                         (t nil))))
           (label-fn (lambda (item _)
                       (insert
                        (cond
                         ((member item ws-set)
                          (eca-workspaces--foldable-label
                           (propertize item 'face 'shadow
                                       'eca-workspaces-session t)))
                         ((gethash item chat->ws)
                          (eca-buttonize
                           nil
                           (propertize item 'face 'default)
                           (lambda ()
                             (push item eca-workspaces-test--opened))))
                         (t (propertize "ECA" 'face 'shadow))))))
           (h (hierarchy-new))
           (buf (generate-new-buffer " *test-eca-workspaces*")))
      (dolist (grp spec)
        (hierarchy-add-tree h (car grp) parent-fn)
        (dolist (c (cdr grp))
          (hierarchy-add-tree h c parent-fn)))
      (with-current-buffer buf
        (eca-workspaces-mode)
        (let ((inhibit-read-only t))
          (widget-create (eca--tree-widget-open-all
                          (hierarchy-convert-to-tree-widget h label-fn)))
          (widget-setup))
        (goto-char (point-min)))
      buf)))

(defun eca-workspaces-test--goto (text)
  "Move point to the start of the first occurrence of TEXT."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0)))

(defun eca-workspaces-test--visible-p (text)
  "Return non-nil if TEXT is present in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (and (search-forward text nil t) t)))

(defun eca-workspaces-test--label-at ()
  "Return the workspace label text at point, or nil."
  (when (get-text-property (point) 'eca-workspaces-session)
    (buffer-substring-no-properties
     (point)
     (or (next-single-property-change (point) 'eca-workspaces-session)
         (line-end-position)))))

;; ---------------------------------------------------------------------------
;; eca-workspaces--foldable-label
;; ---------------------------------------------------------------------------

(describe "eca-workspaces--foldable-label"

  (it "does not advertise clicking when eca-buttons-allow-mouse is nil"
    (let* ((eca-buttons-allow-mouse nil)
           (s (eca-workspaces--foldable-label (copy-sequence "WS")))
           (km (get-text-property 0 'keymap s)))
      (expect (get-text-property 0 'pointer s) :to-be nil)
      (expect (get-text-property 0 'help-echo s) :to-be nil)
      (expect (lookup-key km (kbd "<mouse-1>")) :to-be nil)
      ;; Keyboard folding is always available.
      (expect (lookup-key km (kbd "RET")) :to-be 'eca-workspaces-toggle-line)
      (expect (lookup-key km (kbd "TAB")) :to-be 'eca-workspaces-toggle-line)))

  (it "advertises clicking when eca-buttons-allow-mouse is non-nil"
    (let* ((eca-buttons-allow-mouse t)
           (s (eca-workspaces--foldable-label (copy-sequence "WS")))
           (km (get-text-property 0 'keymap s)))
      (expect (get-text-property 0 'pointer s) :to-be 'hand)
      (expect (get-text-property 0 'help-echo s) :to-equal "mouse-1: fold/unfold")
      (expect (lookup-key km (kbd "<mouse-1>")) :to-be 'eca-workspaces-toggle-line))))

;; ---------------------------------------------------------------------------
;; eca-workspaces-mode
;; ---------------------------------------------------------------------------

(describe "eca-workspaces-mode"

  (it "derives from special-mode"
    (with-temp-buffer
      (eca-workspaces-mode)
      (expect (derived-mode-p 'special-mode) :to-be-truthy)))

  (it "inherits q to close the window from special-mode"
    (with-temp-buffer
      (eca-workspaces-mode)
      (expect (key-binding (kbd "q")) :to-be 'quit-window)))

  (it "binds the tree and navigation keys"
    (expect (lookup-key eca-workspaces-mode-map (kbd "RET"))
            :to-be 'eca-workspaces-visit-or-toggle)
    (expect (lookup-key eca-workspaces-mode-map (kbd "TAB"))
            :to-be 'eca-workspaces-toggle-line)
    (expect (lookup-key eca-workspaces-mode-map (kbd "n"))
            :to-be 'eca-workspaces-next-workspace)
    (expect (lookup-key eca-workspaces-mode-map (kbd "p"))
            :to-be 'eca-workspaces-previous-workspace)))

;; ---------------------------------------------------------------------------
;; line predicates
;; ---------------------------------------------------------------------------

(describe "eca-workspaces--session-line-p"

  (it "is true on a workspace line and nil on chat/root lines"
    (let ((buf (eca-workspaces-test--make-buffer '(("WS1" "c1a")))))
      (unwind-protect
          (with-current-buffer buf
            (eca-workspaces-test--goto "WS1")
            (expect (eca-workspaces--session-line-p) :to-be-truthy)
            (eca-workspaces-test--goto "c1a")
            (expect (eca-workspaces--session-line-p) :to-be nil)
            (goto-char (point-min))     ; root "ECA" line
            (expect (eca-workspaces--session-line-p) :to-be nil))
        (kill-buffer buf)))))

(describe "eca-workspaces--chat-action-on-line"

  (it "returns the chat action on a chat line"
    (let ((buf (eca-workspaces-test--make-buffer '(("WS1" "c1a")))))
      (unwind-protect
          (with-current-buffer buf
            (eca-workspaces-test--goto "c1a")
            (expect (functionp (eca-workspaces--chat-action-on-line))
                    :to-be-truthy))
        (kill-buffer buf))))

  (it "returns nil on a workspace line"
    (let ((buf (eca-workspaces-test--make-buffer '(("WS1" "c1a")))))
      (unwind-protect
          (with-current-buffer buf
            (eca-workspaces-test--goto "WS1")
            (expect (eca-workspaces--chat-action-on-line) :to-be nil))
        (kill-buffer buf)))))

;; ---------------------------------------------------------------------------
;; eca-workspaces-toggle-line
;; ---------------------------------------------------------------------------

(describe "eca-workspaces-toggle-line"

  (it "folds and unfolds a workspace"
    (let ((buf (eca-workspaces-test--make-buffer '(("WS1" "c1a" "c1b")))))
      (unwind-protect
          (with-current-buffer buf
            (expect (eca-workspaces-test--visible-p "c1a") :to-be-truthy)
            (eca-workspaces-test--goto "WS1")
            (eca-workspaces-toggle-line)
            (expect (eca-workspaces-test--visible-p "c1a") :to-be nil)
            (eca-workspaces-test--goto "WS1")
            (eca-workspaces-toggle-line)
            (expect (eca-workspaces-test--visible-p "c1a") :to-be-truthy))
        (kill-buffer buf))))

  (it "does nothing on a chat (leaf) line"
    (let ((buf (eca-workspaces-test--make-buffer '(("WS1" "c1a")))))
      (unwind-protect
          (with-current-buffer buf
            (eca-workspaces-test--goto "c1a")
            (let ((before (buffer-string)))
              (expect (eca-workspaces-toggle-line) :to-be nil)
              (expect (buffer-string) :to-equal before)))
        (kill-buffer buf)))))

;; ---------------------------------------------------------------------------
;; eca-workspaces-visit-or-toggle
;; ---------------------------------------------------------------------------

(describe "eca-workspaces-visit-or-toggle"

  (it "invokes the chat action on a chat line"
    (let ((buf (eca-workspaces-test--make-buffer '(("WS1" "c1a")))))
      (unwind-protect
          (with-current-buffer buf
            (eca-workspaces-test--goto "c1a")
            (eca-workspaces-visit-or-toggle)
            (expect eca-workspaces-test--opened :to-equal '("c1a")))
        (kill-buffer buf))))

  (it "folds the workspace (no chat opened) on a workspace line"
    (let ((buf (eca-workspaces-test--make-buffer '(("WS1" "c1a")))))
      (unwind-protect
          (with-current-buffer buf
            (eca-workspaces-test--goto "WS1")
            (eca-workspaces-visit-or-toggle)
            (expect (eca-workspaces-test--visible-p "c1a") :to-be nil)
            (expect eca-workspaces-test--opened :to-be nil))
        (kill-buffer buf)))))

;; ---------------------------------------------------------------------------
;; workspace navigation
;; ---------------------------------------------------------------------------

(describe "eca-workspaces workspace navigation"

  (it "moves across workspaces, skips chats and stops at the edges"
    (let ((buf (eca-workspaces-test--make-buffer
                '(("WS1" "c1a" "c1b") ("WS2" "c2a")))))
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))     ; root "ECA" line
            (eca-workspaces-next-workspace)
            (expect (eca-workspaces-test--label-at) :to-equal "WS1")
            (eca-workspaces-next-workspace)
            (expect (eca-workspaces-test--label-at) :to-equal "WS2")
            ;; No next workspace: stay put.
            (eca-workspaces-next-workspace)
            (expect (eca-workspaces-test--label-at) :to-equal "WS2")
            (eca-workspaces-previous-workspace)
            (expect (eca-workspaces-test--label-at) :to-equal "WS1")
            ;; No previous workspace: stay put.
            (eca-workspaces-previous-workspace)
            (expect (eca-workspaces-test--label-at) :to-equal "WS1"))
        (kill-buffer buf)))))

;;; eca-workspaces-test.el ends here
