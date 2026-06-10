;;; eca-doom-test.el --- Tests for eca-doom -*- lexical-binding: t; -*-
;;; Commentary:
;; Verify `eca-chat-session-status' and the Doom workspaces tabline
;; decoration based on the ECA session status of each workspace.
;;; Code:
(require 'buttercup)
(require 'cl-lib)
(require 'eca-doom)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun eca-doom-test--make-chat-buffer (name &optional state session-id)
  "Create a chat-like buffer NAME with STATE caching SESSION-ID.
STATE is one of `approval', `question', `loading' or nil.  The
buffer only mimics `eca-chat-mode' by setting `major-mode' so
`derived-mode-p' matches without running the full mode setup."
  (let ((buf (generate-new-buffer name)))
    (with-current-buffer buf
      (setq-local major-mode 'eca-chat-mode)
      (when session-id
        (setq-local eca--session-id-cache session-id))
      (pcase state
        ('approval (insert (propertize
                            "[accept]"
                            'eca-tool-call-pending-approval-accept t)))
        ('question (setq-local eca-chat--pending-question '(:question "q")))
        ('loading (setq-local eca-chat--chat-loading t))))
    buf))

(defun eca-doom-test--make-session (id buffers &optional folders)
  "Create a session ID owning chat BUFFERS with workspace FOLDERS."
  (make-eca--session
   :id id
   :workspace-folders folders
   :chats (cl-loop for buf in buffers
                   for i from 0
                   collect (cons (format "chat-%s-%s" id i) buf))))

(defun eca-doom-test--tabline (names)
  "Build a Doom-like tabline string for workspace NAMES."
  (mapconcat #'identity
             (cl-loop for name in names
                      for i from 1
                      collect (propertize (format " [%d] %s " i name)
                                          'face '+workspace-tab-face))
             " "))

(defmacro eca-doom-test--with-doom (workspaces &rest body)
  "Run BODY with Doom workspace functions stubbed from WORKSPACES.
WORKSPACES is an alist of (name . buffers) mimicking perspectives."
  (declare (indent 1))
  `(let ((eca-doom-test--workspaces ,workspaces))
     (cl-letf (((symbol-function '+workspace-list-names)
                (lambda () (mapcar #'car eca-doom-test--workspaces)))
               ((symbol-function '+workspace-get)
                (lambda (name &optional _noerror)
                  (assoc name eca-doom-test--workspaces)))
               ((symbol-function 'persp-buffers)
                (lambda (persp) (cdr persp))))
       ,@body)))

(defun eca-doom-test--face-at (string regexp)
  "Return the face property at the first REGEXP match in STRING."
  (get-text-property (string-match regexp string) 'face string))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(describe "eca-chat-session-status"

  (it "is idle for a session without chats"
    (expect (eca-chat-session-status (eca-doom-test--make-session 1 nil))
            :to-be 'idle))

  (it "is idle when all chats are idle"
    (let* ((buf (eca-doom-test--make-chat-buffer "*ds-idle*"))
           (session (eca-doom-test--make-session 1 (list buf))))
      (unwind-protect
          (expect (eca-chat-session-status session) :to-be 'idle)
        (kill-buffer buf))))

  (it "is running when any chat is loading"
    (let* ((b1 (eca-doom-test--make-chat-buffer "*ds-i1*"))
           (b2 (eca-doom-test--make-chat-buffer "*ds-l1*" 'loading))
           (session (eca-doom-test--make-session 1 (list b1 b2))))
      (unwind-protect
          (expect (eca-chat-session-status session) :to-be 'running)
        (mapc #'kill-buffer (list b1 b2)))))

  (it "is waiting-approval when a chat has a pending approval"
    (let* ((buf (eca-doom-test--make-chat-buffer "*ds-a1*" 'approval))
           (session (eca-doom-test--make-session 1 (list buf))))
      (unwind-protect
          (expect (eca-chat-session-status session) :to-be 'waiting-approval)
        (kill-buffer buf))))

  (it "is waiting-approval when a chat has a pending question"
    (let* ((buf (eca-doom-test--make-chat-buffer "*ds-q1*" 'question))
           (session (eca-doom-test--make-session 1 (list buf))))
      (unwind-protect
          (expect (eca-chat-session-status session) :to-be 'waiting-approval)
        (kill-buffer buf))))

  (it "prefers waiting-approval over running"
    (let* ((b1 (eca-doom-test--make-chat-buffer "*ds-l2*" 'loading))
           (b2 (eca-doom-test--make-chat-buffer "*ds-a2*" 'approval))
           (session (eca-doom-test--make-session 1 (list b1 b2))))
      (unwind-protect
          (expect (eca-chat-session-status session) :to-be 'waiting-approval)
        (mapc #'kill-buffer (list b1 b2)))))

  (it "ignores killed chat buffers"
    (let* ((buf (eca-doom-test--make-chat-buffer "*ds-dead*" 'approval))
           (session (eca-doom-test--make-session 1 (list buf))))
      (kill-buffer buf)
      (expect (eca-chat-session-status session) :to-be 'idle))))

(describe "eca-doom--session-for-workspace"

  (it "resolves through a buffer cached session id"
    (let* ((buf (eca-doom-test--make-chat-buffer "*dw-cache*" nil 1))
           (session (eca-doom-test--make-session 1 (list buf)))
           (eca--sessions (list (cons 1 session))))
      (unwind-protect
          (eca-doom-test--with-doom (list (cons "proj" (list buf)))
            (expect (eca-doom--session-for-workspace "proj") :to-be session))
        (kill-buffer buf))))

  (it "resolves through a workspace folder match"
    (let* ((root (make-temp-file "eca-doom-test" t))
           (session (eca-doom-test--make-session 1 nil (list root)))
           (buf (generate-new-buffer "*dw-file*"))
           (eca--sessions (list (cons 1 session))))
      (with-current-buffer buf
        (setq default-directory (file-name-as-directory root)))
      (unwind-protect
          (eca-doom-test--with-doom (list (cons "proj" (list buf)))
            (expect (eca-doom--session-for-workspace "proj") :to-be session))
        (kill-buffer buf)
        (delete-directory root t))))

  (it "is nil when no buffer relates to a session"
    (let* ((buf (generate-new-buffer "*dw-none*"))
           (session (eca-doom-test--make-session 1 nil '("/eca/nowhere")))
           (eca--sessions (list (cons 1 session))))
      (unwind-protect
          (eca-doom-test--with-doom (list (cons "proj" (list buf)))
            (expect (eca-doom--session-for-workspace "proj") :to-be nil))
        (kill-buffer buf)))))

(describe "eca-doom--tabline-decorate"

  (it "colors a running workspace and leaves idle ones untouched"
    (let* ((idle-chat (eca-doom-test--make-chat-buffer "*dt-i*" nil 1))
           (run-chat (eca-doom-test--make-chat-buffer "*dt-r*" 'loading 2))
           (s1 (eca-doom-test--make-session 1 (list idle-chat)))
           (s2 (eca-doom-test--make-session 2 (list run-chat)))
           (eca--sessions (list (cons 1 s1) (cons 2 s2))))
      (unwind-protect
          (eca-doom-test--with-doom (list (cons "main" (list idle-chat))
                                          (cons "code" (list run-chat)))
            (let* ((tabline (eca-doom-test--tabline '("main" "code")))
                   (result (eca-doom--tabline-decorate tabline
                                                       '("main" "code"))))
              ;; Same text, only faces change.
              (expect result :to-equal tabline)
              (expect (memq 'eca-doom-workspace-tab-running-face
                            (ensure-list (eca-doom-test--face-at result "code")))
                      :to-be-truthy)
              (expect (eca-doom-test--face-at result "main")
                      :to-equal '+workspace-tab-face)))
        (mapc #'kill-buffer (list idle-chat run-chat)))))

  (it "colors a workspace waiting for approval"
    (let* ((chat (eca-doom-test--make-chat-buffer "*dt-a*" 'approval 1))
           (session (eca-doom-test--make-session 1 (list chat)))
           (eca--sessions (list (cons 1 session))))
      (unwind-protect
          (eca-doom-test--with-doom (list (cons "code" (list chat)))
            (let* ((tabline (eca-doom-test--tabline '("code")))
                   (result (eca-doom--tabline-decorate tabline '("code"))))
              (expect result :to-equal tabline)
              (expect (memq 'eca-doom-workspace-tab-attention-face
                            (ensure-list (eca-doom-test--face-at result "code")))
                      :to-be-truthy)))
        (kill-buffer chat))))

  (it "returns nil when disabled so callers keep the original"
    (let ((eca-doom-workspace-tabs nil))
      (expect (eca-doom--tabline-decorate "tabline" '("main")) :to-be nil)))

  (it "leaves the tabline unchanged when the format is unexpected"
    (let* ((chat (eca-doom-test--make-chat-buffer "*dt-w*" 'loading 1))
           (session (eca-doom-test--make-session 1 (list chat)))
           (eca--sessions (list (cons 1 session))))
      (unwind-protect
          (eca-doom-test--with-doom (list (cons "code" (list chat)))
            (expect (eca-doom--tabline-decorate "weird" '("code"))
                    :to-equal "weird"))
        (kill-buffer chat)))))

(describe "eca-doom--tabline-advice"

  (it "decorates the original tabline"
    (let* ((chat (eca-doom-test--make-chat-buffer "*da-r*" 'loading 1))
           (session (eca-doom-test--make-session 1 (list chat)))
           (eca--sessions (list (cons 1 session)))
           (tabline (eca-doom-test--tabline '("code"))))
      (unwind-protect
          (eca-doom-test--with-doom (list (cons "code" (list chat)))
            (expect (memq 'eca-doom-workspace-tab-running-face
                          (ensure-list
                           (eca-doom-test--face-at
                            (eca-doom--tabline-advice
                             (lambda (&optional _) tabline))
                            "code")))
                    :to-be-truthy))
        (kill-buffer chat))))

  (it "returns the original tabline when disabled"
    (let ((tabline (eca-doom-test--tabline '("main")))
          (eca-doom-workspace-tabs nil))
      (eca-doom-test--with-doom (list (cons "main" nil))
        (expect (eca-doom--tabline-advice (lambda (&optional _) tabline))
                :to-be tabline))))

  (it "returns the original tabline on unexpected errors"
    (let ((tabline (eca-doom-test--tabline '("main"))))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () (error "Boom"))))
        (expect (eca-doom--tabline-advice (lambda (&optional _) tabline))
                :to-be tabline)))))

(provide 'eca-doom-test)
;;; eca-doom-test.el ends here
