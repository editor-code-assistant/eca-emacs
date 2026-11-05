;;; eca-chat-restore-test.el --- Tests for reopening chats on restart -*- lexical-binding: t; -*-
;;; Commentary:
;; Tests for `eca-restart' reopening the chats that were open before
;; it, see `eca-chat-restore-on-restart' (editor-code-assistant/eca#670).
;;; Code:

(require 'buttercup)
(require 'eca)
(require 'eca-chat)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defvar eca-chat-restore-test--buffers nil
  "Chat buffers created by the current spec, killed after it.")

(defun eca-chat-restore-test--make-chat (session id &rest props)
  "Create a chat buffer with ID and register it in SESSION.
PROPS can hold :welcome for a new chat still showing the welcome
message, :closed for a chat of a stopped session and :unregistered to
leave it out of SESSION."
  (let ((buffer (generate-new-buffer
                 (format " *eca-chat-restore-test:%s*" id))))
    (push buffer eca-chat-restore-test--buffers)
    (with-current-buffer buffer
      (setq major-mode 'eca-chat-mode)
      (setq-local eca-chat--id id)
      (setq-local eca-chat--closed (plist-get props :closed))
      (setq-local eca-chat--welcome-shown (plist-get props :welcome)))
    (unless (plist-get props :unregistered)
      (setf (eca--session-chats session)
            (eca-assoc (eca--session-chats session) id buffer)))
    buffer))

(defun eca-chat-restore-test--kill-buffers ()
  "Kill the chat buffers created by the current spec."
  (dolist (buffer eca-chat-restore-test--buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (setq eca-chat-restore-test--buffers nil))

(defun eca-chat-restore-test--tab-ids (session)
  "Return the chat ids of SESSION's tabs, oldest first."
  (reverse (mapcar #'car (eca--session-chats session))))

(defun eca-chat-restore-test--opened-ids ()
  "Return the chat ids of the chat/open requests sent so far."
  (mapcar (lambda (args)
            (plist-get (plist-get (cdr args) :params) :chatId))
          (spy-calls-all-args 'eca-api-request-async)))

(defun eca-chat-restore-test--request (n)
  "Return the keyword arguments of the Nth request sent."
  (cdr (spy-calls-args-for 'eca-api-request-async n)))

(defun eca-chat-restore-test--answer (session n &optional response)
  "Answer the Nth chat/open request sent for SESSION with RESPONSE.
RESPONSE defaults to a found chat titled after its id.  A found chat
gets a buffer first, as the server's chat/opened makes one."
  (let* ((request (eca-chat-restore-test--request n))
         (chat-id (plist-get (plist-get request :params) :chatId))
         (response (or response
                       (list :found t :chatId chat-id
                             :title (concat "Title " chat-id)))))
    (when (eca-chat--open-response-found-p response)
      (eca-chat-restore-test--make-chat session chat-id))
    (funcall (plist-get request :success-callback) response)))

(defun eca-chat-restore-test--models-synced (session)
  "Report to SESSION that the server synced its models."
  (eca--handle-progress session (list :taskId "models"
                                      :type "finish"
                                      :title "Syncing models")))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(describe "eca-chat-restorable-chats"
  (let (session)
    (before-each
      (setq session (make-eca--session)))

    (after-each
      (eca-chat-restore-test--kill-buffers))

    (it "returns the chats the server knows in tab order and the active one"
      (let ((eca-chat-restore-on-restart 'all))
        (eca-chat-restore-test--make-chat session "A")
        (eca-chat-restore-test--make-chat session "new" :welcome t)
        (setf (eca--session-last-chat-buffer session)
              (eca-chat-restore-test--make-chat session "B"))
        (eca-chat-restore-test--make-chat session "C")
        (eca-chat-restore-test--make-chat session "old" :closed t)
        (expect (eca-chat-restorable-chats session)
                :to-equal '(:ids ("A" "B" "C") :active "B"))))

    (it "takes the current chat buffer as the active one"
      (let ((eca-chat-restore-on-restart 'all))
        (setf (eca--session-last-chat-buffer session)
              (eca-chat-restore-test--make-chat session "A"))
        (with-current-buffer (eca-chat-restore-test--make-chat session "B")
          (expect (eca-chat-restorable-chats session)
                  :to-equal '(:ids ("A" "B") :active "B")))))

    (it "has no active chat when the active one is a new chat"
      (let ((eca-chat-restore-on-restart 'all))
        (eca-chat-restore-test--make-chat session "A")
        (setf (eca--session-last-chat-buffer session)
              (eca-chat-restore-test--make-chat session "new" :welcome t))
        (expect (eca-chat-restorable-chats session)
                :to-equal '(:ids ("A") :active nil))))

    (it "only returns the active chat when set to `active'"
      (let ((eca-chat-restore-on-restart 'active))
        (eca-chat-restore-test--make-chat session "A")
        (setf (eca--session-last-chat-buffer session)
              (eca-chat-restore-test--make-chat session "B"))
        (expect (eca-chat-restorable-chats session)
                :to-equal '(:ids ("B") :active "B"))))

    (it "returns nothing when set to `active' and the active chat is new"
      (let ((eca-chat-restore-on-restart 'active))
        (eca-chat-restore-test--make-chat session "A")
        (setf (eca--session-last-chat-buffer session)
              (eca-chat-restore-test--make-chat session "new" :welcome t))
        (expect (eca-chat-restorable-chats session) :to-be nil)))

    (it "returns nothing when disabled"
      (let ((eca-chat-restore-on-restart nil))
        (setf (eca--session-last-chat-buffer session)
              (eca-chat-restore-test--make-chat session "A"))
        (expect (eca-chat-restorable-chats session) :to-be nil)))

    (it "keeps the chats a restore in progress did not reopen yet"
      (let ((eca-chat-restore-on-restart 'all))
        (eca-chat-restore-test--make-chat session "A")
        (setf (eca--session-last-chat-buffer session)
              (eca-chat-restore-test--make-chat session "new" :welcome t))
        (setf (eca--session-chat-restore session)
              (list :queue '("C") :opening "B" :active "B"))
        (expect (eca-chat-restorable-chats session)
                :to-equal '(:ids ("A" "B" "C") :active "B"))))))

(describe "eca-chat-restore-chats"
  (let (session new-chat timers)
    (before-each
      (setq session (make-eca--session)
            timers nil)
      ;; The new chat the session opens on start.
      (setq new-chat (eca-chat-restore-test--make-chat session "new"
                                                       :welcome t))
      (setf (eca--session-last-chat-buffer session) new-chat)
      (spy-on 'eca-api-request-async)
      (spy-on 'eca-info)
      (spy-on 'eca-warn)
      (spy-on 'eca-chat--refresh-load-older-control)
      (spy-on 'eca-chat--protect-non-prompt)
      (spy-on 'run-with-timer :and-call-fake
              (lambda (secs _repeat function &rest args)
                (push (list secs function args) timers)
                (timer-create))))

    (after-each
      (eca-chat--restore-cancel session)
      (eca-chat-restore-test--kill-buffers))

    (it "waits for the models before reopening chats"
      (let ((eca-chat-history-page-size 20))
        (eca-chat-restore-chats session '(:ids ("A") :active "A"))
        (expect 'eca-api-request-async :not :to-have-been-called)
        (eca-chat-restore-test--models-synced session)
        (let ((request (eca-chat-restore-test--request 0)))
          (expect (plist-get request :method) :to-equal "chat/open")
          (expect (plist-get request :params)
                  :to-equal '(:chatId "A" :limit 20)))))

    (it "reopens the active chat first, then the others one at a time"
      (eca-chat-restore-chats session '(:ids ("A" "B" "C") :active "B"))
      (eca-chat-restore-test--models-synced session)
      (expect (eca-chat-restore-test--opened-ids) :to-equal '("B"))
      (eca-chat-restore-test--answer session 0)
      (expect (eca-chat-restore-test--opened-ids) :to-equal '("B" "A"))
      (eca-chat-restore-test--answer session 1)
      (expect (eca-chat-restore-test--opened-ids) :to-equal '("B" "A" "C"))
      (eca-chat-restore-test--answer session 2)
      (expect (eca--session-chat-restore session) :to-be nil)
      (expect (eca-chat-restore-test--tab-ids session)
              :to-equal '("A" "B" "C"))
      (expect 'eca-info :to-have-been-called-with "Reopened %d chat%s" 3 "s"))

    (it "shows the reopened active chat in place of the unused new chat"
      (save-window-excursion
        (set-window-buffer (selected-window) new-chat)
        (eca-chat-restore-chats session '(:ids ("A" "B") :active "B"))
        (eca-chat-restore-test--models-synced session)
        (eca-chat-restore-test--answer
         session 0
         '(:found t :chatId "B" :title "Bravo"
           :selection (:model "provider/model-b" :agent "plan" :trust t)))
        (let ((b (eca-get (eca--session-chats session) "B")))
          (expect (window-buffer (selected-window)) :to-be b)
          (expect (eca--session-last-chat-buffer session) :to-be b)
          (expect (buffer-live-p new-chat) :to-be nil)
          (expect (eca-chat-restore-test--tab-ids session) :to-equal '("B"))
          (expect (buffer-local-value 'eca-chat--title b) :to-equal "Bravo")
          (expect (buffer-local-value 'eca-chat--selected-model b)
                  :to-equal "provider/model-b")
          (expect (buffer-local-value 'eca-chat--selected-agent b)
                  :to-equal "plan")
          (expect (buffer-local-value 'eca-chat--selected-trust b)
                  :to-be-truthy))))

    (it "keeps the new chat when a prompt was sent in it meanwhile"
      (with-current-buffer new-chat
        (setq-local eca-chat--welcome-shown nil))
      (save-window-excursion
        (set-window-buffer (selected-window) new-chat)
        (eca-chat-restore-chats session '(:ids ("A") :active "A"))
        (eca-chat-restore-test--models-synced session)
        (eca-chat-restore-test--answer session 0)
        (expect (window-buffer (selected-window)) :to-be new-chat)
        (expect (eca--session-last-chat-buffer session) :to-be new-chat)
        (expect (eca-chat-restore-test--tab-ids session)
                :to-equal '("A" "new"))))

    (it "keeps the new chat when a prompt is being typed in it"
      (spy-on 'eca-chat--prompt-content :and-return-value "draft")
      (eca-chat-restore-chats session '(:ids ("A") :active "A"))
      (eca-chat-restore-test--models-synced session)
      (eca-chat-restore-test--answer session 0)
      (expect (buffer-live-p new-chat) :to-be-truthy)
      (expect (eca--session-last-chat-buffer session) :to-be new-chat))

    (it "keeps the new chat when it was the active chat"
      (eca-chat-restore-chats session '(:ids ("A") :active nil))
      (eca-chat-restore-test--models-synced session)
      (eca-chat-restore-test--answer session 0)
      (expect (buffer-live-p new-chat) :to-be-truthy)
      (expect (eca--session-last-chat-buffer session) :to-be new-chat)
      (expect (eca-chat-restore-test--tab-ids session)
              :to-equal '("A" "new")))

    (it "skips the chats the server can't open, showing another one"
      (eca-chat-restore-chats session '(:ids ("A" "B" "C") :active "A"))
      (eca-chat-restore-test--models-synced session)
      (eca-chat-restore-test--answer session 0 '(:found nil))
      (funcall (plist-get (eca-chat-restore-test--request 1) :error-callback)
               '(:code -32603 :message "boom"))
      (eca-chat-restore-test--answer session 2)
      (expect (eca-chat-restore-test--opened-ids) :to-equal '("A" "B" "C"))
      (expect (eca-chat-restore-test--tab-ids session) :to-equal '("C"))
      (expect (eca--session-last-chat-buffer session)
              :to-be (eca-get (eca--session-chats session) "C"))
      (expect (buffer-live-p new-chat) :to-be nil)
      (expect 'eca-info :to-have-been-called-with
              "Reopened %d of the %d chats open before the restart" 1 3))

    (it "warns when no chat could be reopened"
      (eca-chat-restore-chats session '(:ids ("A") :active "A"))
      (eca-chat-restore-test--models-synced session)
      (eca-chat-restore-test--answer session 0 '(:found nil))
      (expect (buffer-live-p new-chat) :to-be-truthy)
      (expect 'eca-warn :to-have-been-called-with
              "Could not reopen the chats open before the restart"))

    (it "skips the chats opened meanwhile, e.g. resumed by hand"
      (eca-chat-restore-chats session '(:ids ("A" "B") :active "A"))
      (eca-chat-restore-test--make-chat session "B")
      (eca-chat-restore-test--models-synced session)
      (eca-chat-restore-test--answer session 0)
      (expect (eca-chat-restore-test--opened-ids) :to-equal '("A"))
      (expect (eca--session-chat-restore session) :to-be nil)
      (expect (eca-chat-restore-test--tab-ids session) :to-equal '("A" "B"))
      (expect 'eca-info :to-have-been-called-with "Reopened %d chat%s" 2 "s"))

    (it "replaces the closed buffer a reopened chat left behind"
      (save-window-excursion
        (let ((closed (eca-chat-restore-test--make-chat
                       session "A" :closed t :unregistered t)))
          (set-window-buffer (selected-window) closed)
          (eca-chat-restore-chats session '(:ids ("A") :active "A"))
          (eca-chat-restore-test--models-synced session)
          (eca-chat-restore-test--answer session 0)
          (expect (buffer-live-p closed) :to-be nil)
          (expect (window-buffer (selected-window))
                  :to-be (eca-get (eca--session-chats session) "A")))))

    (it "reopens the chats anyway when the models never sync"
      (eca-chat-restore-chats session '(:ids ("A") :active "A"))
      (expect 'eca-api-request-async :not :to-have-been-called)
      (pcase-let ((`(,secs ,function ,args) (car timers)))
        (expect secs :to-equal eca-chat--restore-models-timeout)
        (apply function args))
      (expect (eca-chat-restore-test--opened-ids) :to-equal '("A"))
      (eca-chat-restore-test--models-synced session)
      (expect (eca-chat-restore-test--opened-ids) :to-equal '("A")))

    (it "stops when the session is deleted"
      (let ((eca--sessions '()))
        (setf (eca--session-id session) 1)
        (setq eca--sessions (eca-assoc eca--sessions 1 session))
        (eca-chat-restore-chats session '(:ids ("A") :active "A"))
        (spy-on 'cancel-timer)
        (eca-delete-session session)
        (expect 'cancel-timer :to-have-been-called)
        (expect (eca--session-chat-restore session) :to-be nil)
        (eca-chat-restore-test--models-synced session)
        (expect 'eca-api-request-async :not :to-have-been-called)))

    (it "does nothing without chats to reopen"
      (eca-chat-restore-chats session nil)
      (eca-chat-restore-test--models-synced session)
      (expect (eca--session-chat-restore session) :to-be nil)
      (expect 'eca-api-request-async :not :to-have-been-called)
      (expect 'eca-info :not :to-have-been-called))))

(describe "eca-restart"
  (before-each
    (spy-on 'eca-api-notify)
    (spy-on 'eca-info))

  (it "restarts the session on its folders, then reopens its chats"
    (let* ((eca--sessions '())
           (root (expand-file-name "/tmp/eca-restore-test/root"))
           (added (expand-file-name "/tmp/eca-restore-test/added"))
           (session (eca-create-session (list root)))
           (chats '(:ids ("A") :active "A"))
           new-session on-ready)
      (eca--session-add-workspace-folder session added)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-process-running-p :and-return-value t)
      (spy-on 'eca-chat-restorable-chats :and-return-value chats)
      (spy-on 'eca-stop-session)
      (spy-on 'eca-chat-restore-chats)
      (spy-on 'eca-start-session :and-call-fake
              (lambda (started ready)
                (setq new-session started
                      on-ready ready)))
      (eca-restart)
      (expect 'eca-stop-session :to-have-been-called-with session)
      ;; The folders the session started with key its chats storage.
      (expect (eca--session-workspace-folders new-session)
              :to-equal (list root))
      (funcall on-ready new-session)
      (expect (eca--session-workspace-folders new-session)
              :to-equal (list root added))
      (expect 'eca-chat-restore-chats
              :to-have-been-called-with new-session chats)))

  (it "just starts eca when no session is running"
    (spy-on 'eca-session :and-return-value nil)
    (spy-on 'eca)
    (spy-on 'eca-stop-session)
    (eca-restart)
    (expect 'eca :to-have-been-called)
    (expect 'eca-stop-session :not :to-have-been-called)))

(provide 'eca-chat-restore-test)
;;; eca-chat-restore-test.el ends here
