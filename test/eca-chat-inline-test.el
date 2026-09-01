;;; eca-chat-inline-test.el --- Tests for eca-chat-inline -*- lexical-binding: t; -*-
;;; Commentary:
;; Tests for the inline prompt flow: chat targeting (sticky, server-side
;; fork, new chat) via chat/inlinePrompt, content routing into the
;; overlay and cleanup.
;;; Code:
(require 'buttercup)
(require 'eca-chat-inline)

(defun eca-chat-inline-test--content (session chat-id role content)
  "Route CONTENT with ROLE for CHAT-ID of SESSION into overlays."
  (eca-chat-inline--content-received
   session (list :chatId chat-id :role role :content content)))

(describe "eca-chat-inline"
  (let (chat-buffer source-buffer session)
    (before-each
      (setq eca-chat-inline--chat-id->overlay '())
      (setq chat-buffer (generate-new-buffer "*eca-inline-test-chat*"))
      (with-current-buffer chat-buffer
        (setq-local eca-chat--id "chat-1"))
      (setq source-buffer (generate-new-buffer "*eca-inline-test-src*"))
      (with-current-buffer source-buffer
        (insert "line one\nline two\n")
        (goto-char (point-min)))
      (setq session (make-eca--session :id "session-1"))
      (setf (eca--session-chats session)
            (eca-assoc '() "chat-1" chat-buffer))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'pulse-momentary-highlight-region))

    (after-each
      (setq eca-chat-inline--chat-id->overlay '())
      (dolist (buf (list chat-buffer source-buffer))
        (when (buffer-live-p buf)
          (kill-buffer buf)))
      (dolist (buf (buffer-list))
        (when (string-prefix-p " *eca-chat-inline*" (buffer-name buf))
          (kill-buffer buf))))

    (describe "eca-chat-inline-prompt"
      (it "signals a user-error when no session is running"
        (spy-on 'eca-session :and-return-value nil)
        (expect (eca-chat-inline-prompt) :to-throw 'user-error))

      (it "sends directly to the sticky chat without asking for a chat"
        (spy-on 'eca-chat-inline--select-chat)
        (spy-on 'eca-chat-inline--read-prompt :and-return-value "hi")
        (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
        (spy-on 'eca-chat-ensure-chat-buffer)
        (spy-on 'eca-api-request-async)
        (with-current-buffer source-buffer
          (setq-local eca-chat-inline--chat-id "chat-1")
          (eca-chat-inline-prompt)
          (expect 'eca-chat-inline--select-chat :not :to-have-been-called)
          (let* ((args (spy-calls-args-for 'eca-api-request-async 0))
                 (params (plist-get (cdr args) :params)))
            (expect (plist-get (cdr args) :method)
                    :to-equal "chat/inlinePrompt")
            (expect (plist-get params :message) :to-equal "hi")
            (expect (plist-get params :chatId) :to-equal "chat-1")
            (expect (plist-member params :sourceChatId) :to-be nil))
          (let ((ov (eca-get eca-chat-inline--chat-id->overlay "chat-1")))
            (expect (overlay-buffer ov) :to-be source-buffer))))

      (it "forks the selected existing chat server-side"
        (spy-on 'eca-chat-inline--select-chat :and-return-value chat-buffer)
        (spy-on 'eca-chat-inline--read-prompt :and-return-value "why")
        (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
        (spy-on 'eca-chat-ensure-chat-buffer)
        (spy-on 'eca-uuid :and-return-value "inline-1")
        (spy-on 'eca-api-request-async)
        (with-current-buffer source-buffer
          (eca-chat-inline-prompt))
        (let* ((args (spy-calls-args-for 'eca-api-request-async 0))
               (params (plist-get (cdr args) :params)))
          (expect (plist-get (cdr args) :method)
                  :to-equal "chat/inlinePrompt")
          (expect (plist-get params :message) :to-equal "why")
          (expect (plist-get params :chatId) :to-equal "inline-1")
          (expect (plist-get params :sourceChatId) :to-equal "chat-1"))
        (expect 'eca-chat-ensure-chat-buffer
                :to-have-been-called-with session "inline-1")
        (let ((ov (eca-get eca-chat-inline--chat-id->overlay "inline-1")))
          (expect (overlay-buffer ov) :to-be source-buffer))
        (expect (buffer-local-value 'eca-chat-inline--chat-id source-buffer)
                :to-equal "inline-1"))

      (it "forks even when the selected chat is busy"
        (with-current-buffer chat-buffer
          (setq-local eca-chat--chat-loading t))
        (spy-on 'eca-chat-inline--select-chat :and-return-value chat-buffer)
        (spy-on 'eca-chat-inline--read-prompt :and-return-value "why")
        (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
        (spy-on 'eca-chat-ensure-chat-buffer)
        (spy-on 'eca-uuid :and-return-value "inline-1")
        (spy-on 'eca-api-request-async)
        (with-current-buffer source-buffer
          (eca-chat-inline-prompt))
        (let* ((args (spy-calls-args-for 'eca-api-request-async 0))
               (params (plist-get (cdr args) :params)))
          (expect (plist-get params :sourceChatId) :to-equal "chat-1")))

      (it "creates a fresh inline chat when `New chat' is picked"
        (spy-on 'eca-chat-inline--select-chat :and-return-value 'new)
        (spy-on 'eca-chat-ensure-chat-buffer)
        (spy-on 'eca-uuid :and-return-value "inline-2")
        (spy-on 'eca-chat-inline--read-prompt :and-return-value "q")
        (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
        (spy-on 'eca-api-request-async)
        (with-current-buffer source-buffer
          (eca-chat-inline-prompt)
          (expect 'eca-chat-ensure-chat-buffer
                  :to-have-been-called-with session "inline-2")
          (let* ((args (spy-calls-args-for 'eca-api-request-async 0))
                 (params (plist-get (cdr args) :params)))
            (expect (plist-get params :chatId) :to-equal "inline-2")
            (expect (plist-member params :sourceChatId) :to-be nil))
          (expect eca-chat-inline--chat-id :to-equal "inline-2")))

      (it "sends configured model, agent and variant"
        (spy-on 'eca-chat-inline--select-chat :and-return-value 'new)
        (spy-on 'eca-chat-ensure-chat-buffer)
        (spy-on 'eca-uuid :and-return-value "inline-3")
        (spy-on 'eca-chat-inline--read-prompt :and-return-value "q")
        (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
        (spy-on 'eca-api-request-async)
        (let ((eca-chat-inline-model "openai/o3")
              (eca-chat-inline-agent "plan")
              (eca-chat-inline-variant "high"))
          (with-current-buffer source-buffer
            (eca-chat-inline-prompt)))
        (let* ((args (spy-calls-args-for 'eca-api-request-async 0))
               (params (plist-get (cdr args) :params)))
          (expect (plist-get params :model) :to-equal "openai/o3")
          (expect (plist-get params :agent) :to-equal "plan")
          (expect (plist-get params :variant) :to-equal "high")))

      (it "omits model, agent and variant when not configured"
        (spy-on 'eca-chat-inline--select-chat :and-return-value 'new)
        (spy-on 'eca-chat-ensure-chat-buffer)
        (spy-on 'eca-uuid :and-return-value "inline-4")
        (spy-on 'eca-chat-inline--read-prompt :and-return-value "q")
        (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
        (spy-on 'eca-api-request-async)
        (with-current-buffer source-buffer
          (eca-chat-inline-prompt))
        (let* ((args (spy-calls-args-for 'eca-api-request-async 0))
               (params (plist-get (cdr args) :params)))
          (expect (plist-member params :model) :to-be nil)
          (expect (plist-member params :agent) :to-be nil)
          (expect (plist-member params :variant) :to-be nil)))

      (it "surfaces request errors on the overlay"
        (spy-on 'eca-chat-inline--select-chat :and-return-value 'new)
        (spy-on 'eca-chat-ensure-chat-buffer)
        (spy-on 'eca-uuid :and-return-value "inline-5")
        (spy-on 'eca-chat-inline--read-prompt :and-return-value "q")
        (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
        (spy-on 'eca-api-request-async)
        (with-current-buffer source-buffer
          (eca-chat-inline-prompt))
        (let* ((args (spy-calls-args-for 'eca-api-request-async 0))
               (error-cb (plist-get (cdr args) :error-callback))
               (ov (eca-get eca-chat-inline--chat-id->overlay "inline-5")))
          (funcall error-cb '(:message "boom"))
          (expect (overlay-get ov 'eca-chat-inline--state) :to-be 'finished)
          (expect (overlay-get ov 'before-string) :to-match "Error: boom")))

      (it "surfaces server-rejected prompts on the overlay"
        (spy-on 'eca-chat-inline--select-chat :and-return-value 'new)
        (spy-on 'eca-chat-ensure-chat-buffer)
        (spy-on 'eca-uuid :and-return-value "inline-6")
        (spy-on 'eca-chat-inline--read-prompt :and-return-value "q")
        (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
        (spy-on 'eca-api-request-async)
        (with-current-buffer source-buffer
          (eca-chat-inline-prompt))
        (let* ((args (spy-calls-args-for 'eca-api-request-async 0))
               (success-cb (plist-get (cdr args) :success-callback))
               (ov (eca-get eca-chat-inline--chat-id->overlay "inline-6")))
          (funcall success-cb '(:chatId "inline-6" :status "error"
                                :model "error"))
          (expect (overlay-get ov 'eca-chat-inline--state) :to-be 'finished)
          (expect (overlay-get ov 'before-string) :to-match "Prompt failed")))

      (it "asks for a chat again when the sticky chat was deleted"
        (spy-on 'eca-chat-inline--select-chat :and-return-value chat-buffer)
        (spy-on 'eca-chat-inline--read-prompt :and-return-value "why")
        (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
        (spy-on 'eca-chat-ensure-chat-buffer)
        (spy-on 'eca-uuid :and-return-value "inline-7")
        (spy-on 'eca-api-request-async)
        (with-current-buffer source-buffer
          (setq-local eca-chat-inline--chat-id "dead-chat")
          ;; Server-side deletion clears the stickiness (even with no
          ;; live overlay), so the next prompt picks a chat again.
          (eca-chat-inline--chat-deleted session "dead-chat")
          (expect eca-chat-inline--chat-id :to-be nil)
          (eca-chat-inline-prompt)
          (expect 'eca-chat-inline--select-chat :to-have-been-called)))

      (it "rolls back the pre-created chat when the first prompt fails"
        (spy-on 'eca-chat-inline--select-chat :and-return-value 'new)
        (spy-on 'eca-uuid :and-return-value "inline-err")
        (spy-on 'eca-chat-inline--read-prompt :and-return-value "q")
        (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
        (spy-on 'eca-chat--force-tab-line-update)
        (spy-on 'eca-api-request-async)
        (unwind-protect
            (progn
              (with-current-buffer source-buffer
                (eca-chat-inline-prompt))
              ;; The mirror buffer was really created and registered.
              (let ((created (eca-chat--get-chat-buffer session "inline-err")))
                (expect (buffer-live-p created) :to-be-truthy)
                (let* ((args (spy-calls-args-for 'eca-api-request-async 0))
                       (error-cb (plist-get (cdr args) :error-callback)))
                  (funcall error-cb '(:message "boom")))
                ;; Failure deregisters and kills it and drops the
                ;; stickiness, leaving only the overlay with the error.
                (expect (buffer-live-p created) :to-be nil)
                (expect (eca-chat--get-chat-buffer session "inline-err")
                        :to-be nil)
                (expect (buffer-local-value 'eca-chat-inline--chat-id
                                            source-buffer)
                        :to-be nil)
                (let ((ov (eca-get eca-chat-inline--chat-id->overlay
                                   "inline-err")))
                  (expect (overlay-get ov 'before-string)
                          :to-match "Error: boom"))))
          ;; Activating a real `eca-chat-mode' buffer installs global
          ;; deletion/kill/yank advices; remove them so later specs (e.g.
          ;; the read-only history ones) run in a pristine global state.
          (dolist (fn '(delete-char delete-backward-char
                        backward-delete-char
                        backward-delete-char-untabify
                        backward-kill-word))
            (advice-remove fn #'eca-chat--key-pressed-deletion))
          (dolist (fn eca-chat--kill-guarded-commands)
            (advice-remove fn #'eca-chat--key-pressed-kill))
          (advice-remove 'yank #'eca-chat--yank-considering-image)))

      (it "keeps a pre-existing chat when a follow-up prompt fails"
        (spy-on 'eca-chat-inline--read-prompt :and-return-value "more")
        (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
        (spy-on 'eca-api-request-async)
        (with-current-buffer source-buffer
          (setq-local eca-chat-inline--chat-id "chat-1")
          (eca-chat-inline-prompt))
        (let* ((args (spy-calls-args-for 'eca-api-request-async 0))
               (error-cb (plist-get (cdr args) :error-callback)))
          (funcall error-cb '(:message "boom")))
        (expect (buffer-live-p chat-buffer) :to-be-truthy)
        (expect (eca-chat--get-chat-buffer session "chat-1")
                :to-be chat-buffer)
        (expect (buffer-local-value 'eca-chat-inline--chat-id source-buffer)
                :to-equal "chat-1"))

      (it "errors when the sticky chat is busy"
        (with-current-buffer chat-buffer
          (setq-local eca-chat--chat-loading t))
        (with-current-buffer source-buffer
          (setq-local eca-chat-inline--chat-id "chat-1")
          (expect (eca-chat-inline-prompt) :to-throw 'user-error)))

      (it "forces chat selection with a prefix argument"
        (spy-on 'eca-chat-inline--select-chat :and-return-value 'new)
        (spy-on 'eca-chat-ensure-chat-buffer)
        (spy-on 'eca-uuid :and-return-value "inline-8")
        (spy-on 'eca-chat-inline--read-prompt :and-return-value "q")
        (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
        (spy-on 'eca-api-request-async)
        (with-current-buffer source-buffer
          (setq-local eca-chat-inline--chat-id "chat-1")
          (eca-chat-inline-prompt '(4))
          (expect 'eca-chat-inline--select-chat :to-have-been-called)
          (expect eca-chat-inline--chat-id :to-equal "inline-8")))

      (it "dismisses the overlay at point when forcing selection"
        (spy-on 'eca-chat-inline--select-chat :and-return-value 'new)
        (spy-on 'eca-chat-ensure-chat-buffer)
        (spy-on 'eca-uuid :and-return-value "inline-9")
        (spy-on 'eca-chat-inline--read-prompt :and-return-value "q")
        (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
        (spy-on 'eca-api-request-async)
        (with-current-buffer source-buffer
          (goto-char (point-min))
          (let ((old-ov (eca-chat-inline--setup-overlay
                         session (line-beginning-position)
                         (line-end-position))))
            (eca-chat-inline--bind-overlay old-ov "chat-1")
            (eca-chat-inline-prompt '(4))
            (expect (overlay-buffer old-ov) :to-be nil)
            (expect 'eca-chat-inline--select-chat :to-have-been-called))))

      (it "sends follow-ups to the overlay's chat"
        (spy-on 'eca-chat-inline--read-prompt :and-return-value "more")
        (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
        (spy-on 'eca-api-request-async)
        (with-current-buffer source-buffer
          (goto-char (point-min))
          (let ((ov (eca-chat-inline--setup-overlay
                     session (line-beginning-position)
                     (line-end-position))))
            (eca-chat-inline--bind-overlay ov "chat-1")
            (overlay-put ov 'eca-chat-inline--text-acc "old answer")
            (eca-chat-inline-prompt)
            (let* ((args (spy-calls-args-for 'eca-api-request-async 0))
                   (params (plist-get (cdr args) :params)))
              (expect (plist-get (cdr args) :method)
                      :to-equal "chat/inlinePrompt")
              (expect (plist-get params :chatId) :to-equal "chat-1")
              (expect (plist-get params :message) :to-equal "more")
              (expect (plist-member params :sourceChatId) :to-be nil))
            (expect (overlay-get ov 'eca-chat-inline--text-acc)
                    :to-equal "")))))

    (describe "content routing"
      (let (ov)
        (before-each
          (with-current-buffer source-buffer
            (setq ov (eca-chat-inline--setup-overlay
                      session (line-beginning-position) (line-end-position)))
            (eca-chat-inline--bind-overlay ov "chat-1")))

        (it "accumulates assistant text and resets it on user text"
          (eca-chat-inline-test--content session "chat-1" "user"
                                         '(:type "text" :text "q"))
          (eca-chat-inline-test--content session "chat-1" "assistant"
                                         '(:type "text" :text "Hello "))
          (eca-chat-inline-test--content session "chat-1" "assistant"
                                         '(:type "text" :text "world"))
          (expect (overlay-get ov 'eca-chat-inline--text-acc)
                  :to-equal "Hello world")
          (eca-chat-inline-test--content session "chat-1" "user"
                                         '(:type "text" :text "q2"))
          (expect (overlay-get ov 'eca-chat-inline--text-acc) :to-equal ""))

        (it "surfaces system error text in the answer area"
          (eca-chat-inline-test--content session "chat-1" "user"
                                         '(:type "text" :text "q"))
          (eca-chat-inline-test--content
           session "chat-1" "system"
           '(:type "text" :text "Error: provider overloaded"))
          (eca-chat-inline-test--content session "chat-1" "system"
                                         '(:type "progress" :state "finished"))
          (expect (overlay-get ov 'eca-chat-inline--text-acc)
                  :to-match "provider overloaded")
          (expect (overlay-get ov 'before-string)
                  :to-match "provider overloaded"))

        (it "ignores content of another session's chat with the same id"
          (let ((other (make-eca--session :id "session-2")))
            (eca-chat-inline-test--content other "chat-1" "assistant"
                                           '(:type "text" :text "nope"))
            (expect (overlay-get ov 'eca-chat-inline--text-acc)
                    :to-equal "")))

        (it "finishes the overlay on progress finished"
          (eca-chat-inline-test--content session "chat-1" "assistant"
                                         '(:type "text" :text "done text"))
          (eca-chat-inline-test--content session "chat-1" "system"
                                         '(:type "progress" :state "finished"))
          (expect (overlay-get ov 'eca-chat-inline--state) :to-be 'finished)
          (expect (overlay-get ov 'before-string) :to-match "ECA: Done")
          (expect (overlay-get ov 'before-string) :to-match "done text")
          (expect (overlay-get ov 'before-string) :to-match "reply"))

        (it "never shows the chat title in the header"
          (eca-chat-inline-test--content session "chat-1" "system"
                                         '(:type "metadata" :title "My title"))
          (eca-chat-inline-test--content session "chat-1" "assistant"
                                         '(:type "text" :text "hi"))
          (expect (overlay-get ov 'before-string) :not :to-match "My title"))

        (it "ignores content of unrelated chats"
          (eca-chat-inline-test--content session "other" "assistant"
                                         '(:type "text" :text "nope"))
          (expect (overlay-get ov 'eca-chat-inline--text-acc) :to-equal ""))

        (it "tracks tool calls pending approval"
          (eca-chat-inline-test--content
           session "chat-1" "assistant"
           '(:type "toolCallRun" :id "t1" :name "shell" :manualApproval t))
          (expect (overlay-get ov 'eca-chat-inline--pending-tools)
                  :to-equal '("t1"))
          (expect (overlay-get ov 'before-string) :to-match "needs approval")
          (eca-chat-inline-test--content
           session "chat-1" "assistant"
           '(:type "toolCalled" :id "t1" :name "shell"))
          (expect (overlay-get ov 'eca-chat-inline--pending-tools) :to-be nil))

        (it "shows the tool call summary while waiting for approval"
          (eca-chat-inline-test--content
           session "chat-1" "assistant"
           '(:type "toolCallRun" :id "t1" :name "shell"
             :summary "Running tests" :manualApproval t))
          (expect (overlay-get ov 'before-string) :to-match "Running tests")
          ;; The generic server progress text must not replace the
          ;; summary while the approval is pending.
          (eca-chat-inline-test--content
           session "chat-1" "system"
           '(:type "progress" :state "running"
             :text "Waiting for tool call approval"))
          (expect (overlay-get ov 'before-string)
                  :not :to-match "Waiting for tool call approval")
          (expect (overlay-get ov 'before-string) :to-match "Running tests"))

        (it "keeps the remaining summaries when one of the tools resolves"
          (eca-chat-inline-test--content
           session "chat-1" "assistant"
           '(:type "toolCallRun" :id "t1" :name "shell"
             :summary "Running tests" :manualApproval t))
          (eca-chat-inline-test--content
           session "chat-1" "assistant"
           '(:type "toolCallRun" :id "t2" :name "edit"
             :summary "Editing foo.el" :manualApproval t))
          (expect (overlay-get ov 'before-string)
                  :to-match "Running tests, Editing foo.el")
          (eca-chat-inline-test--content
           session "chat-1" "assistant"
           '(:type "toolCalled" :id "t1" :name "shell"))
          (expect (overlay-get ov 'before-string) :to-match "Editing foo.el")
          (expect (overlay-get ov 'before-string)
                  :not :to-match "Running tests"))

        (it "resumes showing progress text once approvals resolve"
          (eca-chat-inline-test--content
           session "chat-1" "assistant"
           '(:type "toolCallRun" :id "t1" :name "shell"
             :summary "Running tests" :manualApproval t))
          (eca-chat-inline-test--content
           session "chat-1" "assistant"
           '(:type "toolCallRunning" :id "t1" :name "shell"
             :summary "Running tests"))
          (expect (overlay-get ov 'eca-chat-inline--pending-tools) :to-be nil)
          (eca-chat-inline-test--content
           session "chat-1" "system"
           '(:type "progress" :state "running" :text "Generating"))
          (expect (overlay-get ov 'before-string) :to-match "Generating"))

        (it "approves pending tool calls from the overlay"
          (spy-on 'eca-api-notify)
          (eca-chat-inline-test--content
           session "chat-1" "assistant"
           '(:type "toolCallRun" :id "t1" :name "shell" :manualApproval t))
          (with-current-buffer source-buffer
            (goto-char (point-min))
            (eca-chat-inline-approve-tool-call))
          (let* ((args (spy-calls-args-for 'eca-api-notify 0))
                 (params (plist-get (cdr args) :params)))
            (expect (plist-get (cdr args) :method)
                    :to-equal "chat/toolCallApprove")
            (expect (plist-get params :toolCallId) :to-equal "t1")
            (expect (plist-get params :chatId) :to-equal "chat-1"))
          (expect (overlay-get ov 'eca-chat-inline--pending-tools) :to-be nil))))

    (describe "anchor and rendering"
      (it "clamps stale anchor bounds to the buffer limits"
        ;; Bounds are captured before the prompt minibuffer: the buffer
        ;; may have shrunk while it was open (e.g. a tool call edit).
        (with-current-buffer source-buffer
          (let ((ov (eca-chat-inline--setup-overlay session 1 99999)))
            (expect (overlay-end ov) :to-equal (point-max))
            (delete-overlay ov))))

      (it "keeps the overlay alive when invoked on an empty line"
        (with-current-buffer source-buffer
          (goto-char (point-max))
          (insert "\n")
          (forward-line -1)
          (let* ((bounds (eca-chat-inline--anchor-bounds))
                 (ov (eca-chat-inline--setup-overlay
                      session (car bounds) (cdr bounds))))
            (expect (< (car bounds) (cdr bounds)) :to-be-truthy)
            (expect (overlay-buffer ov) :not :to-be nil))))

      (it "renders the answer above the anchor in the before-string"
        (with-current-buffer source-buffer
          (goto-char (point-min))
          (let ((ov (eca-chat-inline--setup-overlay
                     session (line-beginning-position) (line-end-position))))
            (eca-chat-inline--bind-overlay ov "chat-1")
            (eca-chat-inline-test--content session "chat-1" "assistant"
                                           '(:type "text" :text "hello"))
            (expect (overlay-get ov 'before-string) :to-match "hello")
            (expect (overlay-get ov 'after-string) :to-be nil))))

      (it "hides markdown markup in the rendered answer"
        (with-current-buffer source-buffer
          (let ((ov (eca-chat-inline--setup-overlay
                     session (point-min) (1+ (point-min)))))
            (overlay-put ov 'eca-chat-inline--text-acc "some **bold** text")
            (let ((body (eca-chat-inline--fontified-body ov)))
              (expect (text-property-any 0 (length body) 'invisible t body)
                      :not :to-be nil)))))

      (it "keeps markup visible when eca-chat-hide-markdown-markup is nil"
        (with-current-buffer source-buffer
          (let ((eca-chat-hide-markdown-markup nil)
                (ov (eca-chat-inline--setup-overlay
                     session (point-min) (1+ (point-min)))))
            (overlay-put ov 'eca-chat-inline--text-acc "some **bold** text")
            (let ((body (eca-chat-inline--fontified-body ov)))
              (expect (text-property-any 0 (length body) 'invisible t body)
                      :to-be nil)))))

      (it "viewport follows the tail streaming and the head when done"
        (with-current-buffer source-buffer
          (let ((eca-chat-inline-max-lines 3)
                (ov (eca-chat-inline--setup-overlay
                     session (point-min) (1+ (point-min)))))
            (expect (eca-chat-inline--viewport ov "1\n2\n3\n4\n5" nil)
                    :to-match "5$")
            (expect (eca-chat-inline--viewport ov "1\n2\n3\n4\n5" nil)
                    :to-match "↑ \\+2 lines")
            (expect (eca-chat-inline--viewport ov "1\n2\n3\n4\n5" t)
                    :to-match "^1\n")
            (expect (eca-chat-inline--viewport ov "1\n2\n3\n4\n5" t)
                    :to-match "↓ \\+2 lines (n)")
            (expect (eca-chat-inline--viewport ov "1\n2" nil)
                    :to-equal "1\n2"))))

      (it "hard-wraps long answer lines to the wrap column"
        (with-current-buffer source-buffer
          (let ((eca-chat-inline-wrap-column 10)
                (ov (eca-chat-inline--setup-overlay
                     session (point-min) (1+ (point-min)))))
            (overlay-put ov 'eca-chat-inline--text-acc
                         "aaa bbb ccc ddd eee fff\n\nshort")
            (let* ((body (eca-chat-inline--fontified-body ov))
                   (lines (split-string body "\n")))
              (expect (-every? (lambda (line) (<= (length line) 10)) lines)
                      :to-be-truthy)
              (expect body :to-match "short")))))

      (it "scrolls the viewport with clamping"
        (with-current-buffer source-buffer
          (let ((eca-chat-inline-max-lines 2)
                (ov (eca-chat-inline--setup-overlay
                     session (point-min) (1+ (point-min)))))
            (overlay-put ov 'eca-chat-inline--text-acc "1\n2\n3\n4\n5")
            (overlay-put ov 'eca-chat-inline--state 'finished)
            (eca-chat-inline--refresh ov)
            (eca-chat-inline--scroll-viewport ov 1)
            (expect (overlay-get ov 'eca-chat-inline--scroll) :to-equal 1)
            (eca-chat-inline--scroll-viewport ov 100)
            (expect (overlay-get ov 'eca-chat-inline--scroll) :to-equal 3)
            (eca-chat-inline--scroll-viewport ov -100)
            (expect (overlay-get ov 'eca-chat-inline--scroll) :to-equal 0))))

      (it "refuses to scroll when the answer fits"
        (with-current-buffer source-buffer
          (let ((eca-chat-inline-max-lines 10)
                (ov (eca-chat-inline--setup-overlay
                     session (point-min) (1+ (point-min)))))
            (overlay-put ov 'eca-chat-inline--text-acc "short")
            (eca-chat-inline--refresh ov)
            (expect (eca-chat-inline--scroll-viewport ov 1)
                    :to-throw 'user-error))))

      (it "resets manual scroll on a new turn"
        (with-current-buffer source-buffer
          (let ((ov (eca-chat-inline--setup-overlay
                     session (point-min) (1+ (point-min)))))
            (eca-chat-inline--bind-overlay ov "chat-1")
            (overlay-put ov 'eca-chat-inline--scroll 2)
            (eca-chat-inline-test--content session "chat-1" "user"
                                           '(:type "text" :text "q"))
            (expect (overlay-get ov 'eca-chat-inline--scroll) :to-be nil))))

      (it "anchors over the trailing newline so eol keeps the keymap"
        (with-current-buffer source-buffer
          (goto-char (point-min))
          (let ((bounds (eca-chat-inline--anchor-bounds)))
            (expect (cdr bounds) :to-equal (1+ (line-end-position)))))))

    (describe "keybindings"
      (it "binds single-letter actions on the anchor keymap"
        (expect (lookup-key eca-chat-inline-actions-map (kbd "r"))
                :to-be #'eca-chat-inline-reply)
        (expect (lookup-key eca-chat-inline-actions-map (kbd "q"))
                :to-be #'eca-chat-inline-dismiss)
        (expect (lookup-key eca-chat-inline-actions-map (kbd "s"))
                :to-be #'eca-chat-inline-stop)
        (expect (lookup-key eca-chat-inline-actions-map (kbd "a"))
                :to-be #'eca-chat-inline-approve-tool-call)
        (expect (lookup-key eca-chat-inline-actions-map (kbd "d"))
                :to-be #'eca-chat-inline-reject-tool-call)
        (expect (lookup-key eca-chat-inline-actions-map (kbd "m"))
                :to-be #'eca-chat-inline-menu)
        (expect (lookup-key eca-chat-inline-actions-map (kbd "n"))
                :to-be #'eca-chat-inline-scroll-up)
        (expect (lookup-key eca-chat-inline-actions-map (kbd "p"))
                :to-be #'eca-chat-inline-scroll-down)
        (expect (lookup-key eca-chat-inline-actions-map (kbd "f"))
                :to-be nil)
        (expect (lookup-key eca-chat-inline-actions-map (kbd "o"))
                :to-be nil))

      (it "keeps buffer-wide scroll keys on the viewport map"
        (expect (lookup-key eca-chat-inline-viewport-map (kbd "C-M-v"))
                :to-be #'eca-chat-inline-scroll-up)
        (expect (lookup-key eca-chat-inline-viewport-map (kbd "C-M-S-v"))
                :to-be #'eca-chat-inline-scroll-down))

      (it "dispatches the letter keys through the overlay keymap"
        ;; Real command-loop dispatch: the keymap char property is
        ;; resolved against the selected window's buffer.
        (let ((prev (window-buffer (selected-window))))
          (unwind-protect
              (with-current-buffer source-buffer
                (set-window-buffer (selected-window) source-buffer)
                (goto-char (point-min))
                (let ((eca-chat-inline-max-lines 2)
                      (ov (eca-chat-inline--setup-overlay
                           session (line-beginning-position)
                           (line-end-position))))
                  (overlay-put ov 'eca-chat-inline--text-acc "1\n2\n3\n4\n5")
                  (overlay-put ov 'eca-chat-inline--state 'finished)
                  (eca-chat-inline--refresh ov)
                  (goto-char (overlay-start ov))
                  (set-window-point (selected-window) (point))
                  (execute-kbd-macro "n")
                  (expect (overlay-get ov 'eca-chat-inline--scroll)
                          :to-equal 1)
                  (execute-kbd-macro "q")
                  (expect (overlay-buffer ov) :to-be nil)))
            (set-window-buffer (selected-window) prev)))))

    (describe "toggle overlays"
      (after-each
        (setq eca-chat-inline--overlays-hidden nil))

      (it "hides display, anchor face and keymap, showing back on toggle"
        (with-current-buffer source-buffer
          (goto-char (point-min))
          (let ((ov (eca-chat-inline--setup-overlay
                     session (line-beginning-position)
                     (line-end-position))))
            (eca-chat-inline--bind-overlay ov "chat-1")
            (overlay-put ov 'eca-chat-inline--text-acc "answer")
            (eca-chat-inline--refresh ov)
            (expect (overlay-get ov 'before-string) :not :to-be nil)
            (eca-chat-inline-toggle-overlays)
            (expect (overlay-get ov 'before-string) :to-be nil)
            (expect (overlay-get ov 'face) :to-be nil)
            (expect (overlay-get ov 'keymap) :to-be nil)
            (eca-chat-inline-toggle-overlays)
            (expect (overlay-get ov 'before-string) :to-match "answer")
            (expect (overlay-get ov 'keymap)
                    :to-be eca-chat-inline-actions-map))))

      (it "keeps streaming into hidden overlays"
        (with-current-buffer source-buffer
          (goto-char (point-min))
          (let ((ov (eca-chat-inline--setup-overlay
                     session (line-beginning-position)
                     (line-end-position))))
            (eca-chat-inline--bind-overlay ov "chat-1")
            (eca-chat-inline-toggle-overlays)
            (eca-chat-inline-test--content session "chat-1" "assistant"
                                           '(:type "text" :text "hidden"))
            (expect (overlay-get ov 'eca-chat-inline--text-acc)
                    :to-equal "hidden")
            (expect (overlay-get ov 'before-string) :to-be nil))))

      (it "shows overlays back on a new inline prompt"
        (spy-on 'eca-chat-inline--read-prompt :and-return-value "hi")
        (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
        (spy-on 'eca-chat-ensure-chat-buffer)
        (spy-on 'eca-api-request-async)
        (setq eca-chat-inline--overlays-hidden t)
        (with-current-buffer source-buffer
          (setq-local eca-chat-inline--chat-id "chat-1")
          (eca-chat-inline-prompt)
          (expect eca-chat-inline--overlays-hidden :to-be nil))))

    (describe "settings"
      (it "sets and resets the inline model"
        (let ((eca-chat-inline-model nil))
          (spy-on 'completing-read :and-return-value "openai/o3")
          (eca-chat-inline-select-model)
          (expect eca-chat-inline-model :to-equal "openai/o3")
          (spy-on 'completing-read :and-return-value "")
          (eca-chat-inline-select-model)
          (expect eca-chat-inline-model :to-be nil))))

    (describe "viewport mode"
      (it "is enabled while an overlay exists and disabled after"
        (with-current-buffer source-buffer
          (let ((ov (eca-chat-inline--setup-overlay
                     session (point-min) (1+ (point-min)))))
            (expect eca-chat-inline--viewport-mode :to-be-truthy)
            (eca-chat-inline--delete-overlay ov)
            (expect eca-chat-inline--viewport-mode :to-be nil))))

      (it "scrolls from anywhere in the buffer"
        (with-current-buffer source-buffer
          (goto-char (point-min))
          (let ((eca-chat-inline-max-lines 2)
                (ov (eca-chat-inline--setup-overlay
                     session (line-beginning-position)
                     (line-end-position))))
            (overlay-put ov 'eca-chat-inline--text-acc "1\n2\n3\n4\n5")
            (overlay-put ov 'eca-chat-inline--state 'finished)
            (eca-chat-inline--refresh ov)
            (goto-char (point-max))
            (eca-chat-inline-scroll-up)
            (expect (overlay-get ov 'eca-chat-inline--scroll) :to-equal 1))))

      (it "errors instead of scrolling when the answer fits"
        (with-current-buffer source-buffer
          (let ((ov (eca-chat-inline--setup-overlay
                     session (point-min) (1+ (point-min)))))
            (overlay-put ov 'eca-chat-inline--text-acc "short")
            (eca-chat-inline--refresh ov)
            (expect (eca-chat-inline-scroll-up) :to-throw 'user-error)))))

    (describe "cleanup"
      (let (ov)
        (before-each
          (with-current-buffer source-buffer
            (setq ov (eca-chat-inline--setup-overlay
                      session (line-beginning-position) (line-end-position)))
            (eca-chat-inline--bind-overlay ov "chat-1")))

        (it "drops overlay and stickiness when the chat is deleted"
          (eca-chat-inline--chat-deleted session "chat-1")
          (expect (eca-get eca-chat-inline--chat-id->overlay "chat-1")
                  :to-be nil)
          (expect (overlay-buffer ov) :to-be nil)
          (expect (buffer-local-value 'eca-chat-inline--chat-id source-buffer)
                  :to-be nil))

        (it "dismisses the overlay at point keeping the stickiness"
          (with-current-buffer source-buffer
            (goto-char (point-min))
            (eca-chat-inline-dismiss)
            (expect (overlay-buffer ov) :to-be nil)
            (expect eca-chat-inline--chat-id :to-equal "chat-1")))

        (it "clears stickiness on deletion even after the overlay was dismissed"
          (with-current-buffer source-buffer
            (goto-char (point-min))
            (eca-chat-inline-dismiss))
          (eca-chat-inline--chat-deleted session "chat-1")
          (expect (buffer-local-value 'eca-chat-inline--chat-id source-buffer)
                  :to-be nil))

        (it "detaches the buffer association and overlay"
          (with-current-buffer source-buffer
            (eca-chat-inline-detach)
            (expect (overlay-buffer ov) :to-be nil)
            (expect eca-chat-inline--chat-id :to-be nil)))

        (it "kills the temp buffer when the overlay is deleted"
          (let ((temp (overlay-get ov 'eca-chat-inline--temp-buffer)))
            (expect (buffer-live-p temp) :to-be-truthy)
            (eca-chat-inline--delete-overlay ov)
            (expect (buffer-live-p temp) :to-be nil)))

        (it "cleans up overlays when the source buffer is killed"
          (let ((temp (overlay-get ov 'eca-chat-inline--temp-buffer)))
            (kill-buffer source-buffer)
            (expect (buffer-live-p temp) :to-be nil)
            (expect (eca-get eca-chat-inline--chat-id->overlay "chat-1")
                    :to-be nil)))

        (it "sweeps dead overlays on session status changes"
          (let ((temp (overlay-get ov 'eca-chat-inline--temp-buffer)))
            ;; Simulate an evaporated overlay: deleted without cleanup.
            (delete-overlay ov)
            (eca-chat-inline--session-status-changed session)
            (expect (buffer-live-p temp) :to-be nil)
            (expect (eca-get eca-chat-inline--chat-id->overlay "chat-1")
                    :to-be nil)))

        (it "recreates a killed backing chat buffer on reply"
          (spy-on 'eca-chat-inline--read-prompt :and-return-value "more")
          (spy-on 'eca-chat-inline--dwim-contexts :and-return-value nil)
          (spy-on 'eca-api-request-async)
          (let ((recreated (generate-new-buffer "*eca-inline-test-rechat*")))
            (unwind-protect
                (progn
                  (with-current-buffer recreated
                    (setq-local eca-chat--id "chat-1"))
                  (spy-on 'eca-chat-ensure-chat-buffer
                          :and-return-value recreated)
                  (kill-buffer chat-buffer)
                  (with-current-buffer source-buffer
                    (goto-char (overlay-start ov))
                    (eca-chat-inline-reply))
                  (expect 'eca-chat-ensure-chat-buffer
                          :to-have-been-called-with session "chat-1")
                  (let* ((args (spy-calls-args-for 'eca-api-request-async 0))
                         (params (plist-get (cdr args) :params)))
                    (expect (plist-get params :chatId) :to-equal "chat-1")))
              (kill-buffer recreated))))

        (it "stops via a raw notification when the chat buffer is gone"
          (spy-on 'eca-api-notify)
          (kill-buffer chat-buffer)
          (with-current-buffer source-buffer
            (goto-char (overlay-start ov))
            (eca-chat-inline-stop))
          (let* ((args (spy-calls-args-for 'eca-api-notify 0))
                 (params (plist-get (cdr args) :params)))
            (expect (plist-get (cdr args) :method)
                    :to-equal "chat/promptStop")
            (expect (plist-get params :chatId) :to-equal "chat-1")))

        (it "stops via a raw notification before the chat reports loading"
          ;; Right after sending, statusChanged may not have arrived yet,
          ;; so the mirror buffer is not loading; `s' must still stop.
          (spy-on 'eca-api-notify)
          (with-current-buffer source-buffer
            (goto-char (overlay-start ov))
            (eca-chat-inline-stop))
          (let* ((args (spy-calls-args-for 'eca-api-notify 0))
                 (params (plist-get (cdr args) :params)))
            (expect (plist-get (cdr args) :method)
                    :to-equal "chat/promptStop")
            (expect (plist-get params :chatId) :to-equal "chat-1")))

        (it "stops through the mirror buffer when it is loading"
          (spy-on 'eca-api-notify)
          (spy-on 'eca-chat--force-tab-line-update)
          (with-current-buffer chat-buffer
            (setq-local eca-chat--chat-loading t)
            (setq-local eca-chat--id "chat-1"))
          (unwind-protect
              (progn
                (with-current-buffer source-buffer
                  (goto-char (overlay-start ov))
                  (eca-chat-inline-stop))
                ;; `eca-chat--stop-prompt' ran in the mirror: it notified
                ;; the server and moved the chat to the stopping state.
                (let* ((args (spy-calls-args-for 'eca-api-notify 0))
                       (params (plist-get (cdr args) :params)))
                  (expect (plist-get (cdr args) :method)
                          :to-equal "chat/promptStop")
                  (expect (plist-get params :chatId) :to-equal "chat-1"))
                (expect (buffer-local-value 'eca-chat--chat-loading
                                            chat-buffer)
                        :to-be 'stopping))
            (with-current-buffer chat-buffer
              (when (timerp eca-chat--stopping-safety-timer)
                (cancel-timer eca-chat--stopping-safety-timer)))))))))

;;; eca-chat-inline-test.el ends here
