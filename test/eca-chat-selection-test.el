;;; eca-chat-selection-test.el --- Chat selection state tests -*- lexical-binding: t; -*-
;;; Commentary:
;; Verify that model, variant, and agent state is routed to the active chat.
;;; Code:

(require 'buttercup)
(require 'eca)
(require 'eca-chat)

(defun eca-selection-test--make-chat (session id)
  "Create and register a chat buffer with ID for SESSION."
  (let ((buffer (generate-new-buffer
                 (format " *eca-selection-test:%s*" id))))
    (with-current-buffer buffer
      (setq major-mode 'eca-chat-mode)
      (setq-local eca-chat--id id)
      (setq-local eca-chat--closed nil))
    (setf (eca--session-chats session)
          (eca-assoc (eca--session-chats session) id buffer))
    buffer))

(defun eca-selection-test--kill-buffers (&rest buffers)
  "Kill live BUFFERS without running chat hooks."
  (let ((kill-buffer-hook nil))
    (dolist (buffer buffers)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(describe "eca config chat scoping"
  (it "applies a top-level chatId update only to its chat"
    (let ((eca-chat--last-known-model nil)
          (eca-chat--last-known-agent nil)
          (eca-chat--last-known-variant nil)
          (eca-chat--last-known-trust nil)
          (session (make-eca--session))
          a b)
      (unwind-protect
          (progn
            (setq a (eca-selection-test--make-chat session "A")
                  b (eca-selection-test--make-chat session "B"))
            (with-current-buffer a
              (setq-local eca-chat--selected-model "model-a")
              (setq-local eca-chat--selected-agent "agent-a")
              (setq-local eca-chat--selected-variant "low")
              (setq-local eca-chat--selected-trust nil))
            (with-current-buffer b
              (setq-local eca-chat--selected-model "model-b")
              (setq-local eca-chat--selected-agent "agent-b")
              (setq-local eca-chat--selected-variant "medium")
              (setq-local eca-chat--selected-trust nil))
            (eca-config-updated
             session
             '(:chatId "A"
               :chat (:selectModel "model-new"
                      :selectAgent "agent-new"
                      :selectVariant "high"
                      :selectTrust t)))
            (expect (buffer-local-value 'eca-chat--selected-model a)
                    :to-equal "model-new")
            (expect (buffer-local-value 'eca-chat--selected-agent a)
                    :to-equal "agent-new")
            (expect (buffer-local-value 'eca-chat--selected-variant a)
                    :to-equal "high")
            (expect (buffer-local-value 'eca-chat--selected-trust a)
                    :to-be-truthy)
            (expect (buffer-local-value 'eca-chat--selected-model b)
                    :to-equal "model-b")
            (expect (buffer-local-value 'eca-chat--selected-agent b)
                    :to-equal "agent-b")
            (expect (buffer-local-value 'eca-chat--selected-variant b)
                    :to-equal "medium")
            (expect (buffer-local-value 'eca-chat--selected-trust b)
                    :to-be nil))
        (eca-selection-test--kill-buffers a b))))

  (it "keeps legacy unscoped updates session-wide"
    (let ((eca-chat--last-known-model nil)
          (session (make-eca--session))
          a b)
      (unwind-protect
          (progn
            (setq a (eca-selection-test--make-chat session "A")
                  b (eca-selection-test--make-chat session "B"))
            (eca-config-updated
             session
             '(:chat (:selectModel "model-default")))
            (expect (buffer-local-value 'eca-chat--selected-model a)
                    :to-equal "model-default")
            (expect (buffer-local-value 'eca-chat--selected-model b)
                    :to-equal "model-default"))
        (eca-selection-test--kill-buffers a b)))))

(describe "chat variant isolation"
  (it "scopes available variants without changing session defaults"
    (let ((session (make-eca--session :chat-variants '("default")))
          a b)
      (unwind-protect
          (progn
            (setq a (eca-selection-test--make-chat session "A")
                  b (eca-selection-test--make-chat session "B"))
            (with-current-buffer a
              (setq-local eca-chat--available-variants '("old-a")))
            (with-current-buffer b
              (setq-local eca-chat--available-variants '("old-b")))
            (eca-config-updated
             session
             '(:chatId "A" :chat (:variants ["focused" "fast"])))
            (expect (buffer-local-value 'eca-chat--available-variants a)
                    :to-equal '("focused" "fast"))
            (expect (buffer-local-value 'eca-chat--available-variants b)
                    :to-equal '("old-b"))
            (expect (eca--session-chat-variants session)
                    :to-equal '("default")))
        (eca-selection-test--kill-buffers a b))))

  (it "applies unscoped variants as the default for all chats"
    (let ((session (make-eca--session :chat-variants '("old")))
          a b)
      (unwind-protect
          (progn
            (setq a (eca-selection-test--make-chat session "A")
                  b (eca-selection-test--make-chat session "B"))
            (with-current-buffer a
              (setq-local eca-chat--available-variants '("old-a")))
            (with-current-buffer b
              (setq-local eca-chat--available-variants '("old-b")))
            (eca-config-updated
             session
             '(:chat (:variants ["new-low" "new-high"])))
            (expect (eca--session-chat-variants session)
                    :to-equal '("new-low" "new-high"))
            (expect (buffer-local-value 'eca-chat--available-variants a)
                    :to-equal '("new-low" "new-high"))
            (expect (buffer-local-value 'eca-chat--available-variants b)
                    :to-equal '("new-low" "new-high")))
        (eca-selection-test--kill-buffers a b))))

  (it "uses the last chat local variants from a source buffer"
    (let ((session (make-eca--session
                    :chat-variants '("session-only")))
          chat source seen-candidates)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt collection &rest _)
                (setq seen-candidates (all-completions "" collection))
                "zeta"))
      (unwind-protect
          (progn
            (setq chat (eca-selection-test--make-chat session "A")
                  source (generate-new-buffer " *eca-selection-source*"))
            (with-current-buffer chat
              (setq-local eca-chat--available-variants '("zeta" "alpha")))
            (setf (eca--session-last-chat-buffer session) chat)
            (with-current-buffer source
              (eca-chat-select-variant))
            (expect seen-candidates :to-equal '("-" "alpha" "zeta"))
            (expect (buffer-local-value 'eca-chat--selected-variant chat)
                    :to-equal "zeta"))
        (eca-selection-test--kill-buffers chat source))))

  (it "stores the no-variant UI choice as nil"
    (let ((eca-chat--last-known-variant "old")
          (session (make-eca--session :chat-variants '("high")))
          chat)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'completing-read :and-return-value "-")
      (unwind-protect
          (progn
            (setq chat (eca-selection-test--make-chat session "A"))
            (with-current-buffer chat
              (setq-local eca-chat--available-variants '("high"))
              (setq-local eca-chat--selected-variant "high"))
            (setf (eca--session-last-chat-buffer session) chat)
            (with-current-buffer chat
              (eca-chat-select-variant))
            (expect (buffer-local-value 'eca-chat--selected-variant chat)
                    :to-be nil)
            (expect eca-chat--last-known-variant :to-be nil))
        (eca-selection-test--kill-buffers chat))))

  (it "omits a legacy no-variant sentinel from model notifications"
    (let ((eca-chat--last-known-model nil)
          (session (make-eca--session :models '("new-model")))
          chat)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'completing-read :and-return-value "new-model")
      (spy-on 'eca-api-notify)
      (unwind-protect
          (progn
            (setq chat (eca-selection-test--make-chat session "A"))
            (with-current-buffer chat
              (setq-local eca-chat--selected-variant "-"))
            (setf (eca--session-last-chat-buffer session) chat)
            (with-current-buffer chat
              (eca-chat-select-model))
            (expect 'eca-api-notify :to-have-been-called-with
                    session
                    :method "chat/selectedModelChanged"
                    :params '(:model "new-model" :chatId "A")))
        (eca-selection-test--kill-buffers chat)))))

(describe "eca-chat--get-active-buffer"
  (it "prefers the current registered chat over last-chat-buffer"
    (let ((session (make-eca--session)) a b)
      (unwind-protect
          (progn
            (setq a (eca-selection-test--make-chat session "A")
                  b (eca-selection-test--make-chat session "B"))
            (setf (eca--session-last-chat-buffer session) b)
            (with-current-buffer a
              (expect (eca-chat--get-active-buffer session) :to-be a)))
        (eca-selection-test--kill-buffers a b))))

  (it "uses last-chat-buffer outside a registered chat"
    (let ((session (make-eca--session)) chat source)
      (unwind-protect
          (progn
            (setq chat (eca-selection-test--make-chat session "A")
                  source (generate-new-buffer " *eca-selection-source*"))
            (setf (eca--session-last-chat-buffer session) chat)
            (with-current-buffer source
              (expect (eca-chat--get-active-buffer session) :to-be chat)))
        (eca-selection-test--kill-buffers chat source)))))

(describe "chat agent selection routing"
  (it "targets last-chat-buffer when invoked from a source buffer"
    (let ((eca-chat--last-known-agent nil)
          (session (make-eca--session :chat-agents '("old" "new")))
          chat source)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'completing-read :and-return-value "new")
      (spy-on 'eca-api-notify)
      (unwind-protect
          (progn
            (setq chat (eca-selection-test--make-chat session "A")
                  source (generate-new-buffer " *eca-selection-source*"))
            (with-current-buffer chat
              (setq-local eca-chat--selected-agent "old"))
            (setf (eca--session-last-chat-buffer session) chat)
            (with-current-buffer source
              (eca-chat-select-agent))
            (expect (buffer-local-value 'eca-chat--selected-agent chat)
                    :to-equal "new")
            (expect (buffer-local-value 'eca-chat--selected-agent source)
                    :to-be nil)
            (expect 'eca-api-notify :to-have-been-called-with
                    session
                    :method "chat/selectedAgentChanged"
                    :params '(:agent "new" :chatId "A")))
        (eca-selection-test--kill-buffers chat source))))

  (it "starts at the first agent when the selected agent is stale"
    (let ((session (make-eca--session :chat-agents '("one" "two")))
          chat)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-chat--set-agent)
      (unwind-protect
          (progn
            (setq chat (eca-selection-test--make-chat session "A"))
            (with-current-buffer chat
              (setq-local eca-chat--selected-agent "removed"))
            (setf (eca--session-last-chat-buffer session) chat)
            (with-current-buffer chat
              (eca-chat-cycle-agent))
            (expect 'eca-chat--set-agent :to-have-been-called-with
                    session "one" chat))
        (eca-selection-test--kill-buffers chat))))

  (it "reports an empty agent list"
    (let ((session (make-eca--session :chat-agents nil))
          chat)
      (spy-on 'eca-session :and-return-value session)
      (unwind-protect
          (progn
            (setq chat (eca-selection-test--make-chat session "A"))
            (setf (eca--session-last-chat-buffer session) chat)
            (with-current-buffer chat
              (expect (eca-chat-cycle-agent) :to-throw 'user-error)))
        (eca-selection-test--kill-buffers chat)))))

(describe "eca-chat-delete active chat routing"
  (it "deletes the current registered chat instead of a stale last chat"
    (let ((session (make-eca--session)) a b)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-api-request-sync)
      (spy-on 'eca-chat--force-tab-line-update)
      (unwind-protect
          (progn
            (setq a (eca-selection-test--make-chat session "A")
                  b (eca-selection-test--make-chat session "B"))
            (setf (eca--session-last-chat-buffer session) b)
            (with-current-buffer a
              (eca-chat-delete))
            (expect (buffer-live-p a) :to-be nil)
            (expect (buffer-live-p b) :to-be-truthy)
            (expect (eca-get (eca--session-chats session) "A") :to-be nil)
            (expect 'eca-api-request-sync :to-have-been-called-with
                    session
                    :method "chat/delete"
                    :params '(:chatId "A")))
        (eca-selection-test--kill-buffers a b)))))

(provide 'eca-chat-selection-test)
;;; eca-chat-selection-test.el ends here
