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
    (let ((session
           (make-eca--session
            :chat-default-model "default-model"
            :chat-default-agent "default-agent"
            :chat-default-variant "default-variant"
            :chat-default-trust nil))
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
                    :to-be nil)
            (expect (eca--session-chat-default-model session)
                    :to-equal "default-model")
            (expect (eca--session-chat-default-agent session)
                    :to-equal "default-agent")
            (expect (eca--session-chat-default-variant session)
                    :to-equal "default-variant")
            (expect (eca--session-chat-default-trust session)
                    :to-be nil))
        (eca-selection-test--kill-buffers a b))))

  (it "keeps legacy unscoped updates session-wide"
    (let ((session (make-eca--session))
          a b)
      (unwind-protect
          (progn
            (setq a (eca-selection-test--make-chat session "A")
                  b (eca-selection-test--make-chat session "B"))
            (eca-config-updated
             session
             '(:chat (:selectModel "model-default"
                      :selectAgent "agent-default"
                      :selectVariant "variant-default"
                      :selectTrust t)))
            (dolist (buffer (list a b))
              (expect (buffer-local-value 'eca-chat--selected-model buffer)
                      :to-equal "model-default")
              (expect (buffer-local-value 'eca-chat--selected-agent buffer)
                      :to-equal "agent-default")
              (expect (buffer-local-value 'eca-chat--selected-variant buffer)
                      :to-equal "variant-default")
              (expect (buffer-local-value 'eca-chat--selected-trust buffer)
                      :to-be-truthy))
            (expect (eca--session-chat-default-model session)
                    :to-equal "model-default")
            (expect (eca--session-chat-default-agent session)
                    :to-equal "agent-default")
            (expect (eca--session-chat-default-variant session)
                    :to-equal "variant-default")
            (expect (eca--session-chat-default-trust session)
                    :to-be-truthy))
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
    (let ((session (make-eca--session
                    :chat-variants '("high")
                    :chat-default-variant "old"))
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
            (expect (eca--session-chat-default-variant session)
                    :to-be nil))
        (eca-selection-test--kill-buffers chat))))

  (it "omits a legacy no-variant sentinel from model notifications"
    (let ((session (make-eca--session :models '("new-model")))
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
            (expect (eca--session-chat-default-model session)
                    :to-equal "new-model")
            (expect 'eca-api-notify :to-have-been-called-with
                    session
                    :method "chat/selectedModelChanged"
                    :params '(:model "new-model" :chatId "A")))
        (eca-selection-test--kill-buffers chat)))))

(describe "session-scoped selection defaults"
  (it "keeps user selections within their session for new chats"
    (let ((session-a
           (make-eca--session
            :models '("a-model-new")
            :chat-agents '("a-agent-new")
            :chat-variants '("a-variant-new")
            :chat-default-model "a-model-old"
            :chat-default-agent "a-agent-old"
            :chat-default-variant "a-variant-old"
            :chat-default-trust nil))
          (session-b
           (make-eca--session
            :chat-default-model "b-model"
            :chat-default-agent "b-agent"
            :chat-default-variant "b-variant"
            :chat-default-trust nil))
          chat-a new-a new-b)
      (spy-on 'eca-session :and-return-value session-a)
      (spy-on 'completing-read :and-call-fake
              (lambda (prompt &rest _)
                (pcase prompt
                  ("Select a model:" "a-model-new")
                  ("Select a variant:" "a-variant-new")
                  ("Select an agent:" "a-agent-new"))))
      (spy-on 'eca-api-notify)
      (spy-on 'eca-api-request-sync)
      (unwind-protect
          (progn
            (setq chat-a (eca-selection-test--make-chat session-a "A"))
            (with-current-buffer chat-a
              (setq-local eca-chat--selected-model "a-model-old")
              (setq-local eca-chat--selected-agent "a-agent-old")
              (setq-local eca-chat--selected-variant "a-variant-old")
              (setq-local eca-chat--available-variants
                          '("a-variant-new"))
              (setq-local eca-chat--selected-trust nil))
            (setf (eca--session-last-chat-buffer session-a) chat-a)
            (with-current-buffer chat-a
              (eca-chat-select-model)
              (eca-chat-select-variant)
              (eca-chat-select-agent)
              (eca-chat-toggle-trust))
            (expect (eca--session-chat-default-model session-a)
                    :to-equal "a-model-new")
            (expect (eca--session-chat-default-agent session-a)
                    :to-equal "a-agent-new")
            (expect (eca--session-chat-default-variant session-a)
                    :to-equal "a-variant-new")
            (expect (eca--session-chat-default-trust session-a)
                    :to-be-truthy)
            (setq new-a (generate-new-buffer " *eca-selection-new-a*")
                  new-b (generate-new-buffer " *eca-selection-new-b*"))
            (with-current-buffer new-a
              (eca-chat--initialize-selection-state session-a))
            (with-current-buffer new-b
              (eca-chat--initialize-selection-state session-b))
            (expect (buffer-local-value 'eca-chat--selected-model new-a)
                    :to-equal "a-model-new")
            (expect (buffer-local-value 'eca-chat--selected-agent new-a)
                    :to-equal "a-agent-new")
            (expect (buffer-local-value 'eca-chat--selected-variant new-a)
                    :to-equal "a-variant-new")
            (expect (buffer-local-value 'eca-chat--selected-trust new-a)
                    :to-be-truthy)
            (expect (buffer-local-value 'eca-chat--selected-model new-b)
                    :to-equal "b-model")
            (expect (buffer-local-value 'eca-chat--selected-agent new-b)
                    :to-equal "b-agent")
            (expect (buffer-local-value 'eca-chat--selected-variant new-b)
                    :to-equal "b-variant")
            (expect (buffer-local-value 'eca-chat--selected-trust new-b)
                    :to-be nil))
        (eca-selection-test--kill-buffers chat-a new-a new-b))))

  (it "preserves explicit local clears over session defaults"
    (let ((session
           (make-eca--session
            :chat-default-model "model"
            :chat-default-agent "agent"
            :chat-default-variant "variant"
            :chat-default-trust t)))
      (let ((eca--chat-init-session session))
        (with-temp-buffer
          (expect (eca-chat--model) :to-equal "model")
          (expect (eca-chat--agent) :to-equal "agent")
          (expect (eca-chat--variant) :to-equal "variant")
          (expect (eca-chat--trust) :to-be-truthy)
          (setq-local eca-chat--selected-variant nil)
          (setq-local eca-chat--selected-trust nil)
          (expect (eca-chat--variant) :to-be nil)
          (expect (eca-chat--trust) :to-be nil)))))

  (it "seeds a created session from the trust user option"
    (let ((eca-chat-trust-enable t)
          (eca--sessions nil)
          (eca--session-ids 0))
      (let ((session (eca-create-session '("/tmp/project"))))
        (expect (eca--session-chat-default-trust session)
                :to-be-truthy)))))

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
    (let ((session (make-eca--session :chat-agents '("old" "new")))
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
            (expect (eca--session-chat-default-agent session)
                    :to-equal "new")
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
