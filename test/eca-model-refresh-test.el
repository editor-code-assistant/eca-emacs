;;; eca-model-refresh-test.el --- Model refresh UI tests -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'eca)
(require 'eca-chat)
(require 'eca-providers)
(require 'transient)

(describe "model refresh entry points"
  (it "offers an interactive command in the chat menu and Providers tab"
    (expect (commandp 'eca-chat-refresh-models) :to-be-truthy)
    (expect (transient-get-suffix 'eca--transient-menu-prefix "F")
            :to-be-truthy)
    (let ((session (make-eca--session :id "refresh-ui"))
          buffer)
      (unwind-protect
          (progn
            (setq buffer (eca-settings--create-buffer "providers" session))
            (with-current-buffer buffer
              (eca-providers--render session buffer)
              (goto-char (point-min))
              (expect (search-forward "Refresh models" nil t) :to-be-truthy)
              (expect (get-text-property (1- (point)) 'eca-button-on-action)
                      :to-be-truthy)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(describe "Providers refresh action"
  (it "runs the shared command and renders newly fetched providers"
    (let ((session (make-eca--session :id "refresh-providers"))
          buffer requests)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-api-request-async :and-call-fake
              (lambda (_session &rest args) (push args requests)))
      (unwind-protect
          (progn
            (setq buffer (eca-settings--create-buffer "providers" session))
            (with-current-buffer buffer
              (eca-providers--render session buffer)
              (goto-char (point-min))
              (search-forward "Refresh models")
              (funcall (get-text-property (1- (point)) 'eca-button-on-action)))
            (expect (plist-get (car requests) :method) :to-equal "models/refresh")
            (funcall (plist-get (car requests) :success-callback)
                     '(:modelCount 3 :warnings []))
            (expect (plist-get (car requests) :method) :to-equal "providers/list")
            (funcall (plist-get (car requests) :success-callback)
                     '(:providers [(:id "refreshed" :name "Refreshed"
                                   :configured t :modelCount 3)]))
            (expect (plist-get (car (eca--session-providers session)) :id)
                    :to-equal "refreshed")
            (with-current-buffer buffer
              (expect (buffer-string) :to-match "Refreshed")))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(describe "eca-chat-refresh-models"
  (it "requires a session before sending a request"
    (spy-on 'eca-session :and-return-value nil)
    (spy-on 'eca-api-request-async)
    (expect (eca-chat-refresh-models) :to-throw 'user-error)
    (expect 'eca-api-request-async :not :to-have-been-called))

  (it "sends an empty models/refresh request and reports the result"
    (let ((session (make-eca--session :id "refresh-request"))
          request messages)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-api-request-async :and-call-fake
              (lambda (_session &rest args) (setq request args)))
      (spy-on 'eca-info :and-call-fake
              (lambda (fmt &rest args)
                (push (apply #'format fmt args) messages)))
      (spy-on 'eca-warn :and-call-fake
              (lambda (fmt &rest args)
                (push (apply #'format fmt args) messages)))
      (spy-on 'eca-providers--fetch-and-render)
      (eca-chat-refresh-models)
      (expect (plist-get request :method) :to-equal "models/refresh")
      (expect (plist-get request :params) :to-equal nil)
      (expect messages :to-contain "Refreshing models...")
      (funcall (plist-get request :success-callback)
               '(:modelCount 4 :warnings [(:provider "openai" :message "Cached models")
                                          (:provider "other" :message "Stale models")]))
      (expect messages :to-contain "Model refresh complete: 4 models")
      (expect messages :to-contain "openai: Cached models")
      (expect messages :to-contain "other: Stale models")
      (expect 'eca-providers--fetch-and-render :not :to-have-been-called)))

  (it "reports errors without fetching providers or changing models"
    (let ((session (make-eca--session :id "refresh-error" :models '("old")))
          request warning)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-api-request-async :and-call-fake
              (lambda (_session &rest args) (setq request args)))
      (spy-on 'eca-warn :and-call-fake
              (lambda (fmt &rest args)
                (setq warning (apply #'format fmt args))))
      (spy-on 'eca-providers--fetch-and-render)
      (eca-chat-refresh-models)
      (funcall (plist-get request :error-callback) "discovery failed")
      (expect warning :to-match "discovery failed")
      (expect (eca--session-models session) :to-equal '("old"))
      (expect 'eca-providers--fetch-and-render :not :to-have-been-called)))

  (it "refetches provider counts on success when the tab exists"
    (let ((session (make-eca--session :id "refresh-tab"))
          request buffer)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-api-request-async :and-call-fake
              (lambda (_session &rest args) (setq request args)))
      (spy-on 'eca-providers--fetch-and-render)
      (unwind-protect
          (progn
            (setq buffer (eca-settings--create-buffer "providers" session))
            (eca-chat-refresh-models)
            (funcall (plist-get request :success-callback)
                     '(:modelCount 2 :warnings []))
            (expect 'eca-providers--fetch-and-render
                    :to-have-been-called-with session buffer))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(describe "model catalog notification"
  (it "replaces picker candidates but preserves the selected chat model"
    (let ((session (make-eca--session :models ["old"]
                                      :chat-default-model "old"))
          chat)
      (unwind-protect
          (progn
            (setq chat (generate-new-buffer " *eca-model-refresh-chat*"))
            (with-current-buffer chat
              (setq major-mode 'eca-chat-mode)
              (setq-local eca-chat--id "A")
              (setq-local eca-chat--selected-model "old"))
            (setf (eca--session-chats session) (list (cons "A" chat)))
            (eca-config-updated session '(:chat (:models ["new"])))
            (expect (eca--session-models session) :to-equal ["new"])
            (expect (buffer-local-value 'eca-chat--selected-model chat)
                    :to-equal "old")
            (expect (eca--session-chat-default-model session) :to-equal "old"))
        (when (buffer-live-p chat)
          (let ((kill-buffer-hook nil)) (kill-buffer chat)))))))

(describe "prompt after model refresh"
  (it "adopts the server model when a new chat inherits a stale default"
    (let ((session (make-eca--session :models ["new"]
                                      :chat-default-model "old"))
          chat request)
      (spy-on 'eca-chat--extract-contexts-from-prompt :and-return-value nil)
      (spy-on 'eca-chat--set-prompt)
      (spy-on 'eca-chat--set-chat-loading)
      (spy-on 'eca-api-request-async :and-call-fake
              (lambda (_session &rest args) (setq request args)))
      (unwind-protect
          (progn
            (setq chat (generate-new-buffer " *eca-model-refresh-new-chat*"))
            (with-current-buffer chat
              (setq major-mode 'eca-chat-mode)
              (setq-local eca-chat--id "A")
              (setq-local eca--chat-init-session session)
              (expect (local-variable-p 'eca-chat--selected-model) :to-be nil)
              (expect (eca-chat--model) :to-equal "old")
              (eca-chat--send-prompt session "hello"))
            (expect (plist-get request :method) :to-equal "chat/prompt")
            (expect (plist-get (plist-get request :params) :model) :to-be nil)
            (funcall (plist-get request :success-callback) '(:model "new"))
            (expect (buffer-local-value 'eca-chat--selected-model chat)
                    :to-equal "new")
            (expect (eca--session-chat-default-model session) :to-equal "new"))
        (when (buffer-live-p chat)
          (let ((kill-buffer-hook nil)) (kill-buffer chat))))))

  (it "preserves a new chat model choice made during a stale request"
    (let ((session (make-eca--session :models ["new" "other"]
                                      :chat-default-model "old"))
          chat request)
      (spy-on 'eca-chat--extract-contexts-from-prompt :and-return-value nil)
      (spy-on 'eca-chat--set-prompt)
      (spy-on 'eca-chat--set-chat-loading)
      (spy-on 'eca-api-request-async :and-call-fake
              (lambda (_session &rest args) (setq request args)))
      (unwind-protect
          (progn
            (setq chat (generate-new-buffer " *eca-model-refresh-new-choice*"))
            (with-current-buffer chat
              (setq major-mode 'eca-chat-mode)
              (setq-local eca-chat--id "A")
              (setq-local eca--chat-init-session session)
              (expect (local-variable-p 'eca-chat--selected-model) :to-be nil)
              (eca-chat--send-prompt session "hello")
              (setq-local eca-chat--selected-model "other"))
            (setf (eca--session-chat-default-model session) "other")
            (funcall (plist-get request :success-callback) '(:model "new"))
            (expect (buffer-local-value 'eca-chat--selected-model chat)
                    :to-equal "other")
            (expect (eca--session-chat-default-model session) :to-equal "other"))
        (when (buffer-live-p chat)
          (let ((kill-buffer-hook nil)) (kill-buffer chat))))))

  (it "defers a missing selected model to the server and adopts its reply"
    (let ((session (make-eca--session :models ["old"]
                                      :chat-default-model "old"))
          chat request)
      (spy-on 'eca-chat--extract-contexts-from-prompt :and-return-value nil)
      (spy-on 'eca-chat--set-prompt)
      (spy-on 'eca-chat--set-chat-loading)
      (spy-on 'eca-api-request-async :and-call-fake
              (lambda (_session &rest args) (setq request args)))
      (unwind-protect
          (progn
            (setq chat (generate-new-buffer " *eca-model-refresh-prompt*"))
            (with-current-buffer chat
              (setq major-mode 'eca-chat-mode)
              (setq-local eca-chat--id "A")
              (setq-local eca-chat--selected-model "old"))
            (setf (eca--session-chats session) (list (cons "A" chat)))
            (eca-config-updated session '(:chat (:models ["new"])))
            (expect (buffer-local-value 'eca-chat--selected-model chat)
                    :to-equal "old")
            (with-current-buffer chat
              (eca-chat--send-prompt session "hello"))
            (expect (plist-get request :method) :to-equal "chat/prompt")
            (expect (plist-get (plist-get request :params) :model) :to-be nil)
            (funcall (plist-get request :success-callback) '(:model "new"))
            (expect (buffer-local-value 'eca-chat--selected-model chat)
                    :to-equal "new")
            (expect (eca--session-chat-default-model session) :to-equal "new")
            (with-current-buffer chat
              (eca-chat--send-prompt session "again"))
            (expect (plist-get (plist-get request :params) :model)
                    :to-equal "new")
            (expect (plist-get request :success-callback) :to-be #'ignore))
        (when (buffer-live-p chat)
          (let ((kill-buffer-hook nil)) (kill-buffer chat))))))

  (it "keeps an explicit custom model even when absent from the catalog"
    (let ((session (make-eca--session :models ["new"]
                                      :chat-default-model "old"))
          (eca-chat-custom-model "custom")
          chat request)
      (spy-on 'eca-chat--extract-contexts-from-prompt :and-return-value nil)
      (spy-on 'eca-chat--set-prompt)
      (spy-on 'eca-chat--set-chat-loading)
      (spy-on 'eca-api-request-async :and-call-fake
              (lambda (_session &rest args) (setq request args)))
      (unwind-protect
          (progn
            (setq chat (generate-new-buffer " *eca-model-refresh-custom*"))
            (with-current-buffer chat
              (setq major-mode 'eca-chat-mode)
              (setq-local eca-chat--id "A")
              (setq-local eca-chat--selected-model "old")
              (eca-chat--send-prompt session "hello"))
            (expect (plist-get (plist-get request :params) :model)
                    :to-equal "custom")
            (expect (plist-get request :success-callback) :to-be #'ignore)
            (funcall (plist-get request :success-callback) '(:model "new"))
            (expect (buffer-local-value 'eca-chat--selected-model chat)
                    :to-equal "old")
            (expect (eca--session-chat-default-model session) :to-equal "old"))
        (when (buffer-live-p chat)
          (let ((kill-buffer-hook nil)) (kill-buffer chat))))))

  (it "does not overwrite a newer choice when a stale prompt reply arrives"
    (let ((session (make-eca--session :models ["new" "other"]
                                      :chat-default-model "old"))
          chat request)
      (spy-on 'eca-chat--extract-contexts-from-prompt :and-return-value nil)
      (spy-on 'eca-chat--set-prompt)
      (spy-on 'eca-chat--set-chat-loading)
      (spy-on 'eca-api-request-async :and-call-fake
              (lambda (_session &rest args) (setq request args)))
      (unwind-protect
          (progn
            (setq chat (generate-new-buffer " *eca-model-refresh-late*"))
            (with-current-buffer chat
              (setq major-mode 'eca-chat-mode)
              (setq-local eca-chat--id "A")
              (setq-local eca-chat--selected-model "old")
              (eca-chat--send-prompt session "hello")
              (setq-local eca-chat--selected-model "other"))
            (setf (eca--session-chat-default-model session) "other")
            (funcall (plist-get request :success-callback) '(:model "new"))
            (expect (buffer-local-value 'eca-chat--selected-model chat)
                    :to-equal "other")
            (expect (eca--session-chat-default-model session) :to-equal "other"))
        (when (buffer-live-p chat)
          (let ((kill-buffer-hook nil)) (kill-buffer chat)))))))

;;; eca-model-refresh-test.el ends here
