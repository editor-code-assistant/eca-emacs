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

(describe "Providers list request ordering"
  (it "keeps the newer list and visible tab after older success or error"
    (let ((session (make-eca--session :id "providers-order"))
          buffer requests)
      (spy-on 'eca-api-request-async :and-call-fake
              (lambda (_session &rest args) (push args requests)))
      (unwind-protect
          (progn
            (setq buffer (eca-settings--create-buffer "providers" session))
            (eca-providers--fetch-and-render session buffer)
            (eca-providers--fetch-and-render session buffer)
            (funcall (plist-get (car requests) :success-callback)
                     '(:providers [(:id "fresh" :name "Fresh"
                                   :configured t :modelCount 3)]))
            (eca-providers--handle-provider-updated
             session '(:id "fresh" :name "Fresh" :configured t :modelCount 4))
            (funcall (plist-get (cadr requests) :success-callback)
                     '(:providers [(:id "old" :name "Old"
                                   :configured t :modelCount 1)]))
            (expect (plist-get (car (eca--session-providers session)) :id)
                    :to-equal "fresh")
            (expect (plist-get (car (eca--session-providers session)) :modelCount)
                    :to-equal 4)
            (with-current-buffer buffer
              (expect (buffer-string) :to-match "Fresh")
              (expect (buffer-string) :not :to-match "Old"))
            (funcall (plist-get (cadr requests) :error-callback) "late error")
            (with-current-buffer buffer
              (expect (buffer-string) :to-match "Fresh")
              (expect (buffer-string) :not :to-match "Failed to load providers")))
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

  (it "keeps every warning in the log and ends with a visible summary"
    (let ((session (make-eca--session :id "refresh-warnings"))
          (message-log-max t)
          (original-info (symbol-function 'eca-info))
          (original-warn (symbol-function 'eca-warn))
          request visible)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-api-request-async :and-call-fake
              (lambda (_session &rest args) (setq request args)))
      (spy-on 'eca-info :and-call-fake
              (lambda (fmt &rest args)
                (push (apply #'format fmt args) visible)
                (apply original-info fmt args)))
      (spy-on 'eca-warn :and-call-fake
              (lambda (fmt &rest args)
                (push (apply #'format fmt args) visible)
                (apply original-warn fmt args)))
      (eca-chat-refresh-models)
      (funcall (plist-get request :success-callback)
               '(:modelCount 4
                 :warnings [(:provider "warning-alpha" :message "alpha-detail")
                            (:provider "warning-beta" :message "beta-detail")]))
      (expect (car visible) :to-match "2 warnings")
      (expect (car visible) :to-match "\\*Messages\\*")
      (with-current-buffer (get-buffer "*Messages*")
        (expect (buffer-string) :to-match "warning-alpha: alpha-detail")
        (expect (buffer-string) :to-match "warning-beta: beta-detail"))))

  (it "leaves the success message visible when there are no warnings"
    (let ((session (make-eca--session :id "refresh-no-warnings"))
          request messages)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-api-request-async :and-call-fake
              (lambda (_session &rest args) (setq request args)))
      (spy-on 'eca-info :and-call-fake
              (lambda (fmt &rest args)
                (push (apply #'format fmt args) messages)))
      (eca-chat-refresh-models)
      (funcall (plist-get request :success-callback)
               '(:modelCount 3 :warnings []))
      (expect (car messages) :to-equal "Model refresh complete: 3 models")))

  (it "leaves a single warning visible without a count summary"
    (let ((session (make-eca--session :id "refresh-one-warning"))
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
      (eca-chat-refresh-models)
      (funcall (plist-get request :success-callback)
               '(:modelCount 3
                 :warnings [(:provider "single" :message "one warning")]))
      (expect (car messages) :to-equal "single: one warning")))

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
  (it "omits a stale default model and variant and adopts the fallback"
    (let ((session (make-eca--session :models ["new"]
                                      :chat-default-model "old"
                                      :chat-default-variant "old-variant"))
          chat request)
      (spy-on 'eca-chat--extract-contexts-from-prompt :and-return-value nil)
      (spy-on 'eca-chat--set-prompt)
      (spy-on 'eca-chat--set-chat-loading)
      (spy-on 'eca-api-request-async :and-call-fake
              (lambda (_session &rest args) (setq request args)))
      (unwind-protect
          (progn
            (setq chat (generate-new-buffer " *eca-stale-default-variant*"))
            (with-current-buffer chat
              (setq major-mode 'eca-chat-mode)
              (setq-local eca-chat--id "A")
              (setq-local eca--chat-init-session session)
              (eca-chat--send-prompt session "hello"))
            (expect (plist-get (plist-get request :params) :model) :to-be nil)
            (expect (plist-member (plist-get request :params) :variant) :to-be nil)
            (funcall (plist-get request :success-callback) '(:model "new"))
            (expect (buffer-local-value 'eca-chat--selected-model chat)
                    :to-equal "new")
            (expect (buffer-local-value 'eca-chat--selected-variant chat) :to-be nil)
            (expect (eca--session-chat-default-variant session) :to-be nil))
        (when (buffer-live-p chat)
          (let ((kill-buffer-hook nil)) (kill-buffer chat))))))

  (it "keeps a variant changed while a stale-model prompt is pending"
    (let ((session (make-eca--session :models ["new"]
                                      :chat-default-model "old"
                                      :chat-default-variant "old-variant"))
          chat request)
      (spy-on 'eca-chat--extract-contexts-from-prompt :and-return-value nil)
      (spy-on 'eca-chat--set-prompt)
      (spy-on 'eca-chat--set-chat-loading)
      (spy-on 'eca-api-request-async :and-call-fake
              (lambda (_session &rest args) (setq request args)))
      (unwind-protect
          (progn
            (setq chat (generate-new-buffer " *eca-stale-variant-choice*"))
            (with-current-buffer chat
              (setq major-mode 'eca-chat-mode)
              (setq-local eca-chat--id "A")
              (setq-local eca--chat-init-session session)
              (setq-local eca-chat--selected-model "old")
              (setq-local eca-chat--selected-variant "old-variant")
              (eca-chat--send-prompt session "hello")
              (setq-local eca-chat--selected-variant "user-variant"))
            (setf (eca--session-chat-default-variant session) "user-variant")
            (expect (plist-member (plist-get request :params) :variant) :to-be nil)
            (funcall (plist-get request :success-callback) '(:model "new"))
            (expect (buffer-local-value 'eca-chat--selected-model chat)
                    :to-equal "new")
            (expect (buffer-local-value 'eca-chat--selected-variant chat)
                    :to-equal "user-variant")
            (expect (eca--session-chat-default-variant session)
                    :to-equal "user-variant"))
        (when (buffer-live-p chat)
          (let ((kill-buffer-hook nil)) (kill-buffer chat))))))

  (it "keeps the variant for a valid or explicit custom model"
    (let ((session (make-eca--session :models ["valid"]
                                      :chat-default-model "valid"
                                      :chat-default-variant "fast"))
          chat request)
      (spy-on 'eca-chat--extract-contexts-from-prompt :and-return-value nil)
      (spy-on 'eca-chat--set-prompt)
      (spy-on 'eca-chat--set-chat-loading)
      (spy-on 'eca-api-request-async :and-call-fake
              (lambda (_session &rest args) (setq request args)))
      (unwind-protect
          (progn
            (setq chat (generate-new-buffer " *eca-valid-model-variant*"))
            (with-current-buffer chat
              (setq major-mode 'eca-chat-mode)
              (setq-local eca-chat--id "A")
              (setq-local eca--chat-init-session session)
              (eca-chat--send-prompt session "hello"))
            (expect (plist-get (plist-get request :params) :variant)
                    :to-equal "fast")
            (with-current-buffer chat
              (let ((eca-chat-custom-model "custom"))
                (eca-chat--send-prompt session "again")))
            (expect (plist-get (plist-get request :params) :model)
                    :to-equal "custom")
            (expect (plist-get (plist-get request :params) :variant)
                    :to-equal "fast"))
        (when (buffer-live-p chat)
          (let ((kill-buffer-hook nil)) (kill-buffer chat))))))

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
                                      :chat-default-model "old"
                                      :chat-default-variant "old-variant"))
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
              (setq-local eca-chat--selected-model "old")
              (setq-local eca-chat--selected-variant "old-variant"))
            (setf (eca--session-chats session) (list (cons "A" chat)))
            (eca-config-updated session '(:chat (:models ["new"])))
            (expect (buffer-local-value 'eca-chat--selected-model chat)
                    :to-equal "old")
            (with-current-buffer chat
              (eca-chat--send-prompt session "hello"))
            (expect (plist-get request :method) :to-equal "chat/prompt")
            (expect (plist-get (plist-get request :params) :model) :to-be nil)
            (expect (plist-member (plist-get request :params) :variant) :to-be nil)
            (funcall (plist-get request :success-callback) '(:model "new"))
            (expect (buffer-local-value 'eca-chat--selected-model chat)
                    :to-equal "new")
            (expect (buffer-local-value 'eca-chat--selected-variant chat) :to-be nil)
            (expect (eca--session-chat-default-model session) :to-equal "new")
            (expect (eca--session-chat-default-variant session) :to-be nil)
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
