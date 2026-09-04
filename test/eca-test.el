;;; eca-test.el --- Tests for eca -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'eca)

(describe "eca--handle-message"
  (it "answers an error response when a request handler signals"
    (let ((session (make-eca--session))
          (request (list :id 7 :method "editor/getReferences" :params '())))
      (spy-on 'eca-warn)
      (spy-on 'eca--log-error)
      (spy-on 'eca-editor-get-references
              :and-call-fake (lambda (&rest _) (error "boom")))
      (spy-on 'eca-api-send-request-response)
      (eca--handle-message session request)
      (expect 'eca-api-send-request-response
              :to-have-been-called-with
              session request (list :status "error" :message "boom"))
      (expect 'eca-warn :to-have-been-called)))

  (it "does not respond for async request handlers"
    (let ((session (make-eca--session))
          (request (list :id 8 :method "editor/getReferences" :params '())))
      (spy-on 'eca-editor-get-references :and-return-value :async)
      (spy-on 'eca-api-send-request-response)
      (eca--handle-message session request)
      (expect 'eca-api-send-request-response :not :to-have-been-called))))

(describe "eca-scratch-chat"
  :var (scratch-dir)

  (before-each
    ;; Under `temporary-file-directory' rather than a literal /tmp:
    ;; these tests also run on Windows CI.
    (setq scratch-dir (expand-file-name (make-temp-name "eca-scratch-test-")
                                        temporary-file-directory))
    (setq eca-scratch-directory scratch-dir)
    (setq eca--sessions '())
    (spy-on 'eca-process-start)
    (spy-on 'eca-chat-open))

  (after-each
    (setq eca--sessions '())
    (when (file-directory-p scratch-dir)
      (delete-directory scratch-dir t)))

  (it "creates the scratch directory when it does not exist"
    (expect (file-directory-p scratch-dir) :to-be nil)
    (eca-scratch-chat)
    (expect (file-directory-p scratch-dir) :to-be-truthy))

  (it "starts a session whose workspace is the scratch directory"
    (eca-scratch-chat)
    (expect (length (eca-vals eca--sessions)) :to-equal 1)
    (expect (eca--session-workspace-folders (car (eca-vals eca--sessions)))
            :to-equal (list (directory-file-name scratch-dir)))
    (expect 'eca-process-start :to-have-been-called))

  (it "reuses the scratch session instead of starting a second one"
    (eca-scratch-chat)
    (let ((session (car (eca-vals eca--sessions))))
      (setf (eca--session-status session) 'started)
      (eca-scratch-chat)
      (expect (length (eca-vals eca--sessions)) :to-equal 1)
      (expect 'eca-chat-open :to-have-been-called-with session)))

  ;; The point of the command: the chat must not land in the session of
  ;; whatever project the current buffer belongs to.
  (it "does not reuse the session of the current buffer's project"
    (let ((project (eca-create-session (list "/some/project"))))
      (setf (eca--session-status project) 'started)
      (setq-local eca--session-id-cache (eca--session-id project))
      (eca-scratch-chat)
      (expect (length (eca-vals eca--sessions)) :to-equal 2)
      (expect 'eca-chat-open :not :to-have-been-called-with project)))

  ;; The command must not disturb the buffer it was called from.
  (it "leaves the current buffer's session cache untouched"
    (let ((project (eca-create-session (list "/some/project"))))
      (setf (eca--session-status project) 'started)
      (setq-local eca--session-id-cache (eca--session-id project))
      (eca-scratch-chat)
      (expect eca--session-id-cache :to-equal (eca--session-id project))))

  (it "honours a custom eca-scratch-directory"
    (let ((custom (expand-file-name (make-temp-name "eca-scratch-custom-")
                                    temporary-file-directory)))
      (unwind-protect
          (progn
            (setq eca-scratch-directory custom)
            (eca-scratch-chat)
            (expect (eca--session-workspace-folders
                     (car (eca-vals eca--sessions)))
                    :to-equal (list (directory-file-name custom))))
        (when (file-directory-p custom)
          (delete-directory custom t))))))

(provide 'eca-test)
;;; eca-test.el ends here
