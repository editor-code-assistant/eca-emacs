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

(provide 'eca-test)
;;; eca-test.el ends here
