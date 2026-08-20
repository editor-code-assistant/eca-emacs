;;; eca-editor-test.el --- Tests for eca-editor -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'eca-editor)

;; Forward declaration so byte-compilation does not warn when flycheck
;; is not installed in the test environment.
(defvar flycheck-mode)

;; Declared special so let-bindings below stay dynamic even when
;; etags.el is not loaded in the test environment.
(defvar tags-file-name)
(defvar tags-table-list)

(describe "eca-editor--flycheck-active-in-locus-p"
  (it "returns nil for a nil locus"
    (expect (eca-editor--flycheck-active-in-locus-p nil)
            :not :to-be-truthy))

  (it "returns nil for a file-path (string) locus"
    (expect (eca-editor--flycheck-active-in-locus-p "/tmp/foo.el")
            :not :to-be-truthy))

  (it "returns nil for a killed buffer"
    (let ((buf (generate-new-buffer " *eca-editor-test-killed*")))
      (kill-buffer buf)
      (expect (eca-editor--flycheck-active-in-locus-p buf)
              :not :to-be-truthy)))

  (it "returns nil for a live buffer where flycheck-mode is unset"
    (with-temp-buffer
      (expect (eca-editor--flycheck-active-in-locus-p (current-buffer))
              :not :to-be-truthy)))

  (it "returns nil for a live buffer where flycheck-mode is nil"
    (with-temp-buffer
      (setq-local flycheck-mode nil)
      (expect (eca-editor--flycheck-active-in-locus-p (current-buffer))
              :not :to-be-truthy)))

  (it "returns non-nil for a live buffer where flycheck-mode is t"
    (with-temp-buffer
      (setq-local flycheck-mode t)
      (expect (eca-editor--flycheck-active-in-locus-p (current-buffer))
              :to-be-truthy))))

(defvar eca-editor-test--file nil)

(defun eca-editor-test--nav-params (&rest overrides)
  "Build editor/getDefinition-like params with OVERRIDES."
  (append overrides
          (list :uri (eca--path-to-uri eca-editor-test--file)
                :position (list :line 1 :character 7))))

(describe "eca-editor-get-definition"
  :var (session request)

  (before-each
    (setq session (make-eca--session))
    (setq request (list :id 42 :method "editor/getDefinition"))
    (setq eca-editor-test--file
          (make-temp-file "eca-editor-nav-test" nil ".clj" "(defn my-func [x]\n  x)\n"))
    (spy-on 'eca-api-send-request-response))

  (after-each
    (when-let ((buf (find-buffer-visiting eca-editor-test--file)))
      (kill-buffer buf))
    (delete-file eca-editor-test--file))

  (it "answers no-server when lsp-mode is not available and no xref backend exists"
    (spy-on 'eca-editor--lsp-nav-available-p :and-return-value nil)
    (spy-on 'eca-editor--xref-backend :and-return-value nil)
    (expect (eca-editor-get-definition session request (eca-editor-test--nav-params))
            :to-equal '(:status "no-server"
                        :message "No lsp-mode and no xref backend available for this file")))

  (it "answers error when the file does not exist"
    (spy-on 'eca-editor--lsp-nav-available-p :and-return-value t)
    (expect (eca-editor-get-definition
             session request
             (list :uri (eca--path-to-uri "/non/existent/file.clj")
                   :position (list :line 1 :character 1)))
            :to-equal (list :status "error"
                            :message (format "File not found: %s"
                                             (eca--uri-to-path
                                              (eca--path-to-uri "/non/existent/file.clj"))))))

  (it "starts lsp and answers starting when the root is known to lsp-mode"
    (spy-on 'eca-editor--lsp-nav-available-p :and-return-value t)
    (spy-on 'eca-editor--lsp-buffer-workspaces :and-return-value nil)
    (spy-on 'eca-editor--lsp-known-root-p :and-return-value t)
    (spy-on 'eca-editor--lsp-start :and-return-value t)
    (expect (eca-editor-get-definition session request (eca-editor-test--nav-params))
            :to-equal '(:status "starting"))
    (expect 'eca-editor--lsp-start :to-have-been-called))

  (it "answers no-server when no installed lsp client supports the file"
    (spy-on 'eca-editor--lsp-nav-available-p :and-return-value t)
    (spy-on 'eca-editor--lsp-buffer-workspaces :and-return-value nil)
    (spy-on 'eca-editor--lsp-known-root-p :and-return-value t)
    (spy-on 'eca-editor--lsp-start :and-return-value nil)
    (spy-on 'eca-editor--xref-backend :and-return-value nil)
    (expect (eca-editor-get-definition session request (eca-editor-test--nav-params))
            :to-equal '(:status "no-server"
                        :message "No installed lsp-mode client for this file's major mode and no xref backend")))

  (it "answers no-server when the root is not known to lsp-mode"
    (spy-on 'eca-editor--lsp-nav-available-p :and-return-value t)
    (spy-on 'eca-editor--lsp-buffer-workspaces :and-return-value nil)
    (spy-on 'eca-editor--lsp-known-root-p :and-return-value nil)
    (spy-on 'eca-editor--xref-backend :and-return-value nil)
    (expect (eca-editor-get-definition session request (eca-editor-test--nav-params))
            :to-equal '(:status "no-server"
                        :message "No lsp-mode workspace and no xref backend for this file; start lsp or eglot in its project once")))

  (it "answers starting when the lsp workspace has not finished initializing"
    (spy-on 'eca-editor--lsp-nav-available-p :and-return-value t)
    (spy-on 'eca-editor--lsp-buffer-workspaces :and-return-value '(workspace))
    (spy-on 'eca-editor--lsp-workspaces-ready-p :and-return-value nil)
    (spy-on 'eca-editor--lsp-request-async)
    (expect (eca-editor-get-definition session request (eca-editor-test--nav-params))
            :to-equal '(:status "starting"))
    (expect 'eca-editor--lsp-request-async :not :to-have-been-called))

  (it "answers error when lsp-mode signals synchronously"
    (spy-on 'eca-editor--lsp-nav-available-p :and-return-value t)
    (spy-on 'eca-editor--lsp-buffer-workspaces :and-return-value '(workspace))
    (spy-on 'eca-editor--lsp-request-async
            :and-call-fake
            (lambda (&rest _)
              (error "The connected server(s) does not support method textDocument/definition")))
    (expect (eca-editor-get-definition session request (eca-editor-test--nav-params))
            :to-equal '(:status "error"
                        :message "The connected server(s) does not support method textDocument/definition")))

  (it "fires an async lsp request with 0-based position and answers success with 1-based locations"
    (spy-on 'eca-editor--lsp-nav-available-p :and-return-value t)
    (spy-on 'eca-editor--lsp-buffer-workspaces :and-return-value '(workspace))
    (spy-on 'eca-editor--lsp-request-async)
    (expect (eca-editor-get-definition session request (eca-editor-test--nav-params))
            :to-be :async)
    (let* ((args (car (spy-calls-all-args 'eca-editor--lsp-request-async)))
           (method (nth 0 args))
           (lsp-params (nth 1 args))
           (callback (nth 2 args)))
      (expect method :to-equal "textDocument/definition")
      (expect (plist-get lsp-params :position) :to-equal '(:line 0 :character 6))
      (expect (plist-get (plist-get lsp-params :textDocument) :uri)
              :to-equal (eca--path-to-uri eca-editor-test--file))
      (funcall callback (vector (list :uri "file:///project/src/foo.clj"
                                      :range (list :start (list :line 9 :character 3)
                                                   :end (list :line 9 :character 8)))))
      (expect 'eca-api-send-request-response
              :to-have-been-called-with
              session request
              (list :status "success"
                    :locations (vector (list :uri "file:///project/src/foo.clj"
                                             :range (list :start (list :line 10 :character 4)
                                                          :end (list :line 10 :character 9))))))))

  (it "normalizes a single Location and LocationLink results"
    (expect (eca-editor--lsp-locations->eca
             (list :uri "file:///a.clj"
                   :range (list :start (list :line 0 :character 0)
                                :end (list :line 0 :character 2))))
            :to-equal (vector (list :uri "file:///a.clj"
                                    :range (list :start (list :line 1 :character 1)
                                                 :end (list :line 1 :character 3)))))
    (expect (eca-editor--lsp-locations->eca
             (vector (list :targetUri "file:///b.clj"
                           :targetSelectionRange (list :start (list :line 2 :character 1)
                                                       :end (list :line 2 :character 4)))))
            :to-equal (vector (list :uri "file:///b.clj"
                                    :range (list :start (list :line 3 :character 2)
                                                 :end (list :line 3 :character 5))))))

  (it "normalizes hash-table results with string keys"
    (let ((range (make-hash-table :test 'equal))
          (start (make-hash-table :test 'equal))
          (end (make-hash-table :test 'equal))
          (loc (make-hash-table :test 'equal)))
      (puthash "line" 4 start)
      (puthash "character" 0 start)
      (puthash "line" 4 end)
      (puthash "character" 3 end)
      (puthash "start" start range)
      (puthash "end" end range)
      (puthash "uri" "file:///c.clj" loc)
      (puthash "range" range loc)
      (expect (eca-editor--lsp-locations->eca (vector loc))
              :to-equal (vector (list :uri "file:///c.clj"
                                      :range (list :start (list :line 5 :character 1)
                                                   :end (list :line 5 :character 4)))))))

  (it "answers error with the lsp message when the request fails"
    (spy-on 'eca-editor--lsp-nav-available-p :and-return-value t)
    (spy-on 'eca-editor--lsp-buffer-workspaces :and-return-value '(workspace))
    (spy-on 'eca-editor--lsp-request-async)
    (eca-editor-get-definition session request (eca-editor-test--nav-params))
    (let ((error-callback (nth 3 (car (spy-calls-all-args 'eca-editor--lsp-request-async)))))
      (funcall error-callback (list :code -32603 :message "boom"))
      (expect 'eca-api-send-request-response
              :to-have-been-called-with
              session request
              (list :status "error" :message "boom")))))

(describe "eca-editor--lsp-workspaces-ready-p"
  (it "is not ready when no workspace has server capabilities yet"
    (cl-letf (((symbol-function 'lsp--workspace-server-capabilities)
               (lambda (_) nil)))
      (expect (eca-editor--lsp-workspaces-ready-p '(ws1 ws2))
              :not :to-be-truthy)))

  (it "is ready when some workspace has server capabilities"
    (cl-letf (((symbol-function 'lsp--workspace-server-capabilities)
               (lambda (ws) (when (eq ws 'ready-ws) (make-hash-table)))))
      (expect (eca-editor--lsp-workspaces-ready-p '(ws1 ready-ws))
              :to-be-truthy)))

  (it "fails open when workspaces cannot be inspected"
    ;; lsp-mode is absent in the test env so the accessor errors.
    (expect (eca-editor--lsp-workspaces-ready-p '(ws1))
            :to-be-truthy)))

(describe "eca-editor-get-references"
  :var (session request)

  (before-each
    (setq session (make-eca--session))
    (setq request (list :id 43 :method "editor/getReferences"))
    (setq eca-editor-test--file
          (make-temp-file "eca-editor-nav-test" nil ".clj" "(defn my-func [x]\n  x)\n"))
    (spy-on 'eca-api-send-request-response)
    (spy-on 'eca-editor--lsp-nav-available-p :and-return-value t)
    (spy-on 'eca-editor--lsp-buffer-workspaces :and-return-value '(workspace))
    (spy-on 'eca-editor--lsp-request-async))

  (after-each
    (when-let ((buf (find-buffer-visiting eca-editor-test--file)))
      (kill-buffer buf))
    (delete-file eca-editor-test--file))

  (it "requests references including declarations by default"
    (expect (eca-editor-get-references session request (eca-editor-test--nav-params))
            :to-be :async)
    (let* ((args (car (spy-calls-all-args 'eca-editor--lsp-request-async)))
           (method (nth 0 args))
           (lsp-params (nth 1 args)))
      (expect method :to-equal "textDocument/references")
      (expect (plist-get lsp-params :context) :to-equal '(:includeDeclaration t))))

  (it "propagates includeDeclaration false"
    ;; JSON false is parsed as nil by eca-api, distinguished via plist-member.
    (eca-editor-get-references session request
                               (eca-editor-test--nav-params :includeDeclaration nil))
    (let ((lsp-params (nth 1 (car (spy-calls-all-args 'eca-editor--lsp-request-async)))))
      (expect (plist-get lsp-params :context)
              :to-equal '(:includeDeclaration :json-false)))))

;; A fake xref backend answering fixed locations inside the temp file:
;; the declaration at line 1 column 6 and a usage at line 2 column 2.
(cl-defmethod xref-backend-identifier-at-point ((_backend (eql eca-editor-test--xref)))
  "my-func")

(cl-defmethod xref-backend-definitions ((_backend (eql eca-editor-test--xref)) _identifier)
  (list (xref-make "(defn my-func [x]"
                   (xref-make-file-location eca-editor-test--file 1 6))))

(cl-defmethod xref-backend-references ((_backend (eql eca-editor-test--xref)) _identifier)
  (list (xref-make "(defn my-func [x]"
                   (xref-make-file-location eca-editor-test--file 1 6))
        (xref-make "  x)"
                   (xref-make-file-location eca-editor-test--file 2 2))))

(describe "eca-editor xref fallback"
  :var (session request)

  (before-each
    (setq session (make-eca--session))
    (setq request (list :id 44 :method "editor/getDefinition"))
    (setq eca-editor-test--file
          (make-temp-file "eca-editor-nav-test" nil ".clj" "(defn my-func [x]\n  x)\n"))
    (spy-on 'eca-api-send-request-response)
    (spy-on 'eca-editor--lsp-nav-available-p :and-return-value nil)
    ;; Run the scheduled lookup synchronously: pumping timers with
    ;; sit-for would also fire leftover timers from other test files.
    (spy-on 'run-at-time :and-call-fake
            (lambda (_time _repeat fn &rest args) (apply fn args)))
    (with-current-buffer (find-file-noselect eca-editor-test--file)
      (setq-local xref-backend-functions (list (lambda () 'eca-editor-test--xref)))))

  (after-each
    (when-let ((buf (find-buffer-visiting eca-editor-test--file)))
      (kill-buffer buf))
    (delete-file eca-editor-test--file))

  (it "answers definitions via the buffer's xref backend"
    (expect (eca-editor-get-definition session request (eca-editor-test--nav-params))
            :to-be :async)
    (expect 'eca-api-send-request-response
            :to-have-been-called-with
            session request
            (list :status "success"
                  :locations (vector
                              (list :uri (eca--path-to-uri eca-editor-test--file)
                                    :range (list :start (list :line 1 :character 7)
                                                 :end (list :line 1 :character 7)))))))

  (it "answers references including the declaration by default"
    (expect (eca-editor-get-references session request (eca-editor-test--nav-params))
            :to-be :async)
    (expect 'eca-api-send-request-response
            :to-have-been-called-with
            session request
            (list :status "success"
                  :locations (vector
                              (list :uri (eca--path-to-uri eca-editor-test--file)
                                    :range (list :start (list :line 1 :character 7)
                                                 :end (list :line 1 :character 7)))
                              (list :uri (eca--path-to-uri eca-editor-test--file)
                                    :range (list :start (list :line 2 :character 3)
                                                 :end (list :line 2 :character 3)))))))

  (it "filters the declaration from references when includeDeclaration is false"
    (expect (eca-editor-get-references
             session request
             (eca-editor-test--nav-params :includeDeclaration nil))
            :to-be :async)
    (expect 'eca-api-send-request-response
            :to-have-been-called-with
            session request
            (list :status "success"
                  :locations (vector
                              (list :uri (eca--path-to-uri eca-editor-test--file)
                                    :range (list :start (list :line 2 :character 3)
                                                 :end (list :line 2 :character 3)))))))

  (it "does not use the default etags backend without a loaded tags table"
    (with-current-buffer (find-buffer-visiting eca-editor-test--file)
      (setq-local xref-backend-functions (list (lambda () 'etags))))
    (let ((tags-file-name nil)
          (tags-table-list nil))
      (expect (eca-editor-get-definition session request (eca-editor-test--nav-params))
              :to-equal '(:status "no-server"
                          :message "No lsp-mode and no xref backend available for this file"))))

  (it "uses the etags backend when a tags table is loaded"
    (with-current-buffer (find-buffer-visiting eca-editor-test--file)
      (setq-local xref-backend-functions (list (lambda () 'etags)))
      (let ((tags-table-list '("/tmp/TAGS")))
        (expect (eca-editor--xref-backend) :to-be 'etags)))))

(provide 'eca-editor-test)
;;; eca-editor-test.el ends here
