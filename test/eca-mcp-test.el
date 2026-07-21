;;; eca-mcp-test.el --- Tests for eca-mcp -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'buttercup)
(require 'eca-mcp)

(defun eca-mcp-test--make-session (&rest servers)
  "Create a session containing SERVERS."
  (let ((session (make-eca--session)))
    (setf (eca--session-tool-servers session)
          (mapcar (lambda (server)
                    (cons (plist-get server :name) server))
                  servers))
    session))

(defun eca-mcp-test--plain (string)
  "Return STRING without text properties."
  (substring-no-properties string))

(describe "eca-mcp--server-runtime-action"
  (it "maps server states to immediate runtime actions"
    (expect (eca-mcp--server-runtime-action '(:status "running"))
            :to-equal 'stop)
    (expect (eca-mcp--server-runtime-action '(:status "starting"))
            :to-equal 'stop)
    (expect (eca-mcp--server-runtime-action '(:status "stopped"))
            :to-equal 'start)
    (expect (eca-mcp--server-runtime-action '(:status "failed"))
            :to-equal 'start)
    (expect (eca-mcp--server-runtime-action '(:status "disabled"))
            :to-equal 'start)
    (expect (eca-mcp--server-runtime-action '(:status "requires-auth"))
            :to-equal 'connect)
    (expect (eca-mcp--server-runtime-action '(:status "stopping"))
            :to-be nil)
    (expect (eca-mcp--server-runtime-action '(:status "unknown"))
            :to-be nil)))

(describe "eca-mcp--server-state-label"
  (it "uses human-readable operational states"
    (expect (eca-mcp--server-state-label '(:status "running"))
            :to-equal "Running")
    (expect (eca-mcp--server-state-label '(:status "starting"))
            :to-equal "Starting")
    (expect (eca-mcp--server-state-label '(:status "stopping"))
            :to-equal "Stopping")
    (expect (eca-mcp--server-state-label '(:status "stopped"))
            :to-equal "Stopped")
    (expect (eca-mcp--server-state-label '(:status "disabled"))
            :to-equal "Stopped")
    (expect (eca-mcp--server-state-label '(:status "failed"))
            :to-equal "Failed")
    (expect (eca-mcp--server-state-label '(:status "requires-auth"))
            :to-equal "Needs auth")
    (expect (eca-mcp--server-state-label '(:status "unknown"))
            :to-equal "Unknown")))

(describe "eca-mcp--server-action-label"
  (it "uses concise action labels"
    (expect (eca-mcp--server-action-label '(:status "running"))
            :to-equal "Stop")
    (expect (eca-mcp--server-action-label '(:status "stopped"))
            :to-equal "Start")
    (expect (eca-mcp--server-action-label '(:status "failed"))
            :to-equal "Retry")
    (expect (eca-mcp--server-action-label '(:status "requires-auth"))
            :to-equal "Connect")
    (expect (eca-mcp--server-action-label '(:status "stopping"))
            :to-equal "Wait")
    (expect (eca-mcp--server-action-label '(:status "unknown"))
            :to-equal "Unavailable")))

(describe "eca-mcp--status-face"
  (it "matches the settings status color semantics"
    (expect (eca-mcp--status-face "running") :to-equal 'success)
    (expect (eca-mcp--status-face "starting") :to-equal 'warning)
    (expect (eca-mcp--status-face "failed") :to-equal 'error)
    (expect (eca-mcp--status-face "stopped") :to-equal 'shadow)
    (expect (eca-mcp--status-face "stopping") :to-equal 'shadow)
    (expect (eca-mcp--status-face "disabled") :to-equal 'shadow)
    (expect (eca-mcp--status-face "requires-auth")
            :to-equal 'eca-mcp-details-requires-auth-face)))

(describe "eca-mcp--server-candidates"
  (it "aligns names and states using display widths"
    (let* ((servers '((:name "界" :status "running")
                      (:name "abc" :status "requires-auth")))
           (candidates (eca-mcp--server-candidates servers))
           (running (eca-mcp-test--plain (caar candidates)))
           (auth (eca-mcp-test--plain (caadr candidates))))
      (expect (string-width
               (substring running 0 (string-match "Running" running)))
              :to-equal 5)
      (expect (string-width
               (substring auth 0 (string-match "Needs auth" auth)))
              :to-equal 5)
      (expect (string-width
               (substring running 0 (string-match "Stop" running)))
              :to-equal 17)
      (expect (string-width
               (substring auth 0 (string-match "Connect" auth)))
              :to-equal 17)))

  (it "colors only the state and omits disabled configuration"
    (let* ((server '(:name "database" :status "disabled" :disabled t))
           (candidate (eca-mcp--server-candidate server 8 7))
           (state-position (string-match "Stopped" candidate))
           (action-position (string-match "Start" candidate)))
      (expect (eca-mcp-test--plain candidate)
              :to-equal "database  Stopped  Start")
      (expect (get-text-property 0 'face candidate) :to-be nil)
      (expect (get-text-property 8 'face candidate) :to-be nil)
      (expect (get-text-property state-position 'face candidate)
              :to-equal 'shadow)
      (expect (get-text-property (+ state-position 7) 'face candidate)
              :to-be nil)
      (expect (get-text-property action-position 'face candidate)
              :to-be nil))))

(describe "eca-mcp-servers"
  (it "excludes the built-in ECA server"
    (let* ((eca '(:name "ECA" :status "running"))
           (github '(:name "github" :status "stopped"))
           (session (eca-mcp-test--make-session eca github)))
      (expect (eca-mcp-servers session) :to-equal (list github)))))

(describe "eca-mcp--notify-server-action"
  (it "sends the existing start, stop, and connect notifications"
    (let ((session (make-eca--session))
          (server '(:name "github")))
      (spy-on 'eca-api-notify)
      (eca-mcp--notify-server-action session server 'start)
      (eca-mcp--notify-server-action session server 'stop)
      (eca-mcp--notify-server-action session server 'connect)
      (expect 'eca-api-notify :to-have-been-called-with
              session
              :method "mcp/startServer"
              :params '(:name "github"))
      (expect 'eca-api-notify :to-have-been-called-with
              session
              :method "mcp/stopServer"
              :params '(:name "github"))
      (expect 'eca-api-notify :to-have-been-called-with
              session
              :method "mcp/connectServer"
              :params '(:name "github")))))

(describe "eca-mcp-toggle-server"
  (it "sorts filterable candidates and performs the selected action"
    (let* ((eca '(:name "ECA" :status "running"))
           (zeta '(:name "zeta" :status "running"))
           (alpha '(:name "alpha" :status "stopped"))
           (session (eca-mcp-test--make-session eca zeta alpha))
           seen-candidates
           seen-predicate
           seen-require-match)
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-api-notify)
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt collection predicate require-match &rest _)
                (setq seen-candidates
                      (mapcar (lambda (candidate)
                                (eca-mcp-test--plain (car candidate)))
                              collection)
                      seen-predicate predicate
                      seen-require-match require-match)
                (substring-no-properties (caar collection))))
      (eca-mcp-toggle-server)
      (expect seen-candidates
              :to-equal '("alpha  Stopped  Start"
                          "zeta   Running  Stop"))
      (expect seen-predicate :to-be nil)
      (expect seen-require-match :to-be-truthy)
      (expect 'eca-api-notify :to-have-been-called-with
              session
              :method "mcp/startServer"
              :params '(:name "alpha"))))

  (it "uses the latest server state after completion"
    (let* ((server '(:name "alpha" :status "stopped"))
           (session (eca-mcp-test--make-session server)))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-api-notify)
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt collection &rest _)
                (setf (eca--session-tool-servers session)
                      (eca-assoc (eca--session-tool-servers session)
                                 "alpha"
                                 '(:name "alpha" :status "running")))
                (caar collection)))
      (eca-mcp-toggle-server)
      (expect 'eca-api-notify :to-have-been-called-with
              session
              :method "mcp/stopServer"
              :params '(:name "alpha"))))

  (it "reports when the selected server was removed"
    (let* ((server '(:name "alpha" :status "stopped"))
           (session (eca-mcp-test--make-session server)))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-api-notify)
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt collection &rest _)
                (setf (eca--session-tool-servers session)
                      (eca-dissoc (eca--session-tool-servers session)
                                  "alpha"))
                (caar collection)))
      (expect (eca-mcp-toggle-server) :to-throw 'user-error)
      (expect 'eca-api-notify :not :to-have-been-called)))

  (it "does not act while the selected server is stopping"
    (let* ((server '(:name "browser" :status "stopping"))
           (session (eca-mcp-test--make-session server)))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-api-notify)
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt collection &rest _)
                (caar collection)))
      (expect (eca-mcp-toggle-server) :to-throw 'user-error)
      (expect 'eca-api-notify :not :to-have-been-called)))

  (it "rejects an unsupported server status"
    (let* ((server '(:name "custom" :status "unknown"))
           (session (eca-mcp-test--make-session server)))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'eca-api-notify)
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt collection &rest _)
                (caar collection)))
      (expect (eca-mcp-toggle-server) :to-throw 'user-error)
      (expect 'eca-api-notify :not :to-have-been-called)))

  (it "reports when no configured MCP servers are available"
    (let ((session
           (eca-mcp-test--make-session
            '(:name "ECA" :status "running"))))
      (spy-on 'eca-session :and-return-value session)
      (spy-on 'completing-read)
      (expect (eca-mcp-toggle-server) :to-throw 'user-error)
      (expect 'completing-read :not :to-have-been-called))))

(provide 'eca-mcp-test)
;;; eca-mcp-test.el ends here
