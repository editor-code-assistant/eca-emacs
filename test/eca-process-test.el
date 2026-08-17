;;; eca-process-test.el --- Tests for eca-process -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'buttercup)
(require 'eca-process)

(describe "eca-process--server-command"
  (before-each
    (setq eca-custom-command nil)
    (spy-on 'executable-find :and-return-value nil))

  (after-each
    (setq eca-custom-command nil))

  (it "uses eca-custom-command when set"
    (setq eca-custom-command '("my-eca" "server"))
    (expect (plist-get (eca-process--server-command) :decision)
            :to-be 'custom))

  (it "uses eca from PATH when available"
    (spy-on 'executable-find :and-return-value "/usr/bin/eca")
    (expect (plist-get (eca-process--server-command) :decision)
            :to-be 'system))

  (describe "with a downloaded server binary"
    (before-each
      (spy-on 'f-exists? :and-call-fake
              (lambda (path) (string= path eca-server-install-path))))

    (it "keeps installed binary when up to date"
      (spy-on 'eca-process--get-current-server-version :and-return-value "0.2.0")
      (spy-on 'eca-process--get-latest-server-version :and-return-value "0.2.0")
      (expect (plist-get (eca-process--server-command) :decision)
              :to-be 'already-installed))

    (it "keeps installed binary when newer than latest"
      (spy-on 'eca-process--get-current-server-version :and-return-value "0.3.0")
      (spy-on 'eca-process--get-latest-server-version :and-return-value "0.2.0")
      (expect (plist-get (eca-process--server-command) :decision)
              :to-be 'already-installed))

    (it "downloads when outdated"
      (spy-on 'eca-process--get-current-server-version :and-return-value "0.1.0")
      (spy-on 'eca-process--get-latest-server-version :and-return-value "0.2.0")
      (let ((result (eca-process--server-command)))
        (expect (plist-get result :decision) :to-be 'download)
        (expect (plist-get result :latest-version) :to-equal "0.2.0")))

    (it "downloads when version file is missing (interrupted update)"
      (spy-on 'eca-process--get-current-server-version :and-return-value nil)
      (spy-on 'eca-process--get-latest-server-version :and-return-value "0.2.0")
      (let ((result (eca-process--server-command)))
        (expect (plist-get result :decision) :to-be 'download)
        (expect (plist-get result :latest-version) :to-equal "0.2.0")))

    (it "keeps installed binary when latest version cannot be fetched"
      (spy-on 'eca-process--get-current-server-version :and-return-value "0.1.0")
      (spy-on 'eca-process--get-latest-server-version :and-return-value nil)
      (expect (plist-get (eca-process--server-command) :decision)
              :to-be 'already-installed))

    (it "keeps installed binary when both versions are unknown"
      (spy-on 'eca-process--get-current-server-version :and-return-value nil)
      (spy-on 'eca-process--get-latest-server-version :and-return-value nil)
      (expect (plist-get (eca-process--server-command) :decision)
              :to-be 'already-installed)))

  (describe "without a downloaded server binary"
    (before-each
      (spy-on 'f-exists? :and-return-value nil))

    (it "errors when latest version cannot be fetched"
      (spy-on 'eca-process--get-latest-server-version :and-return-value nil)
      (expect (plist-get (eca-process--server-command) :decision)
              :to-be 'error-download))

    (it "downloads the latest version"
      (spy-on 'eca-process--get-latest-server-version :and-return-value "0.2.0")
      (let ((result (eca-process--server-command)))
        (expect (plist-get result :decision) :to-be 'download)
        (expect (plist-get result :latest-version) :to-equal "0.2.0")))))

(describe "eca--curl-download-string"
  (before-each
    (spy-on 'executable-find :and-return-value "/usr/bin/curl"))

  (it "returns the output when curl succeeds"
    (spy-on 'call-process :and-call-fake
            (lambda (&rest _) (insert "[{\"tag_name\": \"0.2.0\"}]") 0))
    (expect (eca--curl-download-string "https://example.com")
            :to-equal "[{\"tag_name\": \"0.2.0\"}]"))

  (it "errors when curl exits non-zero"
    (spy-on 'call-process :and-return-value 7)
    (expect (eca--curl-download-string "https://example.com") :to-throw 'error))

  (it "errors when curl returns an empty response"
    (spy-on 'call-process :and-return-value 0)
    (expect (eca--curl-download-string "https://example.com") :to-throw 'error)))

(describe "eca-process--fetch-releases"
  (before-each
    (setq eca-process--releases-cache nil
          eca-process--releases-fetch-failed-at nil)
    (spy-on 'eca-warn))

  (after-each
    (setq eca-process--releases-cache nil
          eca-process--releases-fetch-failed-at nil))

  (it "fetches, caches and returns the releases list"
    (spy-on 'eca--curl-download-string
            :and-return-value "[{\"tag_name\": \"0.2.0\"}]")
    (expect (eca-process--get-latest-server-version) :to-equal "0.2.0")
    (expect (consp eca-process--releases-cache) :to-be-truthy))

  (it "returns stale cache and starts cooldown when the fetch fails"
    (let ((eca-server-releases-cache-ttl 0))
      (setq eca-process--releases-cache
            (cons (float-time) (vector '(:tag_name "0.1.0"))))
      (spy-on 'eca--curl-download-string
              :and-call-fake (lambda (_) (error "github down")))
      (expect (eca-process--get-latest-server-version) :to-equal "0.1.0")
      (expect 'eca-warn :to-have-been-called)
      (expect eca-process--releases-fetch-failed-at :not :to-be nil)))

  (it "does not refetch while within the failure cooldown"
    (spy-on 'eca--curl-download-string
            :and-call-fake (lambda (_) (error "github down")))
    (expect (eca-process--fetch-releases) :to-be nil)
    (expect (eca-process--fetch-releases) :to-be nil)
    (expect (spy-calls-count 'eca--curl-download-string) :to-equal 1))

  (it "refetches after the cooldown has passed"
    (spy-on 'eca--curl-download-string
            :and-call-fake (lambda (_) (error "github down")))
    (eca-process--fetch-releases)
    (setq eca-process--releases-fetch-failed-at
          (- (float-time) (1+ eca-process--releases-failure-cooldown)))
    (eca-process--fetch-releases)
    (expect (spy-calls-count 'eca--curl-download-string) :to-equal 2))

  (it "does not cache unexpected payloads like rate-limit errors"
    (spy-on 'eca--curl-download-string
            :and-return-value "{\"message\": \"rate limited\"}")
    (expect (eca-process--fetch-releases) :to-be nil)
    (expect eca-process--releases-cache :to-be nil)
    (expect 'eca-warn :to-have-been-called)))

(describe "eca-process--download-server"
  (before-each
    (spy-on 'eca-info)
    (spy-on 'eca-error)
    (spy-on 'eca-process--cleanup-old-server)
    (spy-on 'f-exists? :and-return-value nil)
    (spy-on 'mkdir)
    (spy-on 'eca-process--download-url
            :and-return-value "https://example.com/eca.zip"))

  (it "calls on-error instead of signaling when the download fails"
    (let ((eca-server-download-method 'curl)
          (on-error-err nil)
          (downloaded nil))
      (spy-on 'eca--curl-download-file
              :and-call-fake (lambda (&rest _) (error "boom")))
      (eca-process--download-server (lambda () (setq downloaded t))
                                    "0.2.0"
                                    (lambda (err) (setq on-error-err err)))
      (expect downloaded :to-be nil)
      (expect on-error-err :not :to-be nil)))

  (it "reports the error when no on-error is given"
    (let ((eca-server-download-method 'curl))
      (spy-on 'eca--curl-download-file
              :and-call-fake (lambda (&rest _) (error "boom")))
      (eca-process--download-server (lambda ()) "0.2.0")
      (expect 'eca-error :to-have-been-called))))

(describe "eca-process-start"
  (it "falls back to the installed server when the update download fails"
    (let ((session (make-eca--session))
          (started nil))
      (spy-on 'eca-info)
      (spy-on 'eca-warn)
      (spy-on 'eca-process--cleanup-old-server)
      (spy-on 'f-exists? :and-return-value t)
      (spy-on 'eca-process--server-command
              :and-return-value (list :decision 'download
                                      :latest-version "9.9.9"
                                      :command (list "eca" "server")))
      (spy-on 'eca-process--download-server
              :and-call-fake (lambda (_on-done _version on-error)
                               (funcall on-error "github down")))
      (spy-on 'make-process :and-return-value 'fake-process)
      (eca-process-start session (lambda () (setq started t)) #'ignore)
      (expect 'make-process :to-have-been-called)
      (expect 'eca-warn :to-have-been-called)
      (expect started :to-be t))))

(describe "eca-server-check-updates"
  (before-each
    (spy-on 'eca-info)
    (spy-on 'eca-warn))

  (it "warns when the update check fails"
    (spy-on 'eca-process--get-latest-server-version :and-return-value nil)
    (spy-on 'eca-process--get-current-server-version :and-return-value nil)
    (eca-server-check-updates)
    (expect 'eca-warn :to-have-been-called))

  (it "reports not installed when binary is absent"
    (spy-on 'eca-process--get-latest-server-version :and-return-value "0.2.0")
    (spy-on 'eca-process--get-current-server-version :and-return-value nil)
    (spy-on 'f-exists? :and-return-value nil)
    (eca-server-check-updates)
    (expect (car (spy-calls-args-for 'eca-info 0))
            :to-match "No eca server installed"))

  (it "reports unknown version when binary exists without version file"
    (spy-on 'eca-process--get-latest-server-version :and-return-value "0.2.0")
    (spy-on 'eca-process--get-current-server-version :and-return-value nil)
    (spy-on 'f-exists? :and-call-fake
            (lambda (path) (string= path eca-server-install-path)))
    (eca-server-check-updates)
    (expect (car (spy-calls-args-for 'eca-info 0))
            :to-match "version is unknown"))

  (it "reports available upgrade when outdated"
    (spy-on 'eca-process--get-latest-server-version :and-return-value "0.2.0")
    (spy-on 'eca-process--get-current-server-version :and-return-value "0.1.0")
    (spy-on 'f-exists? :and-return-value t)
    (eca-server-check-updates)
    (expect (car (spy-calls-args-for 'eca-info 0))
            :to-match "is available"))

  (it "reports up to date when current matches latest"
    (spy-on 'eca-process--get-latest-server-version :and-return-value "0.2.0")
    (spy-on 'eca-process--get-current-server-version :and-return-value "0.2.0")
    (spy-on 'f-exists? :and-return-value t)
    (eca-server-check-updates)
    (expect (car (spy-calls-args-for 'eca-info 0))
            :to-match "up to date")))

(provide 'eca-process-test)
;;; eca-process-test.el ends here
