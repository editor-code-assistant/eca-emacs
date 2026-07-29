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
