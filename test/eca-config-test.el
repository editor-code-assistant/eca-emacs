;;; eca-config-test.el --- Tests for eca-config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'f)
(require 'eca-util)
(require 'eca-config)

(defvar eca-local-to-remote-prefix-map)

(defun eca-config-test--env (&rest vars)
  "Return `process-environment' with VARS prepended.
A VAR of the form \"NAME=VALUE\" sets NAME, a bare \"NAME\" unsets it."
  (append vars process-environment))

(describe "eca-config--global-path"
  (describe "with the path reported by the server"
    (it "uses it over the local resolution"
      (let ((session (make-eca--session
                      :global-config-path "/srv/home/.config/eca/config.json"
                      :workspace-folders '("/srv/project")))
            (eca-local-to-remote-prefix-map nil)
            (process-environment (eca-config-test--env "XDG_CONFIG_HOME=/tmp/xdg")))
        (expect (eca-config--global-path session)
                :to-equal "/srv/home/.config/eca/config.json")))

    (it "applies the explicit prefix map"
      (let ((session (make-eca--session
                      :global-config-path "/root/.config/eca/config.json"
                      :workspace-folders '("/Users/me/dev/project")))
            (eca-local-to-remote-prefix-map
             '(("/Users/me/eca-home" . "/root/.config/eca"))))
        (expect (eca-config--global-path session)
                :to-equal (expand-file-name "/Users/me/eca-home/config.json"))))

    (it "reuses the TRAMP prefix of remote workspace folders"
      (let ((session (make-eca--session
                      :global-config-path "/home/me/.config/eca/config.json"
                      :workspace-folders '("/docker:container:/workspace/project")))
            (eca-local-to-remote-prefix-map nil))
        (expect (eca-config--global-path session)
                :to-equal "/docker:container:/home/me/.config/eca/config.json")))

    (it "does not prefix a path already translated from a TRAMP folder"
      (let ((session (make-eca--session
                      :global-config-path "/workspace/project/eca/config.json"
                      :workspace-folders '("/docker:container:/workspace/project")))
            (eca-local-to-remote-prefix-map nil))
        (expect (eca-config--global-path session)
                :to-equal "/docker:container:/workspace/project/eca/config.json"))))

  (describe "without a path from the server"
    (it "falls back to XDG_CONFIG_HOME when set"
      (let ((process-environment (eca-config-test--env "XDG_CONFIG_HOME=/tmp/xdg")))
        (expect (eca-config--global-path (make-eca--session))
                :to-equal (f-join "/tmp/xdg" "eca" "config.json"))))

    (it "falls back to ~/.config/eca/config.json"
      (let ((system-type 'gnu/linux)
            (process-environment (eca-config-test--env "XDG_CONFIG_HOME"
                                                       "USERPROFILE=/Users/other")))
        (expect (eca-config--global-path)
                :to-equal (f-join (f-expand "~") ".config" "eca" "config.json"))))

    (it "uses USERPROFILE on Windows like the server's user.home"
      (let ((system-type 'windows-nt)
            (process-environment (eca-config-test--env "XDG_CONFIG_HOME"
                                                       "USERPROFILE=/Users/me")))
        (expect (eca-config--global-path)
                :to-equal (f-join "/Users/me" ".config" "eca" "config.json"))))))

(provide 'eca-config-test)
;;; eca-config-test.el ends here
