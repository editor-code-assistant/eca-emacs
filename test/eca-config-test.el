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
                :to-equal "/docker:container:/workspace/project/eca/config.json")))

    (it "does not query the remote host"
      (spy-on 'process-file)
      (let ((session (make-eca--session
                      :global-config-path "/home/me/.config/eca/config.json"
                      :workspace-folders '("/ssh:vm:/home/me/project")))
            (eca-local-to-remote-prefix-map nil))
        (expect (eca-config--global-path session)
                :to-equal "/ssh:vm:/home/me/.config/eca/config.json")
        (expect 'process-file :not :to-have-been-called))))

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
                :to-equal (f-join "/Users/me" ".config" "eca" "config.json"))))

    (it "does not query any host for local sessions"
      (spy-on 'process-file)
      (let ((process-environment (eca-config-test--env "XDG_CONFIG_HOME=/tmp/xdg")))
        (expect (eca-config--global-path
                 (make-eca--session :workspace-folders '("/home/me/project")))
                :to-equal (f-join "/tmp/xdg" "eca" "config.json")))
      (expect 'process-file :not :to-have-been-called))

    (describe "on a remote host"
      :var (session)
      (before-each
        (setq session (make-eca--session
                       :workspace-folders '("/ssh:vm:/home/me/project"))))

      (it "uses its XDG_CONFIG_HOME, not the local one"
        (let ((lookup-directory nil)
              (process-environment (eca-config-test--env "XDG_CONFIG_HOME=/tmp/xdg")))
          (spy-on 'process-file :and-call-fake
                  (lambda (&rest _)
                    (setq lookup-directory default-directory)
                    (insert "/home/me/.xdg\n")
                    0))
          (expect (eca-config--global-path session)
                  :to-equal "/ssh:vm:/home/me/.xdg/eca/config.json")
          (expect 'process-file :to-have-been-called-with
                  "printenv" nil '(t nil) nil "XDG_CONFIG_HOME")
          (expect lookup-directory :to-equal "/ssh:vm:/home/me/project/")))

      (it "falls back to ~/.config of the remote user"
        (spy-on 'process-file :and-return-value 1)
        (let ((process-environment (eca-config-test--env "XDG_CONFIG_HOME=/tmp/xdg")))
          (expect (eca-config--global-path session)
                  :to-equal "/ssh:vm:~/.config/eca/config.json")))

      (it "treats an empty XDG_CONFIG_HOME as unset"
        (spy-on 'process-file :and-call-fake
                (lambda (&rest _) (insert "\n") 0))
        (expect (eca-config--global-path session)
                :to-equal "/ssh:vm:~/.config/eca/config.json"))

      (it "falls back to ~/.config of the remote user when the lookup fails"
        (spy-on 'process-file :and-throw-error 'file-error)
        (expect (eca-config--global-path session)
                :to-equal "/ssh:vm:~/.config/eca/config.json")))))

(defun eca-config-test--call-with-buffer (fn)
  "Call FN with a config buffer visiting a file in a missing directory.
FN gets the buffer, its session and the path of the file."
  (let* ((dir (make-temp-file "eca-config-test" t))
         (path (expand-file-name "eca/config.json" dir))
         (session (make-eca--session :id 1 :workspace-folders (list dir)))
         (buffer (eca-config--create-buffer "global-config" session path)))
    (unwind-protect
        (funcall fn buffer session path)
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (delete-directory dir t))))

(defun eca-config-test--edit-and-save (buffer)
  "Modify BUFFER and save it."
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert " ")
    (let ((inhibit-message t))
      (save-buffer))))

(describe "eca-config--create-buffer"
  (before-each
    ;; Answer like a user would, so a prompt fails the expectation
    ;; below instead of aborting the save.
    (spy-on 'y-or-n-p :and-return-value t))

  (it "creates the missing config directory on save without asking"
    (eca-config-test--call-with-buffer
     (lambda (buffer _session path)
       (eca-config-test--edit-and-save buffer)
       (expect (file-exists-p path) :to-be-truthy)
       (expect 'y-or-n-p :not :to-have-been-called))))

  (it "keeps creating it once a refresh re-enabled the major mode"
    (eca-config-test--call-with-buffer
     (lambda (buffer session path)
       (eca-config-test--edit-and-save buffer)
       (eca-config--refresh session buffer)
       (delete-directory (file-name-directory path) t)
       (eca-config-test--edit-and-save buffer)
       (expect (file-exists-p path) :to-be-truthy)
       (expect 'y-or-n-p :not :to-have-been-called)))))

(provide 'eca-config-test)
;;; eca-config-test.el ends here
