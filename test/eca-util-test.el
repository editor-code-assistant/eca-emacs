;;; eca-util-test.el --- Tests for eca-util -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'eca-util)

(defvar eca-local-to-remote-prefix-map)

(describe "eca--path-local-to-remote"
  (it "translates a mapped local path to a remote path"
    (let ((eca-local-to-remote-prefix-map '(("/Users/me/dev/project" . "/workspace/project"))))
      (expect (eca--path-local-to-remote "/Users/me/dev/project/src/main.rs")
              :to-equal "/workspace/project/src/main.rs")))

  (it "translates using the longest (most specific) matching mapping"
    (let ((eca-local-to-remote-prefix-map '(("/Users/me/dev" . "/workspace")
                               ("/Users/me/dev/project" . "/custom"))))
      (expect (eca--path-local-to-remote "/Users/me/dev/project/package.json")
              :to-equal "/custom/package.json")))

  (it "longest-prefix wins regardless of order"
    ;; General mapping first, specific override second.
    (let ((eca-local-to-remote-prefix-map '(("/Users/me/dev" . "/workspace")
                               ("/Users/me/dev/special" . "/custom"))))
      (expect (eca--path-local-to-remote "/Users/me/dev/special/src/main.rs")
              :to-equal "/custom/src/main.rs")
      (expect (eca--path-local-to-remote "/Users/me/dev/other/src/main.rs")
              :to-equal "/workspace/other/src/main.rs"))
    ;; Specific override first, general mapping second.
    (let ((eca-local-to-remote-prefix-map '(("/Users/me/dev/special" . "/custom")
                               ("/Users/me/dev" . "/workspace"))))
      (expect (eca--path-local-to-remote "/Users/me/dev/special/src/main.rs")
              :to-equal "/custom/src/main.rs")
      (expect (eca--path-local-to-remote "/Users/me/dev/other/src/main.rs")
              :to-equal "/workspace/other/src/main.rs")))

  (it "returns the expanded local path if no mapping matches"
    (let ((eca-local-to-remote-prefix-map '(("/Users/me/dev/project" . "/workspace/project"))))
      (expect (eca--path-local-to-remote "/Users/me/unmapped-dir/file.txt")
              :to-equal (expand-file-name "/Users/me/unmapped-dir/file.txt"))))

  (it "handles paths with trailing slashes correctly"
    (let ((eca-local-to-remote-prefix-map '(("/home/user/app/" . "/app/"))))
      (expect (eca--path-local-to-remote "/home/user/app/src/index.js")
              :to-equal "/app/src/index.js"))))

(describe "eca--path-remote-to-local"
  (it "translates a remote path to a local path"
    (let ((eca-local-to-remote-prefix-map '(("/Users/me/dev/project" . "/workspace/project"))))
      (expect (eca--path-remote-to-local "/workspace/project/src/main.rs")
              :to-equal (expand-file-name "/Users/me/dev/project/src/main.rs"))))

  (it "translates using the longest (most specific) matching mapping"
    (let ((eca-local-to-remote-prefix-map '(("/Users/me/dev" . "/workspace")
                               ("/Users/me/dev/project" . "/custom"))))
      (expect (eca--path-remote-to-local "/custom/package.json")
              :to-equal (expand-file-name "/Users/me/dev/project/package.json"))))

  (it "longest-prefix wins regardless of order"
    ;; General mapping first, specific override second.
    (let ((eca-local-to-remote-prefix-map '(("/Users/me/dev" . "/workspace")
                               ("/Users/me/dev/special" . "/custom"))))
      (expect (eca--path-remote-to-local "/custom/src/main.rs")
              :to-equal (expand-file-name "/Users/me/dev/special/src/main.rs"))
      (expect (eca--path-remote-to-local "/workspace/other/src/main.rs")
              :to-equal (expand-file-name "/Users/me/dev/other/src/main.rs")))
    ;; Specific override first, general mapping second.
    (let ((eca-local-to-remote-prefix-map '(("/Users/me/dev/special" . "/custom")
                               ("/Users/me/dev" . "/workspace"))))
      (expect (eca--path-remote-to-local "/custom/src/main.rs")
              :to-equal (expand-file-name "/Users/me/dev/special/src/main.rs"))
      (expect (eca--path-remote-to-local "/workspace/other/src/main.rs")
              :to-equal (expand-file-name "/Users/me/dev/other/src/main.rs"))))

  (it "returns the inputted path when no match is found"
    (let ((eca-local-to-remote-prefix-map '(("/Users/me/dev/project" . "/workspace/project"))))
      (expect (eca--path-remote-to-local "/unmapped/container/path/file.txt")
              :to-equal "/unmapped/container/path/file.txt")))

  (it "handles paths with trailing slashes"
    (let ((eca-local-to-remote-prefix-map '(("/home/user/app/" . "/app/"))))
      (expect (eca--path-remote-to-local "/app/src/index.js")
              :to-equal (expand-file-name "/home/user/app/src/index.js"))))

  (it "does not match substrings"
    (let ((eca-local-to-remote-prefix-map '(("/Users/me/project" . "/workspace/project"))))
      (expect (eca--path-local-to-remote "/Users/me/project-api/file.txt")
              :to-equal (expand-file-name "/Users/me/project-api/file.txt"))))

  (it "preserves trailing slashes"
    (let ((eca-local-to-remote-prefix-map '(("/Users/me/project" . "/workspace/project"))))
      (expect (eca--path-local-to-remote "/Users/me/project/")
              :to-equal "/workspace/project/")
      (expect (eca--path-local-to-remote "/Users/me/project")
              :to-equal "/workspace/project")
      (expect (eca--path-remote-to-local "/workspace/project/")
              :to-equal (expand-file-name "/Users/me/project/"))))

  (it "normalizes user trailing slashes in the configuration mappings"
    ;; User puts messy trailing slashes in the mapping list
    (let ((eca-local-to-remote-prefix-map '(("/Users/me/project/" . "/workspace/project/"))))
      (expect (eca--path-local-to-remote "/Users/me/project/src")
              :to-equal "/workspace/project/src"))))

(describe "eca--path-local-to-remote nested"
  (it "handles nested local paths mapping to the same remote path"
    (let ((eca-local-to-remote-prefix-map '(("/Users/me/ws/nested" . "/workspace")
                               ("/Users/me/ws/nested/dev" . "/workspace"))))
      (expect (eca--path-local-to-remote "/Users/me/ws/nested/dev/project/file.el")
              :to-equal "/workspace/project/file.el")
      (expect (eca--path-local-to-remote "/Users/me/ws/nested/other/file.el")
              :to-equal "/workspace/other/file.el"))))

(describe "eca--path-remote-to-local nested"
  (it "handles nested local paths mapping to the same remote path"
    (let ((eca-local-to-remote-prefix-map '(("/Users/me/ws/nested" . "/workspace")
                               ("/Users/me/ws/nested/dev" . "/workspace"))))
      (expect (eca--path-remote-to-local "/workspace/project/file.el")
              :to-equal (expand-file-name "/Users/me/ws/nested/dev/project/file.el")))))

(describe "eca--path-translation windows"
  (it "handles Windows paths correctly"
    (let ((eca-local-to-remote-prefix-map '(("C:/Users/me/ws/win-project" . "/workspace/win-project"))))
      (expect (eca--path-local-to-remote "C:/Users/me/ws/win-project/src/main.rs")
              :to-equal "/workspace/win-project/src/main.rs")
      (expect (eca--path-remote-to-local "/workspace/win-project/src/main.rs")
              :to-equal (expand-file-name "C:/Users/me/ws/win-project/src/main.rs")))))

(describe "eca--path-local-to-remote with TRAMP"
  (it "strips TRAMP prefix before applying prefix map"
    (let ((eca-local-to-remote-prefix-map
           '(("/workspace/project" . "/mnt/project"))))
      (expect (eca--path-local-to-remote
               "/docker:container:/workspace/project/src/file.el")
              :to-equal "/mnt/project/src/file.el")))

  (it "strips TRAMP and returns expanded local part when no mapping matches"
    (let ((eca-local-to-remote-prefix-map nil))
      (expect (eca--path-local-to-remote
               "/docker:container:/workspace/project/src/file.el")
              :to-equal (expand-file-name
                         "/workspace/project/src/file.el"))))

  (it "handles non-TRAMP paths unchanged (no double-stripping)"
    (let ((eca-local-to-remote-prefix-map
           '(("/Users/me/dev" . "/workspace"))))
      (expect (eca--path-local-to-remote "/Users/me/dev/src/file.el")
              :to-equal "/workspace/src/file.el"))))

(describe "eca--path-remote-to-local with TRAMP workspace folders"
  (it "uses TRAMP workspace folder when no explicit mapping matches"
    (let ((session (make-eca--session))
          (eca-local-to-remote-prefix-map nil))
      (setf (eca--session-workspace-folders session)
            '("/docker:container:/workspace/project"))
      (spy-on 'eca-session :and-return-value session)
      (expect (eca--path-remote-to-local "/workspace/project/src/file.el")
              :to-equal "/docker:container:/workspace/project/src/file.el")))

  (it "matches the most specific TRAMP workspace folder"
    (let ((session (make-eca--session))
          (eca-local-to-remote-prefix-map nil))
      (setf (eca--session-workspace-folders session)
            '("/docker:a:/workspace"
              "/docker:b:/workspace/project"))
      (spy-on 'eca-session :and-return-value session)
      (expect (eca--path-remote-to-local "/workspace/project/src/file.el")
              :to-equal "/docker:b:/workspace/project/src/file.el")))

  (it "handles exact TRAMP workspace folder match (no trailing slash)"
    (let ((session (make-eca--session))
          (eca-local-to-remote-prefix-map nil))
      (setf (eca--session-workspace-folders session)
            '("/docker:container:/workspace/project"))
      (spy-on 'eca-session :and-return-value session)
      (expect (eca--path-remote-to-local "/workspace/project")
              :to-equal "/docker:container:/workspace/project")))

  (it "prefers explicit prefix map over TRAMP workspace folders"
    (let ((session (make-eca--session))
          (eca-local-to-remote-prefix-map
           '(("/Users/me/dev" . "/workspace/project"))))
      (setf (eca--session-workspace-folders session)
            '("/docker:container:/workspace/project"))
      (spy-on 'eca-session :and-return-value session)
      (expect (eca--path-remote-to-local "/workspace/project/src/file.el")
              :to-equal (expand-file-name
                         "/Users/me/dev/src/file.el"))))

  (it "returns path unchanged when neither mapping nor TRAMP matches"
    (let ((session (make-eca--session))
          (eca-local-to-remote-prefix-map nil))
      (setf (eca--session-workspace-folders session)
            '("/Users/me/local-project"))
      (spy-on 'eca-session :and-return-value session)
      (expect (eca--path-remote-to-local "/unmapped/path/file.el")
              :to-equal "/unmapped/path/file.el"))))

(describe "eca--path-session"
  (it "takes precedence over the current buffer's session"
    (let ((session (make-eca--session))
          (other (make-eca--session))
          (eca-local-to-remote-prefix-map nil))
      (setf (eca--session-workspace-folders session)
            '("/docker:container:/workspace/project"))
      (setf (eca--session-workspace-folders other)
            '("/Users/me/local-project"))
      (spy-on 'eca-session :and-return-value other)
      (let ((eca--path-session session))
        (expect (eca--path-remote-to-local "/workspace/project/src/file.el")
                :to-equal "/docker:container:/workspace/project/src/file.el"))))

  (it "avoids buffer session lookup entirely when bound"
    (let ((session (make-eca--session))
          (eca-local-to-remote-prefix-map nil))
      (setf (eca--session-workspace-folders session)
            '("/docker:container:/workspace/project"))
      (spy-on 'eca-session :and-throw-error 'error)
      (let ((eca--path-session session))
        (expect (eca--path-remote-to-local "/workspace/project/src/file.el")
                :to-equal "/docker:container:/workspace/project/src/file.el")))))

(describe "eca--git-common-dir"
  (it "expands a relative git output against the directory"
    (let ((eca--git-common-dir-cache (make-hash-table :test 'equal)))
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _) (insert ".git\n") 0))
      (expect (eca--git-common-dir "/tmp/eca-repo")
              :to-equal (expand-file-name ".git" "/tmp/eca-repo"))))

  (it "caches positive lookups, spawning git once per directory"
    (let ((eca--git-common-dir-cache (make-hash-table :test 'equal)))
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _) (insert "/repos/main/.git\n") 0))
      (eca--git-common-dir "/tmp/eca-worktree")
      (expect (eca--git-common-dir "/tmp/eca-worktree")
              :to-equal (expand-file-name "/repos/main/.git"))
      (expect 'process-file :to-have-been-called-times 1)))

  (it "caches negative lookups too"
    (let ((eca--git-common-dir-cache (make-hash-table :test 'equal)))
      (spy-on 'process-file :and-call-fake (lambda (&rest _) 128))
      (expect (eca--git-common-dir "/tmp/eca-no-repo") :to-be nil)
      (expect (eca--git-common-dir "/tmp/eca-no-repo") :to-be nil)
      (expect 'process-file :to-have-been-called-times 1)))

  (it "treats the git < 2.5 literal flag echo as no repo"
    (let ((eca--git-common-dir-cache (make-hash-table :test 'equal)))
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _) (insert "--git-common-dir\n") 0))
      (expect (eca--git-common-dir "/tmp/eca-old-git") :to-be nil)))

  (it "is cleared when sessions are created or deleted"
    (let ((eca--git-common-dir-cache (make-hash-table :test 'equal))
          (eca--sessions '())
          (eca--session-ids 0))
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _) (insert ".git\n") 0))
      (eca--git-common-dir "/tmp/eca-repo")
      (expect (hash-table-count eca--git-common-dir-cache) :to-equal 1)
      (let ((session (eca-create-session '("/tmp/eca-repo"))))
        (expect (hash-table-count eca--git-common-dir-cache) :to-equal 0)
        (eca--git-common-dir "/tmp/eca-repo")
        (eca-delete-session session)
        (expect (hash-table-count eca--git-common-dir-cache) :to-equal 0)))))

(provide 'eca-util-test)
;;; eca-util-test.el ends here
