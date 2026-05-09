;;; eca-util-test.el --- Tests for eca-util -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'eca-util)

(defvar eca-path-mappings)

(describe "eca--path-local-to-remote"
  (it "translates a mapped local path to a remote path"
    (let ((eca-path-mappings '(("/Users/me/dev/project" . "/workspace/project"))))
      (expect (eca--path-local-to-remote "/Users/me/dev/project/src/main.rs")
              :to-equal "/workspace/project/src/main.rs")))

  (it "translates using the longest (most specific) matching mapping"
    (let ((eca-path-mappings '(("/Users/me/dev" . "/workspace")
                               ("/Users/me/dev/project" . "/custom"))))
      (expect (eca--path-local-to-remote "/Users/me/dev/project/package.json")
              :to-equal "/custom/package.json")))

  (it "longest-prefix wins regardless of order"
    ;; General mapping first, specific override second.
    (let ((eca-path-mappings '(("/Users/me/dev" . "/workspace")
                               ("/Users/me/dev/special" . "/custom"))))
      (expect (eca--path-local-to-remote "/Users/me/dev/special/src/main.rs")
              :to-equal "/custom/src/main.rs")
      (expect (eca--path-local-to-remote "/Users/me/dev/other/src/main.rs")
              :to-equal "/workspace/other/src/main.rs"))
    ;; Specific override first, general mapping second.
    (let ((eca-path-mappings '(("/Users/me/dev/special" . "/custom")
                               ("/Users/me/dev" . "/workspace"))))
      (expect (eca--path-local-to-remote "/Users/me/dev/special/src/main.rs")
              :to-equal "/custom/src/main.rs")
      (expect (eca--path-local-to-remote "/Users/me/dev/other/src/main.rs")
              :to-equal "/workspace/other/src/main.rs")))

  (it "returns the expanded local path if no mapping matches"
    (let ((eca-path-mappings '(("/Users/me/dev/project" . "/workspace/project"))))
      (expect (eca--path-local-to-remote "/Users/me/unmapped-dir/file.txt")
              :to-equal (expand-file-name "/Users/me/unmapped-dir/file.txt"))))

  (it "handles paths with trailing slashes correctly"
    (let ((eca-path-mappings '(("/home/user/app/" . "/app/"))))
      (expect (eca--path-local-to-remote "/home/user/app/src/index.js")
              :to-equal "/app/src/index.js"))))

(describe "eca--path-remote-to-local"
  (it "translates a remote path to a local path"
    (let ((eca-path-mappings '(("/Users/me/dev/project" . "/workspace/project"))))
      (expect (eca--path-remote-to-local "/workspace/project/src/main.rs")
              :to-equal "/Users/me/dev/project/src/main.rs")))

  (it "translates using the longest (most specific) matching mapping"
    (let ((eca-path-mappings '(("/Users/me/dev" . "/workspace")
                               ("/Users/me/dev/project" . "/custom"))))
      (expect (eca--path-remote-to-local "/custom/package.json")
              :to-equal "/Users/me/dev/project/package.json")))

  (it "longest-prefix wins regardless of order"
    ;; General mapping first, specific override second.
    (let ((eca-path-mappings '(("/Users/me/dev" . "/workspace")
                               ("/Users/me/dev/special" . "/custom"))))
      (expect (eca--path-remote-to-local "/custom/src/main.rs")
              :to-equal "/Users/me/dev/special/src/main.rs")
      (expect (eca--path-remote-to-local "/workspace/other/src/main.rs")
              :to-equal "/Users/me/dev/other/src/main.rs"))
    ;; Specific override first, general mapping second.
    (let ((eca-path-mappings '(("/Users/me/dev/special" . "/custom")
                               ("/Users/me/dev" . "/workspace"))))
      (expect (eca--path-remote-to-local "/custom/src/main.rs")
              :to-equal "/Users/me/dev/special/src/main.rs")
      (expect (eca--path-remote-to-local "/workspace/other/src/main.rs")
              :to-equal "/Users/me/dev/other/src/main.rs")))

  (it "returns the inputted path when no match is found"
    (let ((eca-path-mappings '(("/Users/me/dev/project" . "/workspace/project"))))
      (expect (eca--path-remote-to-local "/unmapped/container/path/file.txt")
              :to-equal "/unmapped/container/path/file.txt")))

  (it "handles paths with trailing slashes"
    (let ((eca-path-mappings '(("/home/user/app/" . "/app/"))))
      (expect (eca--path-remote-to-local "/app/src/index.js")
              :to-equal "/home/user/app/src/index.js")))

  (it "does not match substrings"
    (let ((eca-path-mappings '(("/Users/me/project" . "/workspace/project"))))
      (expect (eca--path-local-to-remote "/Users/me/project-api/file.txt")
              :to-equal (expand-file-name "/Users/me/project-api/file.txt"))))

  (it "preserves trailing slashes"
    (let ((eca-path-mappings '(("/Users/me/project" . "/workspace/project"))))
      (expect (eca--path-local-to-remote "/Users/me/project/")
              :to-equal "/workspace/project/")
      (expect (eca--path-local-to-remote "/Users/me/project")
              :to-equal "/workspace/project")
      (expect (eca--path-remote-to-local "/workspace/project/")
              :to-equal (expand-file-name "/Users/me/project/"))))

  (it "normalizes user trailing slashes in the configuration mappings"
    ;; User puts messy trailing slashes in the mapping list
    (let ((eca-path-mappings '(("/Users/me/project/" . "/workspace/project/"))))
      (expect (eca--path-local-to-remote "/Users/me/project/src")
              :to-equal "/workspace/project/src"))))

(describe "eca--path-local-to-remote nested"
  (it "handles nested local paths mapping to the same remote path"
    (let ((eca-path-mappings '(("/Users/me/ws/nested" . "/workspace")
                               ("/Users/me/ws/nested/dev" . "/workspace"))))
      (expect (eca--path-local-to-remote "/Users/me/ws/nested/dev/project/file.el")
              :to-equal "/workspace/project/file.el")
      (expect (eca--path-local-to-remote "/Users/me/ws/nested/other/file.el")
              :to-equal "/workspace/other/file.el"))))

(describe "eca--path-remote-to-local nested"
  (it "handles nested local paths mapping to the same remote path"
    (let ((eca-path-mappings '(("/Users/me/ws/nested" . "/workspace")
                               ("/Users/me/ws/nested/dev" . "/workspace"))))
      (expect (eca--path-remote-to-local "/workspace/project/file.el")
              :to-equal "/Users/me/ws/nested/dev/project/file.el"))))

(describe "eca--path-translation windows"
  (it "handles Windows paths correctly"
    (let ((eca-path-mappings '(("C:/Users/me/ws/win-project" . "/workspace/win-project"))))
      (expect (eca--path-local-to-remote "C:/Users/me/ws/win-project/src/main.rs")
              :to-equal "/workspace/win-project/src/main.rs")
      (expect (eca--path-remote-to-local "/workspace/win-project/src/main.rs")
              :to-equal (expand-file-name "C:/Users/me/ws/win-project/src/main.rs")))))

(provide 'eca-util-test)
;;; eca-util-test.el ends here
