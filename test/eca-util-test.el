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

  (it "translates using the first matching mapping"
    (let ((eca-path-mappings '(("/Users/me/other" . "/opt/other")
                               ("/Users/me/dev/project" . "/workspace/project"))))
      (expect (eca--path-local-to-remote "/Users/me/dev/project/package.json")
              :to-equal "/workspace/project/package.json")))

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

  (it "translates using the first match"
    (let ((eca-path-mappings '(("/Users/me/other" . "/opt/other")
                               ("/Users/me/dev/project" . "/workspace/project"))))
      (expect (eca--path-remote-to-local "/workspace/project/package.json")
              :to-equal "/Users/me/dev/project/package.json")))

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

(provide 'eca-util-test)
;;; eca-util-test.el ends here
