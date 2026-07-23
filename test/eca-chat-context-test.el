;;; eca-chat-context-test.el --- Tests for eca-chat-context -*- lexical-binding: t; -*-
;;; Commentary:
;; Tests for buffer (text) contexts and related helpers.
;;; Code:
(require 'buttercup)
(require 'eca-chat)

(describe "eca-chat-context-buffer-include-p"
  (it "includes non-file buffers"
    (let ((buf (generate-new-buffer "*compilation-ctx-test*")))
      (unwind-protect
          (expect (eca-chat-context-buffer-include-p buf) :to-be-truthy)
        (kill-buffer buf))))

  (it "excludes hidden buffers"
    (let ((buf (generate-new-buffer " *hidden-ctx-test*")))
      (unwind-protect
          (expect (eca-chat-context-buffer-include-p buf) :to-be nil)
        (kill-buffer buf))))

  (it "excludes eca own buffers"
    (let ((chat (generate-new-buffer "<eca-chat[proj]:1:1>"))
          (other (generate-new-buffer "*eca-diff-orig:foo*")))
      (unwind-protect
          (progn
            (expect (eca-chat-context-buffer-include-p chat) :to-be nil)
            (expect (eca-chat-context-buffer-include-p other) :to-be nil))
        (kill-buffer chat)
        (kill-buffer other))))

  (it "excludes file-visiting buffers"
    (let ((buf (generate-new-buffer "file-visiting-ctx-test")))
      (unwind-protect
          (with-current-buffer buf
            (setq buffer-file-name "/tmp/eca-ctx-test-file.txt")
            (expect (eca-chat-context-buffer-include-p buf) :to-be nil))
        (with-current-buffer buf
          (setq buffer-file-name nil)
          (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(describe "eca-chat--buffer-contexts"
  (it "returns text contexts for eligible buffers"
    (let ((buf (generate-new-buffer "*ctx-listing-test*")))
      (unwind-protect
          (expect (member (list :type "text" :label "*ctx-listing-test*")
                          (eca-chat--buffer-contexts))
                  :to-be-truthy)
        (kill-buffer buf))))

  (it "skips buffers already added to the context"
    (let ((buf (generate-new-buffer "*ctx-added-test*")))
      (unwind-protect
          (let ((eca-chat--context (list (list :type "text" :label "*ctx-added-test*"))))
            (expect (member (list :type "text" :label "*ctx-added-test*")
                            (eca-chat--buffer-contexts))
                    :to-be nil))
        (kill-buffer buf)))))

(describe "eca-chat--buffer-context-content"
  (it "returns whole content when under the limit"
    (with-temp-buffer
      (insert "hello")
      (let ((eca-chat-context-buffer-max-chars 100))
        (expect (eca-chat--buffer-context-content (current-buffer))
                :to-equal "hello"))))

  (it "keeps the tail when over the limit"
    (with-temp-buffer
      (insert "0123456789")
      (let ((eca-chat-context-buffer-max-chars 4))
        (expect (eca-chat--buffer-context-content (current-buffer))
                :to-equal "6789"))))

  (it "returns whole content when limit is nil"
    (with-temp-buffer
      (insert "0123456789")
      (let ((eca-chat-context-buffer-max-chars nil))
        (expect (eca-chat--buffer-context-content (current-buffer))
                :to-equal "0123456789"))))

  (it "ignores narrowing"
    (with-temp-buffer
      (insert "abcdef")
      (narrow-to-region 1 3)
      (let ((eca-chat-context-buffer-max-chars nil))
        (expect (eca-chat--buffer-context-content (current-buffer))
                :to-equal "abcdef")))))

(describe "eca-chat--materialize-context"
  (it "fills text context with fresh buffer content"
    (let ((buf (generate-new-buffer "*materialize-test*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (insert "output line"))
            (let ((context (eca-chat--materialize-context
                            (list :type "text" :label "*materialize-test*"))))
              (expect (plist-get context :content) :to-equal "output line")))
        (kill-buffer buf))))

  (it "does not mutate the original context"
    (let ((buf (generate-new-buffer "*materialize-orig-test*")))
      (unwind-protect
          (let ((original (list :type "text" :label "*materialize-orig-test*")))
            (eca-chat--materialize-context original)
            (expect (plist-get original :content) :to-be nil))
        (kill-buffer buf))))

  (it "drops contexts of killed buffers"
    (expect (eca-chat--materialize-context
             (list :type "text" :label "*no-such-buffer-eca-test*"))
            :to-be nil))

  (it "passes other contexts through unchanged"
    (let ((context (list :type "file" :path "/tmp/foo.txt")))
      (expect (eca-chat--materialize-context context) :to-be context))))

(describe "eca-chat--context->str for text contexts"
  (it "renders the buffer name with the context prefix"
    (let ((str (eca-chat--context->str (list :type "text" :label "*compilation*"))))
      (expect (substring-no-properties str) :to-equal "@*compilation*")
      (expect (get-text-property 0 'eca-chat-context-item str)
              :to-equal (list :type "text" :label "*compilation*")))))

(describe "eca-chat--context-to-completion for text contexts"
  (it "uses the buffer name as label"
    (let ((item (eca-chat--context-to-completion
                 "" '("/tmp") (list :type "text" :label "*shell*"))))
      (expect (substring-no-properties item) :to-equal "*shell*")
      (expect (plist-get (get-text-property 0 'eca-chat-completion-item item) :type)
              :to-equal "text"))))

(describe "eca-chat--normalize-prompt with text context chips"
  (it "keeps non-path context labels as-is"
    (let* ((chip (eca-chat--context->str (list :type "text" :label "*compilation*") 'static))
           (prompt (concat "check " chip " please")))
      (expect (eca-chat--normalize-prompt prompt)
              :to-equal "check @*compilation* please"))))

(provide 'eca-chat-context-test)
;;; eca-chat-context-test.el ends here
