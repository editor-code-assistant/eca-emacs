;;; eca-diff-test.el --- Tests for eca-diff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'buttercup)
(require 'eca-diff)

(defconst native-eol
  (if (eq system-type 'windows-nt)
      "\r\n"
    "\n")
  "End-of-line string for the current OS.")

(describe "eca-diff-parse-unified-diff"
  (it "parses a unified diff into original and new strings"
    (let* ((diff (string-join '("@@ -1,3 +1,4 @@"
                                "-Line 1"
                                "-Line 2"
                                "+Line 1 modified"
                                "+Line 2"
                                "+Line 3 added")
                              native-eol))
           ;; The parser normalises \r\n to \n before splitting.
           (parsed (eca-diff-parse-unified-diff
                    (replace-regexp-in-string "\r\n" "\n" diff))))
      (expect (plist-get parsed :original)
              :to-equal "Line 1\nLine 2")
      (expect (plist-get parsed :new)
              :to-equal "Line 1 modified\nLine 2\nLine 3 added"))))

(provide 'eca-diff-test)
;;; eca-diff-test.el ends here
