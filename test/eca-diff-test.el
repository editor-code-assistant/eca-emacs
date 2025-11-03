;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'eca-diff)

(defconst native-eol
  (if (eq system-type 'windows-nt)
      "\r\n"
    "\n")
  "Native end-of-line string for the current operating system.")

(describe "eca-diff-show-ediff"
          :var ((insert-orig (symbol-function 'insert))
                (contents '()))
          (before-each
           (spy-on 'insert
                   :and-call-fake (lambda (&rest args)
                                    (apply insert-orig args)
                                    (push (list (buffer-name) (buffer-string)) contents )
                                    )))
          (it "can display unified diffs with native EOLs"
              (let* ((inhibit-message t)
                     (path "/a/path")
                     (diff (string-join '("@@ -1,3 +1,4 @@"
                                          "-Line 1"
                                          "-Line 2"
                                          "+Line 1 modified"
                                          "+Line 2"
                                          "+Line 3 added")
                                        native-eol)))

                ;; We expect this function to call `insert` twice.
                (eca-diff-show-ediff path diff)
                (expect (reverse contents)
                        :to-equal
                        `((,(format "*eca-diff-orig:%s*" path) "Line 1
Line 2")
                          (,(format "*eca-diff-new:%s*" path) "Line 1 modified
Line 2
Line 3 added"))))))
