;;; eca-diff-lcs-test.el --- Tests for eca-diff-lcs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'buttercup)
(require 'eca-diff-lcs)

(describe "eca-diff-lcs-opcodes ordering invariant"
  (it "groups all :delete cells before :insert cells in a single run"
    (expect (eca-diff-lcs-opcodes ["second"] ["A" "B"])
            :to-equal '((:delete ("second")) (:insert ("A" "B")))))

  (it "keeps :equal context between independent edit runs"
    (expect (eca-diff-lcs-opcodes ["one" "mid" "two"]
                                  ["ONE" "mid" "TWO"])
            :to-equal '((:delete ("one"))
                        (:insert ("ONE"))
                        (:equal ("mid"))
                        (:delete ("two"))
                        (:insert ("TWO")))))

  (it "returns a single :equal run when inputs match"
    (expect (eca-diff-lcs-opcodes ["a" "b"] ["a" "b"])
            :to-equal '((:equal ("a" "b")))))

  (it "returns no opcodes for two empty vectors"
    (expect (eca-diff-lcs-opcodes [] []) :to-be nil)))

(provide 'eca-diff-lcs-test)
;;; eca-diff-lcs-test.el ends here
