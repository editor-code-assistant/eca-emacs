;;; eca-completion-diff-test.el --- Tests for eca-completion-diff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'buttercup)
(require 'eca-completion-diff)

(describe "eca-completion-diff-opcodes ordering invariant"
  (it "groups all :delete cells before :insert cells in a single run"
    (expect (eca-completion-diff-opcodes ["second"] ["A" "B"])
            :to-equal '((:delete ("second")) (:insert ("A" "B")))))

  (it "keeps :equal context between independent edit runs"
    (expect (eca-completion-diff-opcodes ["one" "mid" "two"]
                                         ["ONE" "mid" "TWO"])
            :to-equal '((:delete ("one"))
                        (:insert ("ONE"))
                        (:equal ("mid"))
                        (:delete ("two"))
                        (:insert ("TWO")))))

  (it "returns a single :equal run when inputs match"
    (expect (eca-completion-diff-opcodes ["a" "b"] ["a" "b"])
            :to-equal '((:equal ("a" "b")))))

  (it "returns no opcodes for two empty vectors"
    (expect (eca-completion-diff-opcodes [] []) :to-be nil)))

(provide 'eca-completion-diff-test)
;;; eca-completion-diff-test.el ends here
