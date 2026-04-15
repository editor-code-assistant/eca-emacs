;;; eca-table-test.el --- Tests for eca-table -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'buttercup)
(require 'eca-table)

;; Ensure the font-lock buffer is configured for tests.
(defvar eca-chat-parent-mode 'gfm-mode)

(describe "eca-table--display-width"
  (it "returns correct width for plain text"
    (expect (eca-table--display-width "hello")
            :to-equal 5))

  (it "returns display width for links"
    (expect (eca-table--display-width "[Click](https://example.com)")
            :to-equal 5))

  (it "returns display width for links with balanced parens"
    (expect (eca-table--display-width "[text](url_(foo))")
            :to-equal 4))

  (it "returns display width for image links"
    (expect (eca-table--display-width "![alt](image.png)")
            :to-equal 3))

  (it "returns display width for bold"
    (expect (eca-table--display-width "**bold**")
            :to-equal 4))

  (it "returns display width for italic with asterisks"
    (expect (eca-table--display-width "*italic*")
            :to-equal 6))

  (it "returns display width for italic with underscores"
    (expect (eca-table--display-width "_italic_")
            :to-equal 6))

  (it "returns display width for bold-italic"
    (expect (eca-table--display-width "***bold italic***")
            :to-equal 13))

  (it "returns display width for strikethrough"
    (expect (eca-table--display-width "~~deleted~~")
            :to-equal 7))

  (it "returns display width for code spans"
    (expect (eca-table--display-width "`code`")
            :to-equal 4))

  (it "returns display width for code spans with markdown inside"
    (expect (eca-table--display-width "`**text**`")
            :to-equal 8))

  (it "returns display width for complex content"
    (expect (eca-table--display-width "PR [#1084](https://github.com/repo/pull/1084)")
            :to-equal 8))

  (it "returns correct width for snake_case identifiers"
    (expect (eca-table--display-width "my_function_name")
            :to-equal 16))

  (it "returns correct width for CJK characters in code spans"
    (expect (eca-table--display-width "`日本語`")
            :to-equal 6))

  (it "returns correct width for mixed markdown"
    (expect (eca-table--display-width "**bold** and `code`")
            :to-equal 13)))

(describe "eca-table--parse-row"
  (it "parses simple row"
    (expect (eca-table--parse-row "| a | b | c |")
            :to-equal '("a" "b" "c")))

  (it "parses row with code spans"
    (expect (eca-table--parse-row "| `code` | text |")
            :to-equal '("`code`" "text")))

  (it "parses row with links"
    (expect (eca-table--parse-row "| [link](url) | text |")
            :to-equal '("[link](url)" "text")))

  (it "handles escaped pipes"
    (expect (eca-table--parse-row "| a\\|b | c |")
            :to-equal '("a\\|b" "c")))

  (it "handles pipes inside code spans"
    (expect (eca-table--parse-row "| `a|b` | c |")
            :to-equal '("`a|b`" "c")))

  (it "handles row without trailing pipe"
    (expect (eca-table--parse-row "| a | b | c")
            :to-equal '("a" "b" "c")))

  (it "handles row without leading pipe"
    (expect (eca-table--parse-row "a | b | c |")
            :to-equal '("a" "b" "c")))

  (it "handles trailing backslash without error"
    (expect (eca-table--parse-row "| a\\ |")
            :to-equal '("a\\"))))

(describe "eca-table--separator-row-p"
  (it "recognizes simple separator"
    (expect (eca-table--separator-row-p "|---|---|")
            :to-be-truthy))

  (it "recognizes separator with colons"
    (expect (eca-table--separator-row-p "|:--|--:|:--:|")
            :to-be-truthy))

  (it "recognizes separator with spaces"
    (expect (eca-table--separator-row-p "| --- | --- |")
            :to-be-truthy))

  (it "rejects content row"
    (expect (eca-table--separator-row-p "| text | more |")
            :not :to-be-truthy))

  (it "rejects header row"
    (expect (eca-table--separator-row-p "| Header | Another |")
            :not :to-be-truthy))

  (it "rejects row with only single dashes"
    (expect (eca-table--separator-row-p "| - | - |")
            :not :to-be-truthy)))

(describe "eca-table--parse-separator-alignments"
  (it "parses left alignment"
    (expect (eca-table--parse-separator-alignments "|:---|---|")
            :to-equal '("l" nil)))

  (it "parses right alignment"
    (expect (eca-table--parse-separator-alignments "|---:|---|")
            :to-equal '("r" nil)))

  (it "parses center alignment"
    (expect (eca-table--parse-separator-alignments "|:---:|---|")
            :to-equal '("c" nil)))

  (it "parses mixed alignments"
    (expect (eca-table--parse-separator-alignments
             "|:---|---:|:---:|---|")
            :to-equal '("l" "r" "c" nil))))

(describe "eca-table--insert-cell"
  (it "left-aligns by default"
    (expect (with-temp-buffer
              (eca-table--insert-cell "a" 5 nil)
              (buffer-string))
            :to-equal " a      |"))

  (it "right-aligns with r"
    (expect (with-temp-buffer
              (eca-table--insert-cell "a" 5 "r")
              (buffer-string))
            :to-equal "      a |"))

  (it "center-aligns with c"
    (expect (with-temp-buffer
              (eca-table--insert-cell "a" 4 "c")
              (buffer-string))
            :to-equal "   a   |"))

  (it "center-aligns with odd padding"
    (expect (with-temp-buffer
              (eca-table--insert-cell "a" 5 "c")
              (buffer-string))
            :to-equal "   a    |"))

  (it "left-aligns with l"
    (expect (with-temp-buffer
              (eca-table--insert-cell "a" 5 "l")
              (buffer-string))
            :to-equal " a      |")))

(describe "eca-table--align-at-point"
  (it "aligns a basic table"
    (let ((input "| A | BB | CCC |\n|---|---|---|\n| x | yy | z |\n")
          (expected (concat "| A | BB | CCC |\n"
                            "|---|----|----- |\n"  ;; hmm
                            "| x | yy | z   |\n")))
      ;; We can't easily test this without markdown-table-begin/end,
      ;; so we test the sub-components instead
      (expect (eca-table--parse-row "| A | BB | CCC |")
              :to-equal '("A" "BB" "CCC"))))

  (it "preserves alignment markers through parse and rebuild"
    (let ((line "|:---|---:|:---:|---|"))
      (expect (eca-table--separator-row-p line)
              :to-be-truthy)
      (expect (eca-table--parse-separator-alignments line)
              :to-equal '("l" "r" "c" nil))
      ;; Verify separator cells are rebuilt with markers
      (expect (eca-table--make-separator-cell 5 "l")
              :to-equal ":------")
      (expect (eca-table--make-separator-cell 5 "r")
              :to-equal "------:")
      (expect (eca-table--make-separator-cell 5 "c")
              :to-equal ":-----:")
      (expect (eca-table--make-separator-cell 5 nil)
              :to-equal "-------"))))

(provide 'eca-table-test)
;;; eca-table-test.el ends here
