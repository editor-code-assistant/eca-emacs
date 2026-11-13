;;; eca-table-test.el --- Tests for eca-table -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'buttercup)
(require 'eca-table)

;; Ensure the font-lock buffer is configured for tests.
(defvar eca-chat-parent-mode 'gfm-mode)

(defun eca-table-test--align (text)
  "Return TEXT after `eca-table-align' ran over it in a gfm-mode buffer."
  (with-temp-buffer
    (gfm-mode)
    (setq-local markdown-hide-markup t)
    (insert text)
    (eca-table-align (point-min) (point-max))
    (buffer-string)))

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

  (it "can keep markup visible in display width calculations"
    (let ((eca-chat-hide-markdown-markup nil))
      (expect (eca-table--display-width "**bold**")
              :to-equal 8)))

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

  (it "handles code spans opened by several backticks"
    (expect (eca-table--parse-row "| a | `` ```md `` b | c |")
            :to-equal '("a" "`` ```md `` b" "c")))

  (it "handles pipes inside double-backtick code spans"
    (expect (eca-table--parse-row "| `` a | b `` | c |")
            :to-equal '("`` a | b ``" "c")))

  (it "handles longer backtick runs inside a code span"
    (expect (eca-table--parse-row "| `a``b` | c |")
            :to-equal '("`a``b`" "c")))

  (it "treats an unclosed backtick as literal text"
    (expect (eca-table--parse-row "| a | `b | c |")
            :to-equal '("a" "`b" "c")))

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
    (expect (eca-table-test--align
             "| A | BB | CCC |\n|---|---|---|\n| x | yy | z |\n")
            :to-equal (concat "| A | BB | CCC |\n"
                              "|---|----|-----|\n"
                              "| x | yy | z   |\n")))

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

(describe "eca-table-align"
  (it "aligns every table in the range after earlier ones grow"
    ;; Aligning the first table pushes the second one past the
    ;; original END position.
    (let ((result (eca-table-test--align
                   (concat "|a|b|\n|---|---|\n|cc|a much longer cell here|\n"
                           "\n"
                           "|e|f|\n|---|---|\n|g|h|\n"))))
      (expect result
              :to-match (regexp-quote "| cc | a much longer cell here |\n"))
      (expect result
              :to-match (regexp-quote "| e | f |\n|---|---|\n| g | h |\n"))))

  (it "aligns read-only text without signaling"
    (with-temp-buffer
      (gfm-mode)
      (setq-local markdown-hide-markup t)
      (insert "|a|b|\n|---|---|\n|c|d|\n")
      (add-text-properties (point-min) (point-max) '(read-only t))
      (setq buffer-read-only t)
      (eca-table-align (point-min) (point-max))
      (expect (buffer-string)
              :to-equal "| a | b |\n|---|---|\n| c | d |\n")))

  (it "is idempotent for rows with multi-backtick code spans"
    (let* ((input (concat "| Mode | Type | Function |\n|---|---|---|\n"
                          "| a | `` ```markdown `` | `a\\|b` |\n"))
           (once (eca-table-test--align input))
           (twice (eca-table-test--align once))
           (lines (split-string once "\n" t)))
      (expect twice :to-equal once)
      (expect (length lines) :to-equal 3)
      (expect (length (eca-table--parse-row (nth 0 lines))) :to-equal 3)
      (expect (length (eca-table--parse-row (nth 2 lines))) :to-equal 3)
      (expect (nth 2 lines)
              :to-match (regexp-quote "`` ```markdown `` | `a\\|b`")))))

(describe "eca-table-align width measure"
  (it "pads by source width when markdown-mode aligns separators"
    (spy-on 'eca-table--markdown-aligns-p :and-return-value t)
    (expect (eca-table-test--align
             (concat "| Language | Hello World |\n|---|---|\n"
                     "| Python | `print(1)` |\n| C | printf |\n"))
            :to-equal (concat "| Language | Hello World |\n"
                              "|----------|-------------|\n"
                              "| Python   | `print(1)`  |\n"
                              "| C        | printf      |\n")))

  (it "pads by display width when markdown-mode does not align"
    (spy-on 'eca-table--markdown-aligns-p :and-return-value nil)
    (let* ((result (eca-table-test--align
                    (concat "| Language | Hello World |\n|---|---|\n"
                            "| Python | `print(1)` |\n| C | printf |\n")))
           (lines (split-string result "\n" t)))
      ;; The two hidden backticks are compensated with two extra
      ;; spaces, so every line has the same display width.
      (expect (nth 2 lines) :to-equal "| Python   | `print(1)`    |")
      (expect (length (seq-uniq (mapcar #'eca-table--display-width lines)))
              :to-equal 1)))

  (it "counts emoji sequences by their source width"
    (spy-on 'eca-table--markdown-aligns-p :and-return-value t)
    ;; Emoji have `char-width' 2 and ZWJ 0, so the family is 6
    ;; columns wide in the source, wider than the "Emoji" header.
    (let* ((family "\U0001F468\u200D\U0001F469\u200D\U0001F467")
           (wave "\U0001F44B")
           (result (eca-table-test--align
                    (concat "| Category | Emoji |\n|---|---|\n"
                            "| Family | " family " |\n"
                            "| Wave | " wave " |\n")))
           (lines (split-string result "\n" t)))
      (expect (nth 2 lines) :to-equal (concat "| Family   | " family " |"))
      (expect (nth 3 lines) :to-equal (concat "| Wave     | " wave "     |"))
      (expect (length (seq-uniq (mapcar #'eca-table--source-width lines)))
              :to-equal 1))))

(describe "eca-table--apply-markdown-markup-visibility"
  (it "adds markdown-markup to the invisibility spec only once"
    (with-temp-buffer
      (let ((eca-chat-hide-markdown-markup t))
        (dotimes (_ 3)
          (eca-table--apply-markdown-markup-visibility)))
      (expect (cl-count 'markdown-markup buffer-invisibility-spec)
              :to-equal 1)))

  (it "removes markdown-markup when markup should stay visible"
    (with-temp-buffer
      (let ((eca-chat-hide-markdown-markup t))
        (eca-table--apply-markdown-markup-visibility))
      (let ((eca-chat-hide-markdown-markup nil))
        (eca-table--apply-markdown-markup-visibility))
      (expect (memq 'markdown-markup buffer-invisibility-spec)
              :to-be nil)))

  (it "keeps the measurement buffer invisibility spec from growing"
    (eca-table--display-width "x")
    (let ((before (buffer-local-value 'buffer-invisibility-spec
                                      (eca-table--get-fontlock-buffer))))
      (dotimes (_ 5)
        (eca-table--display-width "**x**"))
      (expect (buffer-local-value 'buffer-invisibility-spec
                                  (eca-table--get-fontlock-buffer))
              :to-equal before))))

(describe "eca-table-open"
  (it "opens the table at point in a dedicated truncate-lines buffer"
    (when (get-buffer "*eca-table*") (kill-buffer "*eca-table*"))
    (with-temp-buffer
      (gfm-mode)
      (setq-local markdown-hide-markup t)
      (insert "| Setting | What it does |\n")
      (insert "|---|---|\n")
      (insert "| alpha | does alpha things |\n")
      (goto-char (point-min))
      (forward-line 2)
      (eca-table-open))
    (let ((buf (get-buffer "*eca-table*")))
      (expect buf :to-be-truthy)
      (with-current-buffer buf
        (expect truncate-lines :to-be-truthy)
        (expect (> (buffer-size) 0) :to-be-truthy)
        (expect (string-match-p "Setting" (buffer-string)) :to-be-truthy))
      (kill-buffer buf)))

  (it "can keep markdown markup visible in the table buffer"
    (when (get-buffer "*eca-table*") (kill-buffer "*eca-table*"))
    (let ((eca-chat-hide-markdown-markup nil))
      (with-temp-buffer
        (gfm-mode)
        (setq-local markdown-hide-markup t)
        (insert "| Setting | What it does |\n")
        (insert "|---|---|\n")
        (insert "| alpha | **bold** things |\n")
        (goto-char (point-min))
        (forward-line 2)
        (eca-table-open)))
    (let ((buf (get-buffer "*eca-table*")))
      (expect buf :to-be-truthy)
      (with-current-buffer buf
        (expect (memq 'markdown-markup buffer-invisibility-spec)
                :to-be nil))
      (kill-buffer buf)))

  (it "errors when point is not on a table"
    (with-temp-buffer
      (gfm-mode)
      (insert "not a table\n")
      (goto-char (point-min))
      (expect (eca-table-open) :to-throw 'user-error))))

(describe "eca-table-open keybinding"
  (it "binds o to eca-table-open while point is on a table"
    (let ((eca-chat-table-beautify t))
      (with-temp-buffer
        (gfm-mode)
        (setq-local markdown-hide-markup t)
        (insert "| A | B |\n|---|---|\n| x | y |\n")
        (eca-table-beautify (point-min) (point-max))
        (goto-char (point-min))
        (forward-line 2)
        (expect (key-binding (kbd "o") nil nil (point))
                :to-equal 'eca-table-open))))

  (it "leaves o untouched away from any table"
    (let ((eca-chat-table-beautify t))
      (with-temp-buffer
        (gfm-mode)
        (insert "just some prose\n")
        (goto-char (point-min))
        (expect (key-binding (kbd "o") nil nil (point))
                :not :to-equal 'eca-table-open)))))

(defun eca-table-test--table (rows)
  "Return a markdown table of ROWS rows with inline markup."
  (concat "Intro **text**.\n\n"
          "| Name | Code | Note |\n|---|---|---|\n"
          (mapconcat (lambda (r)
                       (format "| **item %d** | `a | b` | _note_ [l](http://x/%d) |"
                               r r))
                     (number-sequence 1 rows) "\n")
          "\n\nAfter the `table`.\n"))

(defmacro eca-table-test--with-table (rows fast &rest body)
  "Run BODY in a gfm buffer holding a table of ROWS rows.
Tables are fontified row by row when FAST is non-nil."
  (declare (indent 2))
  `(with-temp-buffer
     (cl-letf (((symbol-function 'eca-table--fast-fontify-p)
                (lambda () ,fast)))
       (gfm-mode)
       (setq-local markdown-hide-markup t)
       (insert (eca-table-test--table ,rows))
       ,@body)))

(defun eca-table-test--rendering ()
  "Return the face, display and invisibility runs of the buffer."
  (let (runs (pos (point-min)))
    (while (< pos (point-max))
      (push (list pos
                  (get-text-property pos 'face)
                  (get-text-property pos 'display)
                  (get-text-property pos 'invisible))
            runs)
      (setq pos (1+ pos)))
    (nreverse runs)))

(describe "eca-table row by row fontification"
  (it "renders tables the same as markdown-mode"
    (let ((slow (eca-table-test--with-table 8 nil
                  (font-lock-ensure)
                  (eca-table-test--rendering)))
          (fast (eca-table-test--with-table 8 t
                  (font-lock-ensure)
                  (eca-table-test--rendering))))
      (expect fast :to-equal slow)))

  (it "renders the same when fontifying a few rows at a time"
    (let ((slow (eca-table-test--with-table 8 nil
                  (font-lock-ensure)
                  (eca-table-test--rendering)))
          (fast (eca-table-test--with-table 8 t
                  (let ((pos (point-min)))
                    (while (< pos (point-max))
                      (font-lock-fontify-region pos (min (point-max) (+ pos 60)))
                      (setq pos (+ pos 60))))
                  (eca-table-test--rendering))))
      (expect fast :to-equal slow)))

  (it "finds code spans holding pipes within their row"
    (eca-table-test--with-table 3 t
      (goto-char (point-min))
      (search-forward "`a | b`")
      (expect (markdown-inline-code-at-pos-p (- (point) 4)) :to-be-truthy)
      (search-forward "| _note")
      (expect (markdown-inline-code-at-pos-p (- (point) 6)) :not :to-be-truthy)))

  (it "does not flag table rows as continuing on the next one"
    (eca-table-test--with-table 20 t
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "| Name")
      (let ((end (save-excursion (search-forward "After") (point)))
            flagged)
        (while (< (line-end-position) end)
          (when (get-text-property (line-end-position) 'font-lock-multiline)
            (push (line-number-at-pos) flagged))
          (forward-line 1))
        (expect flagged :to-be nil))))

  (it "extends regions inside a table by whole lines only"
    (eca-table-test--with-table 20 t
      (goto-char (point-min))
      (search-forward "item 10")
      (let* ((beg (line-beginning-position))
             (end (1+ (line-end-position)))
             (res (markdown-syntax-propertize-extend-region (+ beg 3) (1- end))))
        (expect res :to-equal (cons beg end)))))

  (it "keeps fontification of a table chunk bounded"
    (eca-table-test--with-table 200 t
      (let (sizes)
        (font-lock-ensure)
        (cl-letf* ((orig (symbol-function 'font-lock-fontify-keywords-region))
                   ((symbol-function 'font-lock-fontify-keywords-region)
                    (lambda (beg end &rest args)
                      (push (- end beg) sizes)
                      (apply orig beg end args))))
          (goto-char (point-min))
          (search-forward "item 100")
          (font-lock-fontify-region (point) (+ (point) 500)))
        (expect (apply #'max sizes) :to-be-less-than 1000))))

  (it "leaves tables outside chat buffers to markdown-mode"
    (with-temp-buffer
      (gfm-mode)
      (insert (eca-table-test--table 20))
      (goto-char (point-min))
      (search-forward "item 10")
      (expect (eca-table--fast-fontify-p) :not :to-be-truthy)
      (let ((res (markdown-syntax-propertize-extend-region
                  (point) (1+ (point)))))
        (expect (car res) :to-be-less-than (line-beginning-position)))))

  (it "calls a markdown-inline-code-at-pos taking POS only"
    ;; markdown-mode before 2.7 defines it without FROM: passing FROM
    ;; broke the fontification of all markdown buffers.
    (let ((orig (lambda (pos) (list :pos pos))))
      (with-temp-buffer
        (gfm-mode)
        (insert "Some `code` here.\n")
        (expect (eca-table--inline-code-at-pos-a orig 7)
                :to-equal '(:pos 7)))
      (eca-table-test--with-table 3 t
        (goto-char (point-min))
        (search-forward "item 2")
        (expect (eca-table--inline-code-at-pos-a orig (point))
                :to-equal (list :pos (point))))))

  (it "passes FROM on to markdown-inline-code-at-pos"
    (with-temp-buffer
      (gfm-mode)
      (insert "Some `code` here.\n")
      (expect (eca-table--inline-code-at-pos-a
               (lambda (pos &optional from) (list pos from)) 7 1)
              :to-equal '(7 1)))))

(provide 'eca-table-test)
;;; eca-table-test.el ends here
