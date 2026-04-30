;;; eca-diff-lcs.el --- Token/line diff utilities for ECA -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Token- and line-level diff utilities for ECA.
;;
;;  `eca-diff-lcs-opcodes' uses LCS with common-prefix/suffix
;;  elimination, which is O(NM) worst-case but highly optimised for the
;;  common case (single contiguous edit across a short token run).
;;  Consumers (currently `eca-completion.el') turn the opcodes into
;;  red/green inline highlights.
;;
;;  These helpers are pure data: no buffers, no overlays, no faces.
;;
;;; Code:

(require 'cl-lib)

(defun eca-diff-lcs-tokenize (s)
  "Split S into a vector of word / whitespace / single-punctuation tokens.
Word runs use `[[:alnum:]_-]+' (so hyphenated identifiers like
`foo-bar' stay one token), whitespace runs use `[[:space:]]+', and
every other character becomes its own one-character token.  The
concatenation of the resulting tokens equals S."
  (let (tokens (pos 0))
    (while (string-match "[[:alnum:]_-]+\\|[[:space:]]+\\|." s pos)
      (push (match-string 0 s) tokens)
      (setq pos (match-end 0)))
    (vconcat (nreverse tokens))))

(defun eca-diff-lcs-opcodes (a b)
  "Return diff opcodes from vector A to vector B.
Uses LCS with common-prefix/suffix elimination so the common case
\(single contiguous edit across a few tokens) is near O(N+M).
Each opcode is `(:equal ITEMS)', `(:delete ITEMS)', or `(:insert ITEMS)',
where ITEMS is a list of contiguous vector entries."
  (let* ((na (length a))
         (nb (length b))
         (prefix 0)
         (suffix 0))
    ;; ----- common prefix -----
    (while (and (< prefix na) (< prefix nb)
                (equal (aref a prefix) (aref b prefix)))
      (cl-incf prefix))
    ;; ----- common suffix (does not overlap prefix) -----
    (while (and (< suffix (- na prefix))
                (< suffix (- nb prefix))
                (equal (aref a (- na suffix 1))
                       (aref b (- nb suffix 1))))
      (cl-incf suffix))
    ;; ----- LCS on the middle strips -----
    (let* ((ma (- na prefix suffix))
           (mb (- nb prefix suffix))
           (row-len (1+ mb))
           (dp (make-vector (* (1+ ma) row-len) 0))
           a-idx b-idx)
      ;; Build LCS-length DP table (flat vector, row-major).
      ;; Use while loops instead of cl-loop for speed.
      (let ((i 1) j row-base prev-base)
        (while (<= i ma)
          (setq a-idx (+ prefix i -1)
                row-base (* i row-len)
                prev-base (* (1- i) row-len)
                j 1)
          (while (<= j mb)
            (setq b-idx (+ prefix j -1))
            (aset dp (+ row-base j)
                  (if (equal (aref a a-idx) (aref b b-idx))
                      (1+ (aref dp (+ prev-base (1- j))))
                    (max (aref dp (+ prev-base j))
                         (aref dp (+ row-base (1- j))))))
            (cl-incf j))
          (cl-incf i)))
      ;; Backtrack: push builds edits left-to-right (push reverses
      ;; the right-to-left encounter order, giving correct
      ;; within-run token order).
      (let ((i ma) (j mb) edits)
        (while (or (> i 0) (> j 0))
          (cond
           ((and (> i 0) (> j 0)
                 (equal (aref a (setq a-idx (+ prefix i -1)))
                        (aref b (setq b-idx (+ prefix j -1)))))
            (push (cons :equal (aref a a-idx)) edits)
            (cl-decf i)
            (cl-decf j))
           ((and (> i 0)
                 (or (= j 0)
                     (> (aref dp (+ (* (1- i) row-len) j))
                        (aref dp (+ (* i row-len) (1- j))))))
            (push (cons :delete (aref a (+ prefix i -1))) edits)
            (cl-decf i))
           (t
            (push (cons :insert (aref b (+ prefix j -1))) edits)
            (cl-decf j))))
        ;; Normalize edit ordering: within a contiguous run of
        ;; non-`:equal' edits, all `:delete' cells must precede all
        ;; `:insert' cells.  Downstream consumers rely on this
        ;; invariant to fold a delete-then-insert run into a
        ;; `:replace' without re-checking ordering.  Bubble any
        ;; `:insert' past following `:delete's; the run length is
        ;; tiny (LCS already stripped common prefix/suffix).
        (let (changed)
          (cl-loop do
                   (setq changed nil)
                   (let ((cur edits))
                     (while (cdr cur)
                       (let ((x (car cur)) (y (cadr cur)))
                         (when (and (eq (car x) :insert)
                                    (eq (car y) :delete))
                           (setcar cur y)
                           (setcar (cdr cur) x)
                           (setq changed t)))
                       (setq cur (cdr cur))))
                   while changed))
        ;; Insert prefix / suffix tokens at front / back.
        (let ((prefix-toks (and (> prefix 0)
                                (let ((i 0) acc)
                                  (while (< i prefix)
                                    (push (aref a i) acc)
                                    (cl-incf i))
                                  (nreverse acc))))
              (suffix-toks (and (> suffix 0)
                                (let ((i (- na suffix)) acc)
                                  (while (< i na)
                                    (push (aref a i) acc)
                                    (cl-incf i))
                                  (nreverse acc)))))
          (dolist (tok (nreverse prefix-toks))
            (push (cons :equal tok) edits))
          (dolist (tok suffix-toks)
            (setq edits (nconc edits (list (cons :equal tok))))))
        ;; Group consecutive same-op items.
        (let (out)
          (dolist (e edits)
            (let ((op (car e)) (item (cdr e)))
              (if (eq op (car-safe (car out)))
                  (push item (cadr (car out)))
                (push (list op (list item)) out))))
          (mapcar (lambda (h) (list (car h) (nreverse (cadr h))))
                  (nreverse out)))))))

(defun eca-diff-lcs-diff (a b)
  "Return diff hunks from token vector A to B using `eca-diff-lcs-opcodes'.
Each hunk is `(:equal STR)', `(:delete STR)', or `(:insert STR)' where
STR is the concatenation of the contiguous run of tokens."
  (mapcar (lambda (h)
            (list (car h) (mapconcat #'identity (cadr h) "")))
          (eca-diff-lcs-opcodes a b)))

(provide 'eca-diff-lcs)
;;; eca-diff-lcs.el ends here
