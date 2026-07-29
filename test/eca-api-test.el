;;; eca-api-test.el --- Tests for eca-api -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'eca-api)

(describe "eca-api--json-read-buffer"
  (it "returns a fresh form on each expansion"
    (expect (macroexpand '(eca-api--json-read-buffer))
            :not :to-be (macroexpand '(eca-api--json-read-buffer))))

  (it "is not poisoned by callers mutating an expansion (byte-compiled)"
    ;; When the macro function is byte-compiled, a constant expansion is
    ;; a shared literal from the .elc constants vector. Emacs 31's
    ;; `macroexp--posify-form-1' setcars expansions in place, so sharing
    ;; poisons the macro for the rest of the session (#284).
    (let* ((raw (cdr (symbol-function 'eca-api--json-read-buffer)))
           (compiled (if (byte-code-function-p raw) raw (byte-compile raw)))
           (expansion (funcall compiled)))
      (expect expansion :not :to-be (funcall compiled))
      (setcar expansion 'poisoned)
      (expect (car (funcall compiled)) :not :to-be 'poisoned)))

  (it "parses json from the current buffer as a plist"
    (with-temp-buffer
      (insert "{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":null}")
      (goto-char (point-min))
      (expect (eca-api--json-read-buffer)
              :to-equal '(:jsonrpc "2.0" :id 3 :result nil)))))

(provide 'eca-api-test)
;;; eca-api-test.el ends here
