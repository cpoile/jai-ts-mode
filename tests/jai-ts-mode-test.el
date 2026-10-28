;;; jai-ts-mode-test.el --- Regression checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'compile)
(require 'seq)
(require 'jai-ts-mode)

(ert-deftest jai-ts-mode-navigation-at-end-of-buffer ()
  (skip-unless (treesit-language-available-p 'jai))
  (dolist (suffix '("" "\n"))
    (with-temp-buffer
      (insert "first :: () {\n    return;\n}\nsecond :: () {\n    return;\n}" suffix)
      (jai-ts-mode)
      (goto-char (point-max))
      (jai-ts-mode--prev-defun)
      (should (looking-at "second"))
      (jai-ts-mode--prev-defun)
      (should (looking-at "first")))))

(ert-deftest jai-ts-mode-navigation-in-empty-buffer ()
  (skip-unless (treesit-language-available-p 'jai))
  (with-temp-buffer
    (jai-ts-mode)
    (should-not (jai-ts-mode--find-enclosing-defun))))

(ert-deftest jai-ts-mode-punctuation-query-compiles ()
  (skip-unless (treesit-language-available-p 'jai))
  (with-temp-buffer
    (insert "main :: () {\n    value := 1;\n}\n")
    (jai-ts-mode)
    (let ((rule (seq-find (lambda (rule) (eq (nth 2 rule) 'punctuation))
                          jai-ts-mode--font-lock-rules)))
      (should rule)
      (should (treesit-query-capture (treesit-buffer-root-node 'jai) (car rule))))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "{")
    (should (eq (get-text-property (1- (point)) 'face)
                'font-lock-punctuation-face))))

(provide 'jai-ts-mode-test)
;;; jai-ts-mode-test.el ends here
