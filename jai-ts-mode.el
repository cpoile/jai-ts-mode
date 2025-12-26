;;; jai-ts-mode.el --- Jai Lang Major Mode for Emacs -*- lexical-binding: t -*-

;; TODO: fix all this preamble
;; Author: Christopher Poile
;; URL: https://github.com/cpoile/jai-ts-mode
;; Keywords: jai languages tree-sitter
;; Version 0.1.0
;; Package-Requires : ((emacs "29.1"))

;;; License:

;; MIT License
;;
;; Copyright (c) 2025 Christopher Poile
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Powered by Emacs >= 29 and tree-sitter this major mode provides
;; syntax highlighting, indentation and imenu support for Jai.
;; jai-ts-mode is built against the tree-sitter grammar locatated at
;; https://github.com/constantitus/tree-sitter-jai

;; Much of the structure of this code is based on valignatev's jai-mode (thank you!)
;; https://github.com/valignatev/jai-mode/blob/master/jai-mode.el
;; And the odin-ts-mode by Sampie159 (thank you!)
;; https://github.com/Sampie159/odin-ts-mode

;;; Code:

(require 'treesit)

(defgroup jai-ts nil
  "Major mode for editing jai files."
  :prefix "jai-ts-"
  :group 'languages)

(defcustom jai-ts-mode-hook nil
  "Hook run after entering `jai-ts-mode`."
  :version "29.1"
  :type 'symbol
  :group 'jai)

(defcustom jai-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `go-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'jai)

(defconst jai-ts-mode--syntax-table ;; shamelessly stolen directly from jai-mode
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; additional symbols
    (modify-syntax-entry ?_ "w" table)

    ;; NOTE:  Make _ not part of word syntax (i.e., treat _ as a subword boundary) by adding this to your init.el:
    ;;(modify-syntax-entry ?_ "_" jai-ts-mode--syntax-table)

    (modify-syntax-entry ?' "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?^  "." table)
    (modify-syntax-entry ?!  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ??  "." table)

    ;; Modify some syntax entries to allow nested block comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table)
  "Syntax table for `jai-ts-mode`.")

(defun jai-ts-mode--type-name (node)
  "Return the name of NODE with type face applied."
  (let ((name (treesit-node-text
               (treesit-node-child-by-field-name node "name")
               t)))
    (propertize name 'face 'font-lock-type-face)))

(defun jai-ts-mode--proc-signature (node)
  "Return the full signature for a procedure NODE.
Returns a string like `name :: (arg: type) -> return_type`."
  (let* ((name (treesit-node-text
                (treesit-node-child-by-field-name node "name")
                t))
         (proc (treesit-search-subtree node "procedure"))
         (params (or (treesit-search-subtree proc "named_parameters")
                     (treesit-search-subtree proc "assignment_parameters")))
         (returns (treesit-search-subtree proc "procedure_returns")))
    (concat (propertize name 'face 'font-lock-function-name-face)
            (if params
                (concat " :: " (treesit-node-text params t))
              " :: ()")
            (when returns
              (concat " -> " (treesit-node-text returns t))))))

(defcustom jai-ts-mode-imenu-settings
  `((nil "\\`procedure_declaration\\'" nil jai-ts-mode--proc-signature)
    (nil "\\`struct_declaration\\'" nil jai-ts-mode--type-name)
    (nil "\\`enum_declaration\\'" nil jai-ts-mode--type-name)
    (nil "\\`const_declaration\\'" nil jai-ts-mode--type-name))
  "Imenu settings for `jai-ts-mode`.
Reorder this list to change how categories appear in imenu.
Use nil instead of a string for flat (ungrouped) display sorted by position.
Each entry is (CATEGORY REGEXP PRED NAME-FN)."
  :type '(repeat (list (choice (const nil) string) string (choice (const nil) function) function))
  :group 'jai-ts)

;; NOTE: still working on these, correcting them as I encounter annoyances.
(defvar jai-ts-mode--indent-rules
  `((jai
     ;;((parent-is "source_file") column-0 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((parent-is "block") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "struct_literal") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "if_case_statement") parent-bol 0)  ;; Jai's style is no indent on cases?
     ((parent-is "switch_case") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "if_statement") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "named_parameters") parent-bol jai-ts-mode-indent-offset)
     ((match nil "assignment_parameters" nil 1 1) standalone-parent jai-ts-mode-indent-offset)
     ((match ")" "assignment_parameters" nil nil nil) standalone-parent 0)
     ((match nil "assignment_parameters" nil 2 nil) (nth-sibling 1) 0)
     ((parent-is "variable_declaration") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "struct_declaration") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "enum_declaration") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "const_declaration") parent-bol jai-ts-mode-indent-offset)
     (no-node parent-bol 0)))
  "Tree-sitter indent rules for `jai-ts-mode'.")

(defvar jai-ts-mode--font-lock-rules
  (treesit-font-lock-rules
    :language 'jai
    :feature 'comment
    '([(comment) (block_comment)] @font-lock-comment-face
      (static_if_statement
       (if_statement_condition_and_consequence
        (if_condition (boolean "false"))
        consequence:
        (statement
         (block
          ((statement) @font-lock-comment-face)))))
      (static_if_statement
       (if_statement_condition_and_consequence
        (if_condition (boolean "true"))
        consequence:
        (_)
        alternative:
        (else_clause
         (statement
          (block
           ((statement) @font-lock-comment-face)))))))

    :language 'jai
    :feature 'string
    '((string) @font-lock-string-face)

    :language 'jai
    :feature 'type
    `((types (identifier) @font-lock-type-face)
      (array_type (identifier) @font-lock-type-face)
      (identifier_type (identifier) @font-lock-type-face)
      (struct_declaration name: (identifier) @font-lock-type-face)
      (struct_literal type: (identifier) @font-lock-type-face)
      (const_declaration name: (identifier) @font-lock-type-face)
      (enum_declaration name: (identifier) @font-lock-type-face)
      (member_type (identifier) @font-lock-type-face))

    :language 'jai
    :feature 'preprocessor
    '(((compiler_directive) @font-lock-preprocessor-face)
      (string_directive ("#string") @font-lock-preprocessor-face)
      (type_literal ("#type") @font-lock-preprocessor-face))

    :language 'jai
    :feature 'keyword
    '((return_statement ("return") @font-lock-keyword-face)
      (continue_statement ("continue") @font-lock-keyword-face)
      (for_statement ("for") @font-lock-keyword-face)
      (while_statement ("while") @font-lock-keyword-face)
      (using_statement ("using") @font-lock-keyword-face)
      (break_statement ("break") @font-lock-keyword-face)
      (defer_statement ("defer") @font-lock-keyword-face)
      (switch_case ("case") @font-lock-keyword-face)
      (cast_expression ("cast") @font-lock-keyword-face)
      (procedure_declaration modifier: ("inline") @font-lock-keyword-face)
      (enum_declaration [("enum_flags") ("enum")] @font-lock-keyword-face)
      (struct_or_union [("struct") ("union")] @font-lock-keyword-face)
      (if_statement [("if")] @font-lock-keyword-face)

      ;; NOTE: I personally like to have these highlighted specifically so they're clear in the code
      ;;       similar to how Jon has it in his scheme.  But you can change as you like.
      (if_statement_condition_and_consequence [("then") ("else")] @font-lock-operator-face)
      (else_clause ("else") @font-lock-operator-face)
      (if_expression ("ifx") @font-lock-keyword-face)
      (if_expression [("then") ("else")] @font-lock-operator-face)
      (auto_cast_expression ("xx") @font-lock-operator-face))

    :language 'jai
    :feature 'function
    '((procedure_declaration name: [(identifier) ("operator")] @font-lock-function-name-face)
     (call_expression function: (identifier) @font-lock-function-call-face))

    :language 'jai
    :feature 'pointer-operator
    '((pointer_type ("*") @font-lock-operator-face)
      (address ("*") @font-lock-operator-face)
      (pointer_type ("*") @font-lock-operator-face)
      (pointer_expression operator: ("<<") @font-lock-operator-face)
      (member_expression (postfix_dereference) @font-lock-operator-face)
      (for_statement ("*") @font-lock-operator-face))

    :language 'jai
    :feature 'operator
    '((update_statement ("+=") @font-lock-operator-face)
      (assignment_statement ("=") @font-lock-operator-face)
      (binary_expression [("==") ("+") ("+=") ("-") ("-=") (">=")
                          (">") ("<=") ("<") ("/") ("!=")] @font-lock-operator-face)
      (variable_declaration [(":") ("=")] @font-lock-operator-face)
      (procedure_declaration (":") @font-lock-operator-face)
      (const_declaration (":") @font-lock-operator-face)
      (for_statement [(",") (":")] @font-lock-operator-face))

    :language 'jai
    :feature 'punctuation
    '((statement (";") @font-lock-punctuation-face)
      (assignment_parameters [("(") (")")] @font-lock-punctuation-face)
      (named_parameters [("(") (")")] @font-lock-punctuation-face)
      (cast_expression [("(") (")")] @font-lock-punctuation-face)
      (parenthesized_expression [("(") (")")] @font-lock-punctuation-face)
      (array_type [("[") ("]") ("..")] @font-lock-punctuation-face)
      (index_expression [("[") ("]")] @font-lock-punctuation-face)
      (block [("{") ("}")] @font-lock-punctuation-face)
      (struct_or_union_block [("{") ("}")] @font-lock-punctuation-face)
      (struct_literal [("{") ("}")] @font-lock-punctuation-face)
      (assignment_parameters [(",")] @font-lock-punctuation-face))

    :language 'jai
    :feature 'number
    '([(float)
       (integer)] @font-lock-number-face)

    :language 'jai
    :feature 'constant
    '(([(boolean) (null)] @font-lock-constant-face))))

(defconst jai-ts-mode--defun-function-type-list
  '("procedure_declaration"
    "struct_declaration"
    "enum_declaration"
    ;; NOTE: not using const_declaration so a constant doesn't become a topsy
    ;; header and block the enclosing function's name in the topsy header.
    ;; Uncomment if you want to have consts be a target for next/prev-defun
    ;; "const_declaration"
    )
  "List of tree-sitter node types considered as defuns in Jai mode.")

;;;###autoload
(define-derived-mode jai-ts-mode prog-mode "jai"
  "Major mode for editing jai files, powered by tree-sitter."
  :group 'jai
  :syntax-table jai-ts-mode--syntax-table

  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'jai)
    (treesit-parser-create 'jai)

    ;; Comments.
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx "//" (* (syntax whitespace))))

    ;; FIXME: this breaks which-fun:
    ;;(setq-local treesit-defun-name-function #'jai-ts-mode--proc-name)
    (setq-local treesit-defun-name-function nil)

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings jai-ts-mode-imenu-settings)

    ;; Indent.
    (setq-local indent-tabs-mode nil
                treesit-simple-indent-rules jai-ts-mode--indent-rules
                which-func-functions nil)

    ;; Electric
    (setq-local electric-indent-chars
                (append "{}()" electric-indent-chars))

      ;; Font-lock
    (setq-local treesit-font-lock-settings jai-ts-mode--font-lock-rules)
    (setq-local treesit-font-lock-feature-list
                '((comment string)
                  (type keyword number constant pointer-operator)
                  (preprocessor function punctuation))) ;; add operator here if you want those highlighted

    ;; Navigation
    (setq-local treesit-defun-type-regexp (regexp-opt jai-ts-mode--defun-function-type-list 'string))

    (treesit-major-mode-setup)))

;;;###autoload
(when (treesit-ready-p 'jai)
  (add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-ts-mode)))

;; to reset to nothing:
;;(unload-feature 'jai-ts-mode t)

(defconst jai-ts-mode-error-regexp
  "^\\([^ \n:]+.*\.jai\\):\\([0-9]+\\),\\([0-9]+\\):")
(push `(jai ,jai-ts-mode-error-regexp 1 2 3 2) compilation-error-regexp-alist-alist)
(push 'jai compilation-error-regexp-alist)

(defun jai-ts-mode--matching-brace ()
  "Jump to matching { or (."
  (interactive)
  (push-mark)
  (if (or (= (char-before) ?\}) (= (char-before) ?\)))
      (backward-sexp)
    (if (or (= (char-after) ?\{) (= (char-after) ?\())
        (forward-sexp)
      (if (or (= (char-before) ?\{) (= (char-before) ?\())
          (progn (backward-char)
                 (forward-sexp))
        (backward-up-list)))))

(defun jai-ts-mode--every-line-has-symbol (beg end symbol)
  "Check if every line in the region defined by BEG and END contains symbol."
  (interactive "r")
  (let ((result t)
        (current-line-has-symbol nil))
    (save-excursion
      (goto-char beg)
      (while (and result (< (point) end))
        (setq current-line-has-symbol (string-match-p symbol (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (unless current-line-has-symbol
          (setq result nil))
        (forward-line 1)))
    result))

(defun jai-ts-mode--align-struct ()
  "Align structs, right in the colon.
If there's a region active, align that. If there's a `:=' then
align with the := at the end, not at the beginning."
  (interactive)
  (let ((cmd '(cond ((eq t (jai-ts-mode--every-line-has-symbol (region-beginning) (region-end) "::"))
                    (align-regexp (region-beginning) (region-end) "\\(\\s-*\\):"))
                   ((string-match-p (regexp-quote ":=") (buffer-substring (region-beginning) (region-end)))
                    (align-regexp (region-beginning) (region-end) "\\(\\s-*\\):+=?\\(\\s-*\\)" 1 1))
                   (t
                    (align-regexp (region-beginning) (region-end) ":+\\(\\s-*\\)" 1 1)))))
    (if (region-active-p)
        (eval cmd)
      (save-excursion
        (jai-ts-mode--matching-brace)
        (forward-line 1)
        (let ((begin (point)))
          (jai-ts-mode--matching-brace)
          (jai-ts-mode--matching-brace)
          (beginning-of-line)
          (set-mark (point))
          (goto-char begin)
          (eval cmd)
          (deactivate-mark))))))

(defun jai-ts-mode--prev-defun (&optional arg)
  "Wrap treesit-beginning-of-defun.
ARG will be passed through (for going forwards)."
  (interactive "P")
  (treesit-beginning-of-defun arg)
  (back-to-indentation))

(defun jai-ts-mode--next-defun ()
  "Go to next proc."
  (interactive)
  (jai-ts-mode--prev-defun -1))

;;
;; Example bindings:
;;
;; (map! :map jai-ts-mode-map
;;       "C-M-a" #'jai-ts-mode--prev-defun
;;       "C-M-e" #'jai-ts-mode--next-defun
;;       "C-M-l" #'align-regexp
;;       "C-M-S-l" #'jai-ts-mode--align-struct)


(provide 'jai-ts-mode)

;;; jai-ts-mode.el ends here
