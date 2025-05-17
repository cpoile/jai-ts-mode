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

;; NOTE: still working on these, correcting them as I encounter annoyances.
(defvar jai-ts-mode--indent-rules
  `((jai
     ;;((parent-is "source_file") column-0 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ;;((parent-is "raw_string_literal") no-indent 0)
     ((parent-is "block") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "struct_literal") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "if_case_statement") parent-bol 0)  ;; Jai's style is no indent on cases?
     ((parent-is "if_statement") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "argument_list") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "communication_case") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "const_declaration") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "default_case") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "expression_case") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "selector_expression") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "expression_switch_statement") parent-bol 0)
     ;; ((parent-is "field_declaration_list") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "import_spec_list") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "interface_type") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "labeled_statement") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "literal_value") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "named_parameters") parent-bol jai-ts-mode-indent-offset)
     ;;((parent-is "assignment_parameters") parent-bol jai-ts-mode-indent-offset)
     ((match nil "assignment_parameters" nil 1 1) standalone-parent jai-ts-mode-indent-offset)
     ((match ")" "assignment_parameters" nil nil nil) standalone-parent 0)
     ((match nil "assignment_parameters" nil 2 nil) (nth-sibling 1) 0)
     ;; ((parent-is "select_statement") parent-bol 0)
     ;; ((parent-is "type_case") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "type_spec") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "type_switch_statement") parent-bol 0)
     ((parent-is "variable_declaration") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "struct_declaration") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "enum_declaration") parent-bol jai-ts-mode-indent-offset)
     ;; ((match nil "variable_declaration" nil 1 4) standalone-parent jai-ts-mode-indent-offset)
     ;; ((match ")" "variable_declaration" nil nil nil) standalone-parent 0)
     ;; ((match nil "variable_declaration" nil 2 nil) (nth-sibling 1) 0)
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
    '(
      (load (compiler_directive) @font-lock-preprocessor-face)
      (import (compiler_directive) @font-lock-preprocessor-face)
      (run_statement (compiler_directive) @font-lock-preprocessor-face)
      (compiler_directive [("#") (identifier)] @font-lock-preprocessor-face)
      (modify_block (compiler_directive) @font-lock-preprocessor-face)
      (run_or_insert_expression (compiler_directive) @font-lock-preprocessor-face)
      (static_if_statement (compiler_directive) @font-lock-preprocessor-face)
      (asm_statement (compiler_directive) @font-lock-preprocessor-face)
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
      (struct_declaration (identifier) @font-lock-keyword-face)
      (procedure_declaration modifier: ("inline") @font-lock-keyword-face)
      (enum_declaration [("enum_flags") ("enum")] @font-lock-keyword-face)
      (struct_or_union ("struct") @font-lock-keyword-face)
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
      (member_expression (postfix_dereference) @font-lock-operator-face))

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
    "enum_declaration")
  "List of tree-sitter node types considered as defuns in Jai mode.")

(defun jai-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ("procedure_declaration"
     (treesit-node-text
      (treesit-node-child-by-field-name
       node "name")
      t))
    ("struct_declaration"
     (treesit-node-text
      (treesit-node-child-by-field-name
       node "name")
      t))
    ("enum_declaration"
     (treesit-node-text
      (treesit-node-child-by-field-name
       node "name")
      t))))

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
    ;;(setq-local treesit-defun-name-function #'jai-ts-mode--defun-name)
    (setq-local treesit-defun-name-function nil)

    ;; TODO: these don't seem to do anything, fix? I don't think you can
    ;; setq-local to redefine a function. So instead, when I need this to be
    ;; called (e.g. within mc/mark-all-symbols-like-this-in-defun), I have my
    ;; own version of mark-all-symbols... which calls
    ;; jai-ts-mode--narrow-to-defun instead of narrow-to-defun.
    ;;
    ;; (setq-local narrow-to-defun #'jai-ts-mode--narrow-to-defun)


    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("proc" "\\`procedure_declaration\\'" nil jai-ts-mode--defun-name)
                  ("struct" "\\`struct_declaration\\'" nil jai-ts-mode--defun-name)
                  ("enum" "\\`enum_declaration\\'" nil jai-ts-mode--defun-name)))

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
    
    ;; If you need a custom skipper function:
    ;; (setq-local treesit-defun-skipper #'my-jai-defun-skipper)

    ;; TODO: nochekin remove
    ;; Tell Emacs to use the standard tree-sitter functions
    ;; (setq-local beginning-of-defun-function #'treesit-beginning-of-defun)
    ;; (setq-local end-of-defun-function #'treesit-end-of-defun)
    
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

(defun jai-ts-mode--align-struct ()
  "Align structs, right in the colon.
If there's a region active, align that. If there's a `:=' then
align with the := at the end, not at the beginning."
  (interactive)
  (let ((cmd (if (and (region-active-p)
                  (string-match-p (regexp-quote ":=") (buffer-substring (region-beginning) (region-end))))
                '(align-regexp (region-beginning) (region-end) "\\(\\s-*\\):+=?\\(\\s-*\\)" 1 1)
              '(align-regexp (region-beginning) (region-end) ":+\\(\\s-*\\)" 1 1))))
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
