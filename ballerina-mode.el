;;; ballerina-mode.el --- A major mode for editing ballerina source code -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Heshan Padmasiri
;;
;; Author: Heshan Padmasiri <hpheshan@gmail.com>
;; Maintainer: Heshan Padmasiri <hpheshan@gmail.com>
;; Created: July 22, 2023
;; Modified: July 22, 2023
;; Version: 0.0.1
;; Keywords: ballerina languages
;; Homepage: https://github.com/heshanpadmasiri/ballerina-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file implements a major mode for editing ballerina code
;;
;;; Code:

(defconst ballerina-mode-basic-types
  '("decimal"
    "float"
    "int"
    "map"
    "object"
    "record"
    "string"
    "table"
    "table") "Basic types in ballerina language used for font locking.")

(defconst ballerina-mode-keywords
  '("ascending"
    "break"
    "by"
    "check"
    "checkpanic"
    "class"
    "client"
    "conflict"
    "const"
    "descending"
    "distinct"
    "else"
    "equals"
    "final"
    "foreach"
    "from"
    "function"
    "if"
    "import"
    "in"
    "isolated"
    "join"
    "let"
    "limit"
    "on"
    "order"
    "outer"
    "private"
    "public"
    "readonly"
    "remote"
    "return"
    "returns"
    "select"
    "service"
    "type"
    "var"
    "where"
    "while")  "All keywords in the ballerina language.  Used for font locking.")

(defvar ballerina-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; punctuations
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; whitespaces

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)
    table)
  "Syntax definitions and helpers.")

(defun ballerina-build-font-lock-keywords ()
  (append
   `((, (concat "\\_<" (regexp-opt ballerina-mode-basic-types t) "\\_>") . font-lock-type-face)
     (, (concat "\\_<" (regexp-opt ballerina-mode-keywords t) "\\_>") . font-lock-keyword-face))))

;;;###autoload
(define-derived-mode ballerina-mode prog-mode "Ballerina"
   "Ballerina mode is a major mode for editing ballerina files"
   ;; Syntax highlighting
   :syntax-table ballerina-mode-syntax-table
   ;; keybindings
   ;; (define-key ballerina-mode-map (kbd "C-c C-f") 'ballerina-mode-command)
   ;; Fonts
        (setq font-lock-defaults '(ballerina-build-font-lock-keywords))   ;; Indentation settings
   (setq-local indent-tabs-mode nil) ; use spaces for indentation
   (setq-local tab-width 4)) ; tabwidth 4 spaces

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bal\\'" . ballerina-mode))

(provide 'ballerina-mode)
;;; ballerina-mode.el ends here
