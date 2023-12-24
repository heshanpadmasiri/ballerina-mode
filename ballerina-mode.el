;;; ballerina-mode.el --- A major mode for editing ballerina source code -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Heshan Padmasiri
;;
;; Author: Heshan Padmasiri <hpheshan@gmail.com>
;; Maintainer: Heshan Padmasiri <hpheshan@gmail.com>
;; Created: July 22, 2023
;; Modified: December 24, 2023
;; Version: 0.0.1
;; Keywords: ballerina languages
;; Homepage: https://github.com/heshanpadmasiri/ballerina-mode
;; Package-Requires: ((emacs "24.3") (reformatter "0.6"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file implements a major mode for editing ballerina code
;;
;;; Code:
(require 'reformatter)
(defconst ballerina-mode-basic-types
  '("any"
     "anydata"
     "boolean"
     "byte"
     "decimal"
     "error"
     "float"
     "function"
     "future"
     "handle"
     "int"
     "json"
     "map"
     "never"
     "nil"
     "object"
     "object"
     "readonly"
     "record"
     "stream"
     "string"
     "table"
     "typedesc") "Basic types in ballerina language used for font locking.")

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
     "is"
     "isolated"
     "join"
     "let"
     "limit"
     "match"
     "on"
     "order"
     "outer"
     "private"
     "public"
     "remote"
     "return"
     "returns"
     "select"
     "service"
     "type"
     "var"
     "where"
     "while")  "All keywords in the ballerina language.  Used for font locking.")

;; TODO: currently does nothing (mostly to shutup copilot)
(defcustom ballerina-indent-offset 4
  "Indent ballerina code by this number of spaces."
  :type 'integer
  :group 'ballerina-mode
  :safe #'integerp)

(defcustom ballerina-format-on-save t
  "Format buffers before saving using bal format."
  :type 'boolean
  :safe #'booleanp
  :group 'ballerina-mode)

;; FIXME: this is not doing exactly what we need since ballerina formatter is not reading from stdin
(reformatter-define ballerina-format
  :program "bal"
  :args '("format")
  :group 'ballerina-mode)

(defconst ballerina-electric-indent-chars
  '(?\; ?, ?\) ?\] ?}))

;;;###autoload (autoload 'ballerina-format-buffer "current-file" nil t)
;;;###autoload (autoload 'ballerina-format-region "current-file" nil t)
;;;###autoload (autoload 'ballerina-format-on-save-mode "current-file" nil t)

(defconst ballerina-identifier-regexp "[[:word:][:multibyte:]]+")
(defconst ballerina-type-regexp "[[:word:][:multibyte:]|&?]+")
(defconst ballerina-func-regexp (concat "\\_<function\\_>\\s *\\(" ballerina-identifier-regexp "\\)"))
(defconst ballerina-return-type-regexp (concat "\\_<returns\\_>\\s *\\(" ballerina-type-regexp "\\)"))
(defconst ballerina-var-type-regexp (concat "\\(" ballerina-type-regexp "\\)"
                                      "[[:space:]][[:space:]]*"
                                      "\\(" ballerina-identifier-regexp"\\)"))

(defvar ballerina-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; punctuations
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?* "."  table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?`  "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; whitespaces

    ;; Comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)    ; Newline ends a
    table)
  "Syntax definitions and helpers.")

(defun ballerina-build-font-lock-keywords ()
  (append
    `((, (concat "\\_<" (regexp-opt ballerina-mode-basic-types t) "\\_>") . font-lock-type-face)
       (, (concat "\\_<" (regexp-opt ballerina-mode-keywords t) "\\_>") . font-lock-keyword-face)
       (, ballerina-func-regexp 1 font-lock-function-name-face)
       (, ballerina-return-type-regexp 1 font-lock-type-face)
       (, ballerina-var-type-regexp 1 font-lock-type-face)
       (, ballerina-var-type-regexp 2 font-lock-variable-name-face))))

;;;###autoload
(defun ballerina-mode-test ()
  "Run test suite in Ballerina project"
  (interactive)
  (let ((user-input (read-string "Enter test selection (leave empty to run all tests): ")))
    (if (string-empty-p user-input)
      (compile "bal test")
      (compile (format "bal test --tests %s" user-input)))))

;;;###autoload
(defun ballerina-mode-run ()
  "Run current ballerina file"
  (interactive)
  (let ((file-name (buffer-file-name)))
    (compile (format "bal run %s" file-name))))

;;;###autoload
(defun ballerina-mode-build ()
  "Build current ballerina project"
  (interactive)
  (compile "bal build"))

(defvar ballerina-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") 'ballerina-mode-build)
    (define-key map (kbd "C-c C-f") 'ballerina-format-buffer)
    (define-key map (kbd "C-c C-r") 'ballerina-mode-run)
    (define-key map (kbd "C-c C-t") 'ballerina-mode-test)
    map)
  "Keymap for ballerina major mode.")

;;;###autoload
(define-derived-mode ballerina-mode c-mode "Ballerina"
  "Ballerina mode is a major mode for editing ballerina files."
  ;; Syntax highlighting
  :syntax-table ballerina-mode-syntax-table
  ;; keybindings
  ;; (define-key ballerina-mode-map (kbd "C-c C-f") 'ballerina-mode-command)
  ;; Fonts
  (setq font-lock-defaults '(ballerina-build-font-lock-keywords))   ;; Fonts
  (setq-local indent-tabs-mode nil) ; use spaces for indentation
  (setq-local tab-width 4) ; tabwidth 4 spaces
  ;; comments
  (setq-local comment-start "//")
  (setq-local comment-end "")
  ;; indentation
  (setq-local electric-indent-chars
    (append ballerina-electric-indent-chars
      (and (boundp 'electric-indent-chars)
        electric-indent-chars))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bal\\'" . ballerina-mode))

(provide 'ballerina-mode)
;;; ballerina-mode.el ends here
