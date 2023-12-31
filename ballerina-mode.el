;;; ballerina-mode.el --- A major mode for editing ballerina source code -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Heshan Padmasiri
;;
;; Author: Heshan Padmasiri <hpheshan@gmail.com>
;; Maintainer: Heshan Padmasiri <hpheshan@gmail.com>
;; Created: July 22, 2023
;; Modified: December 26, 2023
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

(defconst ballerina-electric-indent-chars
  '(?\; ?, ?\) ?\] ?}))

;;;###autoload
(defun ballerina-format-buffer ()
  "Format the current Ballerina buffer using `bal format` and reload the buffer."
  (interactive)
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (let ((default-directory (file-name-directory buffer-file-name))
          (output-buffer (generate-new-buffer "*bal-format-output*")))
      (call-process "bal" nil output-buffer nil "format" buffer-file-name)
      (message "%s" (with-current-buffer output-buffer (buffer-string)))
      (kill-buffer output-buffer))
    ;; Reload the buffer
    (revert-buffer t t)))

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

(defcustom ballerina-bal-bin "bal"
  "Path to ballerina executable."
  :type 'string
  :group 'ballerina-mode)

(defvar-local ballerina-buffer-project nil)

(defun ballerina-update-buffer-project ()
  "Update the project directory."
  (setq-local ballerina-buffer-project (ballerina-find-project-directory)))

(defun ballerina-find-project-directory ()
  "Find the nearest directory containing a Ballerina.toml file."
  (let ((current-dir (file-name-directory buffer-file-name))
        (continue t))
    (while (and current-dir continue)
      (let ((toml-path (expand-file-name "Ballerina.toml" current-dir)))
        (if (file-exists-p toml-path)
            (setq continue nil)
          (let ((parent-dir (file-truename (concat current-dir "../"))))
            (if (string= current-dir parent-dir)
                (setq current-dir nil)
              (setq current-dir parent-dir))))))
    current-dir))

(defun ballerina--compile (format-string &rest args)
  "Run compile command at the project root directory."
  (when (null ballerina-buffer-project)
    (ballerina-update-buffer-project))
  (let ((default-directory
         (or (and ballerina-buffer-project
                  (file-name-directory ballerina-buffer-project))
             default-directory)))
    (compile (apply #'format format-string args))))

;;;###autoload
(defun ballerina-mode-test ()
  "Run test suite in Ballerina project."
  (interactive)
  (let ((user-input (read-string "Enter test selection (leave empty to run all tests): ")))
    (if (string-empty-p user-input)
        (ballerina--compile "%s test" ballerina-bal-bin)
      (ballerina--compile "%s test --tests %s" ballerina-bal-bin user-input))))

;;;###autoload
(defun ballerina-mode-run ()
  "Run current ballerina file."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (ballerina--compile "%s run %s" ballerina-bal-bin file-name)))

;;;###autoload
(defun ballerina-mode-build ()
  "Build current ballerina project."
  (interactive)
  (ballerina--compile "%s build" ballerina-bal-bin))

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

;;; Hooks
(defun ballerina-before-save ()
  "Hook to run format before save."
  (when ballerina-format-on-save
    (condition-case e
        (ballerina-format-buffer)
      (message (format "ballerina-before-save-hook: %S %S"
                     (car e)
                     (cdr e))))))
(add-hook 'after-save-hook 'ballerina-before-save)
(provide 'ballerina-mode)
;;; ballerina-mode.el ends here
