# Ballerina-mode
Emacs major mode for [Ballerina language](https://ballerina.io). 

## Objectives
I wrote this mostly for my personal use and is expected to be used in conjunction the ballerina language server.

### Configuring LSP
Since emacs-29+ comes with [eglot](https://github.com/joaotavora/eglot) out of the box, and ballerina language server comes with any ballerina installation you can setup the language server without any external dependencies,

``` emacs-lisp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(ballerina-mode . ("bal" "start-language-server"))))
(add-hook 'ballerina-mode-hook 'eglot-ensure)
```
## Similar projects
+ [a5anka/ballerina-mode](https://github.com/a5anka/ballerina-mode)
