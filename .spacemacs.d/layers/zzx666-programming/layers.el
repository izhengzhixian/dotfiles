(configuration-layer/declare-layers '(
                                      zzx666-base
                                      ;html
                                      ;javascript
                                      ;php
                                      (python 
                                       :variables 
                                       python-backend 'anaconda
                                       python-sort-imports-on-save t
                                       python-enable-yapf-format-on-save t
                                       python-shell-interpreter "python3"
                                       )
                                      ;;django
                                      yaml
                                      ;ruby
                                      ;latex
                                      gtags
                                      (c-c++ :variables 
                                             c-c++-enable-clang-support t
                                             c-c++-default-mode-for-headers 'c++-mode
                                             c-c++-enable-google-style t
                                             c-c++-enable-google-newline t
                                             )
                                      ;csharp
                                      ;; api文档
                                      ;dash
                                      imenu-list
                                      ;; 问题太多，这里就给关闭了
                                      ;semantic
                                      asm
                                      shell-scripts
                                      emacs-lisp
                                      (auto-completion :variables auto-completion-enable-sort-by-usage t
                                                       ;; auto-completion-enable-snippets-in-popup t
                                                       :disabled-for org markdown)
                                    ;  git
                                      (shell :variables
                                             shell-default-height 30
                                             shell-default-position 'bottom)
                                      syntax-checking
                                      ;; version-control
                                      ))
