(setq zzx666-office/blog-root-dir "~/blog")
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))
(add-hook 'LaTeX-mode-hook (lambda()
                             (add-to-list 
                               'TeX-command-list 
                               '("XeLaTeX" "xelatex %(mode) %t" TeX-run-TeX nil (latex-mode) ))
                             (setq TeX-command-default "XeLaTeX")
                             (setq TeX-save-query nil )
                             (setq TeX-show-compilation t)
                             (setq TeX-PDF-mode t) ))
