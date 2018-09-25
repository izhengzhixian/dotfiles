(defconst zzx666-programming-packages
  '(
    company
    semantic
    quickrun
    web-mode
    ))

(defun zzx666-programming/post-init-web-mode ()
  ;; fix web-mode can not preview in browser
  (add-hook 'web-mode-hook
            (lambda ()
              (progn
                (define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-buffer)
                ))))

(defun zzx666-programming/post-init-semantic()
  ;; company 提示会被覆盖，所以这里关闭
  (use-package semantic
               :defer t
               :config
               (when 
                 (member 'global-semantic-idle-summary-mode semantic-default-submodes)
                 (setq semantic-default-submodes
                       (remove 'global-semantic-idle-summary-mode semantic-default-submodes)))))

(defun zzx666-programming/post-init-company()
  ;; set c++11
  (add-hook 'c++-mode-hook (lambda ()
                             ;(setq company-clang-arguments '("-std=c++11"))
                             (setq flycheck-clang-language-standard "c++11")
                             (let* ((c++-std-include-root "/usr/include/c++")
                                    (c++-std-include-root-dirs (reverse (directory-files c++-std-include-root))))
                               (when (> (list-length c++-std-include-root-dirs) 2)
                                 (add-to-list 'company-c-headers-path-system
                                              (expand-file-name (nth 0 c++-std-include-root-dirs)
                                                                c++-std-include-root)
                                              ))
                               )))
  ;; 把company-c-headers 提到 company-clang前面，否则#include<> 不会有提示
  (add-hook 'c-mode-common-hook (lambda ()
                             (when 
                               (member 'company-c-headers company-backends-c-mode-common)
                               (setq company-backends-c-mode-common
                                     (remove 'company-c-headers company-backends-c-mode-common))
                               (push 'company-c-headers company-backends-c-mode-common)
                               (setq company-backends company-backends-c-mode-common))
            ))
  )

  ;; quickrun使用c++11版本运行c++ 
(defun zzx666-programming/init-quickrun ()
  (use-package quickrun
    :ensure t
    :defer t
    :config
    (progn
      ;; Use this parameter as C++ default
      (quickrun-add-command "c++/c11"
                            '((:command . "g++")
                              (:exec    . ("%c -std=c++11 %o -o %e %s"
                                           "%e %a"))
                              (:remove  . ("%e")))
                            :default "c++")
      )))
