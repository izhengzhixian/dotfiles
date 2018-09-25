(defconst zzx666-office-packages
  '(
    blog-admin
    gnus
    org
    prodigy
    ))

(defun zzx666-office/init-blog-admin ()
  (use-package blog-admin
    :ensure t
    ;:commands (blog-admin-start)
    :init
    (progn
      ;; 后台类型
      (setq blog-admin-backend-type 'hexo)
      ;; hexo 博客所在路径
      (setq blog-admin-backend-path 
           zzx666-office/blog-root-dir)
      ;; 默认在drafts创建文章
      (setq blog-admin-backend-new-post-in-drafts t)
      ;; 默认不创建相应的目录，目前没有发现这个目录的作用，先关掉
      (setq blog-admin-backend-new-post-with-same-name-dir nil)
      ;; default assumes _config.yml
      (setq blog-admin-backend-hexo-config-file "_config.yml")

      (spacemacs/set-leader-keys "obS" 'blog-admin-start)
      ;; 手动设置org的模板文件
      (define-namespace blog-admin-backend-hexo-
         (defvar  template-org-post
           (if (file-readable-p (concat zzx666-text-dir "template-org-post"))
             (zzx666-base/get-string-from-file
               (concat zzx666-text-dir "template-org-post"))
                          "")
                          "template for hexo's org post"))
       )))

(defun zzx666-office/post-init-blog-admin ()
  (use-package blog-admin
    :config
    (progn
      (when (equal 'hexo blog-admin-backend-type)
        (progn
          (defun blog-admin-backend-hexo--publish-or-unpublish-path ()
            "Switch between publish and drafts"
            (interactive)
            (let* ((post (blog-admin--table-current-file))
                   (dirpath (f-no-ext post)))
              (blog-admin-backend-hexo--exchange-place post)
              (blog-admin-backend-hexo--exchange-place dirpath)
              (blog-admin-refresh)
              ))
          (plist-put (blog-admin-backend-get-backend)
                     :publish-unpublish-func-path
                     'blog-admin-backend-hexo--publish-or-unpublish-path )
          (add-hook 'blog-admin-mode-hook '(lambda ()
          (define-key blog-admin-mode-map "S" 
                      (plist-get 
                        (blog-admin-backend-get-backend) :publish-unpublish-func-path))
          ))
          
          )
      ))
  ))


(defun zzx666-office/post-init-gnus ()
  (custom-set-variables
   `(gnus-home-directory
     ,(concat dotspacemacs-directory "layers/zzx666-office/gnus/"))
   )
  )

(defun zzx666-office/post-init-org()
 (use-package org
    :config
    (progn
      ;; 转码字符直接显示结果
      (setq org-pretty-entities t)
      ;; 讨厌的中文输入法
      (define-key org-mode-map (kbd "×") (kbd "*"))
      (define-key org-mode-map (kbd "－") (kbd "-"))
      ;; 用来解决org-mode中 #+ * 开头的行总加逗号的问题
      (defun org-escape-code-in-region (beg end)
  "Escape lines between BEG and END.
Escaping happens when a line starts with \"*\", \"#+\", \",*\" or
\",#+\" by appending a comma to it."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (while (re-search-backward "^[ \t]*\\(,*\\(?:\\*\\|#\\+\\)\\)" beg t)
      (save-excursion (replace-match "\\1" nil nil nil 1)))))

(defun org-escape-code-in-string (s)
  "Escape lines in string S.
Escaping happens when a line starts with \"*\", \"#+\", \",*\" or
\",#+\" by appending a comma to it."
  (replace-regexp-in-string "^[ \t]*\\(,*\\(?:\\*\\|#\\+\\)\\)" "\\1"
			    s nil nil 1))

(defun org-unescape-code-in-region (beg end)
  "Un-escape lines between BEG and END.
Un-escaping happens by removing the first comma on lines starting
with \",*\", \",#+\", \",,*\" and \",,#+\"."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (while (re-search-backward "^[ \t]*,*\\(,\\)\\(?:\\*\\|#\\+\\)" beg t)
      (save-excursion (replace-match "" nil nil nil 1)))))

(defun org-unescape-code-in-string (s)
  "Un-escape lines in string S.
Un-escaping happens by removing the first comma on lines starting
with \",*\", \",#+\", \",,*\" and \",,#+\"."
  (replace-regexp-in-string
   "^[ \t]*,*\\(,\\)\\(?:\\*\\|#\\+\\)" "" s nil nil 1))
    )
))

(defun zzx666-office/post-init-prodigy()
  ;; python2 http服务器
  (prodigy-define-service
    :name "Python2 server"
    :command "python2"
    :args '("-m" "SimpleHTTPServer" "8080")
    :cwd "~/Documents"
    :tags '(python2-http)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)

  ;; python3 http服务器
  (prodigy-define-service
    :name "Python3 server"
    :command "python3"
    :args '("-m" "http.server" "8080")
    :cwd "~/Documents"
    :tags '(python3-http)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Hexo Server"
    :command "hexo"
    :args '("server")
    :cwd zzx666-office/blog-root-dir
    :tags '(hexo server)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Hexo Deploy"
    :command "hexo"
    :args '("deploy" "--generate")
    :cwd zzx666-office/blog-root-dir
    :tags '(hexo deploy)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)
  )
