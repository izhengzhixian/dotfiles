;;; packages.el --- emms layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: love <love@CentOS>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `emms-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `emms/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `emms/pre-init-PACKAGE' and/or
;;   `emms/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst zzx666-life-packages
  '(
     ;; 必应词典
     bing-dict
     ;; 一款写作和管理纯文本格式笔记的工具
     deft
     ;; 多媒体播放器
	 emms
     ;; rss 阅读器
     elfeed
     ;; epub阅读器
     nov
     ;; 网页浏览器
     w3m
     ;; pandora客户端
     pianobar
     ;; 中文输入法
     pyim
     ;; sdcv客户端
     chinese-yasdcv
     ;; 多人本地协作编辑
     floobits
     ;; music
     )
  "The list of Lisp packages required by the emms layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
name of the package to be installed or loaded, and KEYS are
any number of keyword-value-pairs.

The following keys are accepted:

- :excluded (t or nil): Prevent the package from being loaded
if value is non-nil

- :location: Specify a custom installation location.
The following values are legal:


      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


;;; packages.el ends here

(defun zzx666-life/init-emms ()
  (use-package emms
			   :ensure t
			   :defer t
			   :config
			   (progn
				 (require 'emms-setup)
				 (emms-standard)
                 (require 'emms-player-simple)
                 (require 'emms-source-file)
                 (require 'emms-source-playlist)
                 (require 'emms-player-mplayer)
                 (emms-default-players)
                 ;(setq emms-player-list '(emms-player-mplayer))
                 (setq emms-source-file-default-directory "~/Music/")
                   )))

(defun zzx666-life/init-floobits()
  (use-package floobits
               :defer t
               :config
               (progn

                 )))

(defun zzx666-life/init-nov ()
  (use-package nov
               :defer t
               :init
               (progn
                 ;(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
                 )
               :config
               (progn
                 (setq nov-text-width 100)
                 (add-hook 'nov-mode-hook '(lambda () 
                                             (progn

                                               )))
                 )))

(defun zzx666-life/init-w3m()
  (use-package w3m
               :defer t
               :init
               (progn

                 )))

(defun zzx666-life/init-pianobar()
  (use-package pianobar
               :defer t
               :init
               (progn

                 )))

(defun zzx666-life/post-init-deft ()
  (use-package deft
               :defer t
               :config
               (progn
                 (setq deft-recursive t)
                 (setq deft-directory 
                       (if (file-exists-p "~/github/z-wiki")
                       "~/github/z-wiki"
                       "~/.deft"
                       ))
                 )))
(defun zzx666-life/post-init-pyim ()
 ;; pyim作者的配置
 (use-package pyim
  :ensure nil
  :demand t
  :config
  (custom-set-variables
    '(pyim-dicts
    `((
       :name "pyim大词库" 
       :file 
       ,(concat dotspacemacs-directory "pyim-bigdict.pyim")))))
  ; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))

  ;; 五笔用户使用 wbdict 词库
  ;; (use-package pyim-wbdict
  ;;   :ensure nil
  ;;   :config (pyim-wbdict-gbk-enable))

  (setq default-input-method "pyim")

  ;; 我使用全拼
  (setq pyim-default-scheme 'quanpin)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
  ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;; 手动安装 posframe 包。
  (setq pyim-page-tooltip 'popup)

  ;; 选词框显示9个候选词
  (setq pyim-page-length 9)

  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t)))
  :bind
  (("M-j" . pyim-convert-code-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer)))
  )

(defun zzx666-life/init-chinese-yasdcv ()
  (use-package chinese-yasdcv
               :ensure t
               :defer t
               :config
               (progn

                 )))

(defun zzx666-life/init-bing-dict ()
  (use-package bing-dict
               :ensure t
               :config
               (progn
                 ;; 'synonym, 'antonym or 'both 同义词，反义词，或者两者都有
                 (setq bing-dict-show-thesaurus 'both)
                 ;; 发音，有uk 和 us 两种，默认us
                 (setq bing-dict-pronunciation-style 'us)
                 (setq bing-dict-save-search-result t)
                 (setq bing-dict-org-file (concat user-emacs-directory
                                                  ".cache/bing-dict.org")
                 ))))
  
(defun zzx666-life/pre-init-bing-dict ()
  (use-package bing-dict
               :ensure t
               :config
               (progn
                 (defun zzx666-life/bing-dict-brief+ (word)
                   "Show the explanation of WORD from Bing in the echo area."
                   (interactive
                     (let* ((default (if (use-region-p)
                                       (buffer-substring-no-properties
                                         (region-beginning) (region-end))
                                       (let ((text (thing-at-point 'word)))
                                         (if text (substring-no-properties text)))))
                            (string default))
                       (list string)))
                   (save-match-data
                     (url-retrieve (concat bing-dict--base-url
                                           (url-hexify-string word))
                                   'bing-dict-brief-cb
                                   `(,(decode-coding-string word 'utf-8))
                                   t
                                   t)))
                 (autoload 'zzx666-life/bing-dict-brief+ "bing-dict")
                 )))

(defun zzx666-life/pre-init-elfeed  ()
  (use-package elfeed
               :ensure t
               :defer t
               :config
               (progn
                 ;; 解决elfeed中G与vim中G冲突
                 (define-key elfeed-search-mode-map "G" nil)
                 (setq elfeed-feeds '(("https://emacs-china.org/latest.rss" emacs)
                                      ("http://zzhcoding.coding.me/atom.xml" zhouzihao)
                                      ("https://www.zhihu.com/rss" 知乎)
                                      ("http://songshuhui.net/feed" 松鼠科学会)
                                      ("http://www.geekpark.net/rss" 极客公园)
                                      ("http://feed.cnblogs.com/blog/u/62514/rss" 个体博客)
                                      ))
               )))
