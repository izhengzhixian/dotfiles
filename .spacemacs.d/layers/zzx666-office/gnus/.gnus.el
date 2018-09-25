;; gnus
;;;; gnus email
;; email地址
(setq zzx666-email "your_id@qq.com")
;; 发送邮件时显示的名字
(setq zzx666-name "your_name")
;; pop服务器
(setq zzx666-pop-server "pop.qq.com")
;; imap服务器
(setq zzx666-imap-server "imap.qq.com")
;; smtp服务器
(setq zzx666-smtp-server "smtp.qq.com")
;; 在这设置相应值以后，在~/.authinfo中设置pop3密码和smtp密码
;; 如以下格式
;; machine pop.qq.com login your_id@qq.com password passwordvalue
;; machine smtp.qq.com login your_id@qq.com password passwordvalue


(setq gnus-secondary-select-methods '((nnfolder "")))

;; imap
(setq gnus-secondary-select-methods
`(
  (nnimap "qq-mail"
           (nnimap-address ,zzx666-imap-server)
           (nnimap-server-port 993)
           (nnimap-stream ssl))
  ))

;; pop3
;(setq mail-sources
;`((pop
;:server ,zzx666-pop-server ;; 在这里设置pop3服务器
;:user ,zzx666-email ;; 用户名
;:port 995
;:stream ssl
;)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server zzx666-smtp-server
      smtpmail-stream-type 'ssl
      smtpmail-smtp-service 465
      smtpmail-debug-info t
      smtpmail-debug-verb t
      )

;;;; gnus news
(setq user-full-name "zzx666")    ;;你在新闻组显示的名字
(setq user-mail-address "your_id@qq.com")  ;;你在新闻组中显示的邮箱
(setq gnus-select-method '(nntp "freenews.netfront.net"))
;(setq gnus-select-method '(nntp "news.newsfan.net"))
;(add-to-list 'gnus-secondary-select-methods '(nntp "freenews.netfront.net"))
;(add-to-list 'gnus-secondary-select-methods '(nntp "news.php.net"))
;; 保存有价值的帖子,在有价值的帖子上加*，就保存到本地，按meta+*就取消保存
(setq gnus-use-cache 'passive)

;;;;;;;;;;;;;;;;;;;;
;;   语言环境设定
;;;;;;;;;;;;;;;;;;;;
;(set-language-environment 'Chinese-GB)
(set-language-environment 'UTF-8)
(setq gnus-default-charset 'chinese-iso-8bit
      gnus-group-name-charset-group-alist '(
            ("\\.com\\.cn:" . cn-gb-2312)
            (".*" . cn-gb-2312)
            ("a-z.*" . utf-8))
	  gnus-summary-show-article-charset-alist
	  '((1 . cn-gb-2312)
		(2 . gb18030)
		(3 . chinese-iso-8bit)
		(4 . gbk)
		(5 . big5)
		(6 . utf-8))
	  gnus-newsgroup-ignored-charsets
	  '(unknown-8bit x-unknown iso-8859-1))

;;;;;;;;;;;;;;;;;;;;
;;自动显示图片
;;;;;;;;;;;;;;;;;;;;
(auto-image-file-mode)
(setq mm-inline-large-images t)
(add-to-list 'mm-attachment-override-types "image/*")

(setq gnus-posting-styles
      `((".*"
     (name ,zzx666-name)
     (address ,zzx666-email)
     (signature "")
     ))
)
