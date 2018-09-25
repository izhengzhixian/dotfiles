(defun zzx666-programming/smart-open-line ()
  "Insert an empty line after the current line.
  Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun zzx666-programming/setup-indent (n)
  ;; java/c/c++
  (setq c-basic-offset n)
  ;; web development
  (setq coffee-tab-width n) ; coffeescript
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq python-indent-guess-indent-offset nil) ; python-mode
  (setq python-indent-offset n) ; python-mode
  (setq css-indent-offset n) ; css-mode
  )

(defun zzx666-programming/format-buffer ()
  ;; 格式化缓冲区
  (interactive)
  (indent-region (point-min) (point-max) nil)
  )

(defun zzx666-programming/makefile-indent-line ()
  (save-excursion
(forward-line 0)
(cond
 ;; keep TABs
 ((looking-at "\t")
  t)
 ;; indent continuation lines to 4
 ((and (not (bobp))
       (= (char-before (1- (point))) ?\\))
  (delete-horizontal-space)
  (indent-to 4))
 ;; delete all other leading whitespace
 ((looking-at "\\s-+")
  (replace-match "")))))

(defun zzx666-programming/auto-insert-copyright (company-name author email)
  (zzx666-base/insert-line-above
   (let ((copyright-path (concat zzx666-text-dir "copyright")))
    (when (file-readable-p copyright-path)
    (format (zzx666-base/get-string-from-file copyright-path) (format-time-string "%Y") 
 (if (not (string= company-name "")) (concat " " company-name) "") 
 (buffer-name)
 author email
 (format-time-string "%Y/%m/%d %H:%M:%S")
 )))))

;; insert my copyright
(defun zzx666-programming/auto-insert-my-copyright ()
  (interactive)
  (zzx666-programming/auto-insert-copyright "" copyright-author copyright-email)
  )

;; insert company copyright
(defun zzx666-programming/auto-insert-company-copyright ()
  (interactive)
  (zzx666-programming/auto-insert-copyright 
     copyright-company-name
     copyright-author copyright-company-email)
  )

(defun zzx666-programming/my-c-mode ()
  (interactive)
  (c-set-offset 'access-label '-)
  (c-set-offset 'case-label 0)
  (setq c-basic-offset 4 )         ;; 基本缩进宽度
  (setq default-tab-width 4))     ;; 默认Tab宽度

(defun zzx666-programming/disable-semantic-idle-summary-mode ()
  (setq semantic-idle-summary-mode 0))
