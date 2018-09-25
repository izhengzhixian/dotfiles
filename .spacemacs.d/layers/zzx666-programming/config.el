;; GDB窗口配置，抄的，然后修改了一点，网址如下
;; http://everet.org/customize-emacs-gud-many-windows.html
(defadvice gdb-setup-windows (after my-setup-gdb-windows activate)
  "my gdb UI"
  (gdb-get-buffer-create 'gdb-stack-buffer)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (let ((win0 (selected-window))
        (win1 (split-window nil nil 'left))      ;code and output
        (win2 (split-window-below (/ (* (window-height) 2) 3)))     ;stack
        )
    (select-window win2)
    (gdb-set-window-buffer (gdb-stack-buffer-name))
    (select-window win1)
    (set-window-buffer
     win1
     (if gud-last-last-frame
         (gud-find-file (car gud-last-last-frame))
       (if gdb-main-file
           (gud-find-file gdb-main-file)
         ;; Put buffer list in window if we
         ;; can't find a source file.
         (list-buffers-noselect))))
    (setq gdb-source-window (selected-window))
    (let ((win3 (split-window nil (/ (* (window-height) 3) 4)))) ;io
      (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil win3))
    (let ((win4 (split-window nil (/ (* (window-height) 2) 3)))) ;disasm
      (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-disassembly-buffer) nil win4))
    (select-window win0)
    ))


(custom-set-variables
  ;; quickrun does not move focus to output buffer.
  '(quickrun-focus-p nil)
  '(company-dabbrev-minimum-length 3)
  '(company-dabbrev-other-buffers nil)
  '(company-show-numbers t)
  '(company-statistics-auto-restore nil)
  '(markdown-command "/usr/bin/pandoc")
  )

(add-hook 'makefile-mode-hook
  (lambda ()
    (setq-local indent-line-function 'zzx666-programming/makefile-indent-line)))

(zzx666-programming/setup-indent 4)

;; c/c++ mode 中 copr 的yasnippet的配置
(setq copyright-author "your_name")
(setq copyright-email "your_id@qq.com")
(setq copyright-company-name "")
(setq copyright-company-email "")
(add-hook 'c-mode-common-hook 'zzx666-programming/my-c-mode)
;; tags文件不要提示过大
(add-to-list 'spacemacs-large-file-modes-list 'tags-table-mode)
