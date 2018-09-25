;; 下面是增加一个函数，org-display-inline-images-async，能够显示org-mode下的网络图片 
(defun org-display-inline-images-callback (status start end type old width ori-buffer)
  (unwind-protect
    (let (file-data)
      (goto-char (point-min))
      (search-forward-regexp "^$")
      (setq file-data (buffer-substring-no-properties (+ (point) 1) (point-max)))
      (when file-data
        (with-current-buffer ori-buffer
                             (if (and (car-safe old) refresh)
                               (image-refresh (overlay-get (cdr old) 'display))
                               (setq img (create-image file-data type t :width width))
                               (when img
                                 (setq ov (make-overlay start end))
                                 (overlay-put ov 'display img)
                                 (overlay-put ov 'face 'default)
                                 (overlay-put ov 'org-image-overlay t)
                                 (overlay-put ov 'modification-hooks
                                              (list 'org-display-inline-remove-overlay))
                                 (push ov org-inline-image-overlays))))))
    (kill-buffer)))

(defun org-display-inline-images-async (&optional include-linked refresh beg end)
  "Display inline images asynchronously.

  like org-display-inline-images. But it can display http-url-images in a asynchronous way. "
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (if (fboundp 'clear-image-cache) (clear-image-cache)))
    (save-excursion
      (save-restriction
        (widen)
        (setq beg (or beg (point-min)) end (or end (point-max)))
        (goto-char beg)
        (let ((re (concat "\\[\\[\\(\\(file:\\|http:\\|https:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
                          (substring (org-image-file-name-regexp) 0 -2)
                          "\\)\\]" (if include-linked "" "\\]")))
              (case-fold-search t)
              old file ov img type attrwidth width)
          (while (re-search-forward re end t)
                 (setq old (get-char-property-and-overlay (match-beginning 1)
                                                          'org-image-overlay)
                       file (substring-no-properties (match-string 0) 2 -2))
                 (when (image-type-available-p 'imagemagick)
                   (setq attrwidth (if (or (listp org-image-actual-width)
                                           (null org-image-actual-width))
                                     (save-excursion
                                       (save-match-data
                                         (when (re-search-backward
                                                 "#\\+attr.*:width[ \t]+\\([^ ]+\\)"
                                                 (save-excursion
                                                   (re-search-backward "^[ \t]*$\\|\\`" nil t)) t)
                                           (string-to-number (match-string 1))))))
                         width (cond ((eq org-image-actual-width t) nil)
                                     ((null org-image-actual-width) attrwidth)
                                     ((numberp org-image-actual-width)
                                      org-image-actual-width)
                                     ((listp org-image-actual-width)
                                      (or attrwidth (car org-image-actual-width))))
                         type (if width 'imagemagick)))
                 (require 'url)
                 (url-retrieve file #'org-display-inline-images-callback 
                               `(,(match-beginning 0) 
                                  ,(match-end 0) ,type ,old ,width ,(current-buffer)))))))))

(defun zzx666-office/org-blog-head()
  (interactive)
  (save-excursion
   (beginning-of-buffer)
   (zzx666-base/insert-line-above
   (format
     blog-admin-backend-hexo-template-org-post
     ""
     (format-time-string "<%Y-%m-%d %a>")
     )
  )))

(defun zzx666-office/latex-head()
  (interactive)
  (zzx666-base/insert-line-below
   "#+LATEX_HEADER: \\usepackage{xeCJK}\n#+LATEX_HEADER: \\setCJKmainfont{SimSun}"
  ))
