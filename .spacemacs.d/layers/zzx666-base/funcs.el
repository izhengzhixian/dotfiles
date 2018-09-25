(defun zzx666-base/insert-line-below (str)
  "Insert an empty line below the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)
    (forward-char)
    (insert str)))

(defun zzx666-base/insert-line-above (str)
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (end-of-line 0)
    (open-line 1)
    (insert str)))

(defun zzx666-base/get-string-from-file (filePath)
   "Return filePath's file content."
     (with-temp-buffer
          (insert-file-contents filePath)
              (buffer-string)))

(defun zzx666-base/read-lines (filePath)
   "Return a list of lines of a file at filePath."
     (with-temp-buffer
          (insert-file-contents filePath)
              (split-string (buffer-string) "\n" t)))
