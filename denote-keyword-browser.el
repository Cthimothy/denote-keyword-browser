;;; denote-keyword-dired.el --- Denote keyword browser -*- lexical-binding: t; -*-

(defvar tw/denote-keyword-directory "~/Org"
  "Directory where Denote files are stored.")

(defface tw/denote-keyword-button-face
  '((t (:inherit button :underline nil)))
  "Face for denote keyword buttons without underline.")

(defun tw/hash-table-to-alist (table)
  "Convert a hash table to an alist. Compatible with older Emacs versions."
  (let (result)
    (maphash (lambda (k v)
               (push (cons k v) result))
             table)
    result))

(defun tw/denote--extract-keywords ()
  "Return an alist of (keyword . count) pairs from filenames in `tw/denote-keyword-directory`."
  (let ((files (directory-files-recursively tw/denote-keyword-directory "\\.org$"))
        (table (make-hash-table :test 'equal)))
    (dolist (file files)
      (let ((base (file-name-base file)))
        (when (string-match "__\\(.*\\)$" base)
          (dolist (kw (split-string (match-string 1 base) "_"))
            (puthash kw (1+ (gethash kw table 0)) table)))))
    (sort (tw/hash-table-to-alist table)
          (lambda (a b) (string< (car a) (car b))))))

(defun tw/denote--files-for-keyword (keyword)
  "Return list of files matching KEYWORD in their filename."
  (let ((files (directory-files-recursively tw/denote-keyword-directory "\\.org$"))
        result)
    (dolist (file files result)
      (let ((base (file-name-base file)))
        (when (and (string-match "__\\(.*\\)$" base)
                   (member keyword (split-string (match-string 1 base) "_")))
          (push file result))))))

(defun tw/denote-keyword-browser ()
  "Display keywords from Denote filenames. RET opens dired with matching files. q quits buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Denote Keywords*")))
    (with-current-buffer buffer
      (setq default-directory tw/denote-keyword-directory)
      (face-remap-add-relative 'button '(:underline nil))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Denote Keywords (RET to open files, q to quit):\n\n")
        (dolist (pair (tw/denote--extract-keywords))
          (let ((kw (car pair))
                (count (cdr pair)))
            (insert-text-button
             (format "%-20s (%d)\n" kw count)
             'action
             `(lambda (_)
                (let ((files (tw/denote--files-for-keyword ,kw)))
                  (if files
                      (dired (cons tw/denote-keyword-directory files))
                    (message "No files found for keyword: %s" ,kw))))
             'follow-link t
             'face 'tw/denote-keyword-button-face)))
        (goto-char (point-min))
        (read-only-mode 1)
        (hl-line-mode 1))
      (local-set-key (kbd "q") #'quit-window))
    (switch-to-buffer buffer)))

(provide 'denote-keyword-browser)
