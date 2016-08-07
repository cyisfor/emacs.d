(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "c") 'my-dired-create-file)
     (defun create-new-file (file-list)
       (defun exsitp-untitled-x (file-list cnt)
         (while (and (car file-list) (not (string= (car file-list) (concat "untitled" (number-to-string cnt) ".txt"))))
           (setq file-list (cdr file-list)))
         (car file-list))

       (defun exsitp-untitled (file-list)
         (while (and (car file-list) (not (string= (car file-list) "untitled.txt")))
           (setq file-list (cdr file-list)))
         (car file-list))

       (if (not (exsitp-untitled file-list))
           "untitled.txt"
         (let ((cnt 2))
           (while (exsitp-untitled-x file-list cnt)
             (setq cnt (1+ cnt)))
           (concat "untitled" (number-to-string cnt) ".txt")
           )
         )
       )
     (defun my-dired-create-file (file)
       (interactive
        (list (read-file-name "Create file: " (concat (dired-current-directory) (create-new-file (directory-files (dired-current-directory))))))
        )
       (write-region "" nil (expand-file-name file) t) 
       (dired-add-file file)
       (revert-buffer)
       (dired-goto-file (expand-file-name file))
       )
     )
	)
