(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "c") 'find-file)))

(provide 'dired-create-file)
