(defvar-local backup-directory "~/tmp/emacsbackups")
(make-directory backup-directory t)

(setq backup-directory-alist
      `(("." . ,backup-directory)))
(setq auto-save-file-name-transforms
	  `((".*/?\\(.*\\)" ,(concat
					  backup-directory "/\\1") t)))

(provide 'nostupidbackups)
