;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

(require 'types)

(defun not-important (s)
  (let ((s (intern s)))
    (or (eq s 'markup)
	(eq s 'chapters)
	(eq s 'extra)
	(eq s 'html)
	(eq s 'code)
	(eq s 'home)
	(eq s 'stories)
	(eq s (intern (user-login-name))))))

(defun tail-of (s)
  (let ((parts (reverse (cl-remove-if 'not-important (split-string s "/" t)))))
    (if (cdr parts)
	(concat (cadr parts) " - " (car parts))
      (car parts))))

(defun update-title (&optional buffer)
  (interactive)
	(let* ((buffer (or buffer (current-buffer)))
				 (file-name (buffer-file-name buffer))
				 (name (if file-name (tail-of file-name) (buffer-name buffer))))
		(message "name %s %s" name buffer)
		(set-frame-parameter (selected-frame) 'title name)))

(add-hook 'window-configuration-change-hook 'update-title)
;;(add-hook 'focus-in-hook 'update-title)
	
(provide 'loctitle)
