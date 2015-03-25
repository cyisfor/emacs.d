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
  (let ((parts (reverse (remove-if 'not-important (split-string s "/" t)))))
    (if (cdr parts)
	(concat (cadr parts) " - " (car parts))
      (car parts))))

(defun update-title ()
  (interactive)
  (let ((name (buffer-file-name (current-buffer))))
    (when name
      (set-frame-parameter (selected-frame) 'title (tail-of name)))))

(add-hook 'buffer-list-update-hook 'update-title)

(provide 'loctitle)
