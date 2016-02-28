(defun fix-html-mode ()
  (interactive)
  (setq sgml-unclosed-tags nil)
  (message "yas offing")
  (yas-minor-mode -1)  
  (setcdr (assoc "code" html-tag-alist) '(nil)))

;; hish is an XML mode
(defun guess-hish (orig-fun &rest args)
  (when buffer-file-name
    (or (string= "hish" (file-name-extension buffer-file-name))
	(apply orig-fun args))))

(advice-add 'guess-hish :around #'sgml-xml-guess)

(add-hook 'html-mode-hook 'fix-html-mode)
					
(provide 'htmlfixes)
