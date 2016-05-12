;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;; sort ido filelist by mtime instead of alphabetically
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
(defun ido-sort-mtime ()
  (setq ido-temp-list
		(sort ido-temp-list 
			  (lambda (a b)
				(time-less-p
				 (sixth (file-attributes (concat ido-current-directory b)))
				 (sixth (file-attributes (concat ido-current-directory a)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
			  (lambda (x) (and (char-equal (string-to-char x) ?.) x))
			  ido-temp-list))))

(provide 'ido-fixes)
