(setq package-archives '(
                         ("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")                         
                         ))
(require 'package)
(package-initialize)

(defun pr--check ()
  (when (different old-directory elpa/new) dostuff))

(defmacro package-require (package)
  `(condition-case nil (require ,package)
     (error
      (progn
        (message "Not found %s" (list 'installing (quote ,package)))
        (condition-case nil
	    (package-install ,package)
	  (error
	   (progn
	     (package-refresh-contents)
	     (package-install ,package))))
	(require ,package)))))


(provide 'package-require)
