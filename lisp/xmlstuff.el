; -*- mode: Lisp; lexical-binding: t -*-

(require 'types)

(setq auto-mode-alist (cons
		       '("\.hish$" . html-mode)
		       auto-mode-alist))

(setq htmly-types '(sgml html xml nxml text))
(setq edity-types (append htmly-types edity-types))

; strike out is ctrl-p? meh...

(setq mappings
      '(i b u (p . s) p (l . li)))

(setq mappings-memo nil)

; http://stackoverflow.com/a/4513683
(when (display-graphic-p)
   (keyboard-translate ?\C-i ?\H-i))

; passing a list ending in a character to the low level event-convert-list
; returns an integer, which made a 1-element vector works for local-set-key

(defun maybecontrol (sym)
  (vector (event-convert-list
   (list
    (if (display-graphic-p)
	(if (eq sym 'i)
	'hyper
	'control)
      'meta)
    (string-to-char (symbol-name sym))))))

(require 'cl) ; for flet

(defun html-shortcuts ()
  (when (eq mappings-memo nil)
    (flet ((funfortag (tag)
		      (lambda ()
			(interactive)
			(sgml-tag tag))))      
      (setq mappings-memo
	    (mapcar
	     (lambda (item)
		 (if (consp item)
		     (cons (maybecontrol (car item))
			   (funfortag (symbol-name (cdr item))))
		   (cons (maybecontrol item)
			 (funfortag (symbol-name item)))))
	     mappings))))
  (dolist (item mappings-memo)
    (local-set-key (car item) (cdr item)))
  (local-set-key [?\C-t] 'sgml-tag)
  (local-set-key [?\C-d] "\C-\M-n"))

(dolist (type htmly-types)
  (add-hook (type->hook type) 'html-shortcuts))

(provide 'xmlstuff)
