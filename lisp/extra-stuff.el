(provide 'extra-stuff)

(require 'dired-create-file)
(condition-case nil
		(progn
			(require 'frame-bufs)
			(frame-bufs-mode t))
	(error nil))

;(require 'ido-fixes)

(require 'show-point-mode)

(defmacro if-load (p &rest block) (declare (indent defun))
          `(when (file-directory-p ,p)
             (push ,p load-path)
             ,@(append block
					   '((pop load-path)))))

(defun unsafe-home (path)
	"The \"unsafe\" home directory, that public stuff can go in, like public source code repositories"
	(let ((unsafe-home "/extra/home"))
		(concat (file-name-as-directory unsafe-home) path)))

(if-load (unsafe-home "packages/git/emacswiki.org")
	(require 'apropos-fn+var)
	(require 'icomplete+)
	(require 'mb-depth+))

(if-load (unsafe-home "packages/git/xah-replace-pairs")
	;; in load thou
	(require 'curlify-quotes)) 

(if-load (unsafe-home "packages/git/lua-mode/")
  (require 'lua-mode))

(if-load (unsafe-home "packages/hg/wisp/")
  (require 'wisp-mode)
  (add-to-list 'auto-mode-alist '("\\.wisp\\'" . wisp-mode)))

;; (if-load "~/packages/bzr/components-python-mode"
;;          (require 'python-components-mode))

(defmacro when-require (what &rest body)
	`(if (member ,what features)
		 nil
	   (progn
		 (require ,what)
		 ,@body)))



;; Make absolutely sure python-mode uses tabs
;; thanks mk-fg
(defun do-smart-tabs ()
  (when-require 'smart-tabs-mode
				(setq do-smart-tabs t)
				(smart-tabs-insinuate 'python)))

(require 'types)

;(require 'savekill)
(require 'lazyclipboard)

;;(require 'quote-display)
;;(global-set-key (kbd "M-q") 'toggle-hide-outside-quotes)

(when-require
 'undo-tree
 (global-undo-tree-mode)
 (delete-selection-mode 1)

 (defun count-words-so-far (&rest args)
   (interactive)
   (message "Buffer has %S lines %S words, and %S characters (so far)"
			(count-lines 1 (point))
			(count-words 1 (point))
			(point)))
 
 (defun bonglify (s)
   (subst-char-in-string ?+ ?_
						 (subst-char-in-string ?/ ?-
						(base64-encode-string s))))
 
 (if (not (file-exists-p "~/.emacs.d/undo-history")) (make-directory "~/.emacs.d/undo-history/"))
 (defadvice undo-tree-make-history-save-file-name
	 (after undo-tree activate)
   (setq ad-return-value (concat "~/.emacs.d/undo-history/"
																 (substring
					   (bonglify ad-return-value)
					   (+ 3 (string-bytes "~/.emacs.d/undo-history/")))
																 ".gz")))
 (setq undo-tree-auto-save-history t)
 
 (setq undo-limit 800000))

(cl-macrolet ((insertit (what)
						`(lambda ()
						   (interactive)
						   (insert ,what))))
  (defun for-cool-schemes-only ()
	(local-set-key (kbd "C-\\") (insertit "λ")))
  (global-set-key (kbd "C-\\") (insertit "lambda")))

(setq auto-mode-alist (append
		       '(("\.lua$" . lua-mode)
						 ("\.rkt$" . (lambda ()
													 (racket-mode)
													 (for-cool-schemes-only))))
		       auto-mode-alist))

(autoload 'lua-mode "lua-mode" "Lua Editing mode." t)

;; (autoload 'bison-mode "bison-mode.el")
;; (autoload 'flex-mode "flex-mode.el")

;; (setq auto-mode-alist (append
;; 		       '(("\\.y$" . bison-mode)
;; 			 ("\\.l$" . flex-mode))
;; 		       auto-mode-alist))

(defun fix-d ()
	(interactive)
	(save-excursion 
		(goto-char (point-min))
		(replace-string "	" "		")
		(goto-char (point-min))
		(replace-string "  " "	")))

(defun herp-derp-emacs-sucks ()
  (require 'c-stuff))

(add-hook 'c++-mode
		  'herp-derp-emacs-sucks)
(add-hook 'c-mode
		  'herp-derp-emacs-sucks)

;(package-require 'geiser)

(defun setup-yas ()
  (interactive)
  (when-require 'yasnippet
				(yas-reload-all))
  (yas-minor-mode 1)
  (yas-minor-mode-on))

(dolist (type programmy-types)
  (add-hook (type->hook type)
			'setup-yas))
(require 'package-require)
(package-require 'bookmark+)

;; (push "~/packages/git/autopair/" load-path)
;; (require 'autopair)
;; (dolist (type programmy-types)
;;          (add-hook (type->hook type)
;;                    'autopair-mode))

;; (require 'python-skeleton)

;; (push "~/packages/git/rust-mode/" load-path)
;; (require 'rust-mode)

;;(require 'haskell-mode-autoloads)

;;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;(push "~/packages/git/emacs-haskell-unicode-input-method" load-path)
;;(require 'haskell-unicode-input-method)

;;(add-hook 'haskell-mode-hook 'quail-activate)

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)

;; (load "~/packages/git/lyqi/lyqi")
;; (setq auto-mode-alist (append
;;                        '(("\\.ly$" . lyqi-mode)
;;                          ("\\.ily$" . lyqi-mode))
;;                        auto-mode-alist))

(if-load "~/packages/git/nim-mode"
;;(if-load "~/code/"
  (require 'nim-mode))

(if-load "~/packages/git/go-mode.el/"
         (require 'go-mode-autoloads)
         (defun cancellable-gofmt-before-save ()
           (interactive)
           (condition-case nil
               (gofmt-before-save)
             (quit nil)))
         (defun gofmt-mode ()
           (interactive)
           (add-hook 'before-save-hook 'cancellable-gofmt-before-save nil t)
           (go-mode))

         (push '("\\.go$" . gofmt-mode) auto-mode-alist))


;; (push "~/packages/git/slime" load-path)
;; (require 'slime-autoloads)
;; (setq slime-lisp-implementations
;;       '((sbcl ("sbcl" "--core" "/extra/user/packages/git/slime/sbcl.core-for-slime"))))

;; (setq inferior-lisp-program "/usr/bin/sbcl")
;; (setq slime-contribs '(slime-fancy))

(require 'sexpfun)

(defun my-add-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          ("->" . 8594)    ; →
          ("=>" . 8658)    ; ⇒
          ("map" . 8614)   ; ↦
          ))
  (prettify-symbols-mode 1))

(add-hook 'scheme-mode-hook 'my-add-pretty-lambda)
(add-hook 'racket-mode-hook 'my-add-pretty-lambda)

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.
  
    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.
  
    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "[_[:word:]-]+" "\\&" beg end))

;; (load "~/quicklisp/slime-helper.el")

;;(icy-mode 1)

