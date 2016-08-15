(when (file-exists-p "~/.emacs.d/init.elc")
  (delete-file "~/.emacs.d/init.elc"))

(push "~/.emacs.d/lisp" load-path)
(byte-recompile-directory (expand-file-name "~/.emacs.d/lisp"))
(require 'essential-stuff)

(require 'typopunct)
(typopunct-change-language 'english t)
(dolist (type programmy-types)
  (add-hook (type->hook type) 'buffer-face-mode))
(add-hook (type->hook 'dired) 'buffer-face-mode)

(require 'loctitle)

(defun my-typo-init ()
  (typopunct-mode 1))

(dolist (type edity-types)
  (add-hook (type->hook type) 'my-typo-init))

(push `(yaml ?"
			 ?"
			 ,(decode-char 'ucs #x2018)
			 ,(decode-char 'ucs #x2019))
	  typopunct-language-alist)

(add-hook 'yaml-mode-hook
		  (lambda ()
			(typopunct-change-language 'yaml)
			(error "oops")
			(typopunct-mode 1)))

(require 'dired-create-file)
(condition-case nil
		(progn
			(require 'frame-bufs)
			(frame-bufs-mode t))
	(error nil))

;(require 'ido-fixes)

(require 'nostupidbackups)

(require 'show-point-mode)

(defmacro if-load (p &rest block) (declare (indent defun))
          `(when (file-directory-p ,p)
             (push ,p load-path)
             ,@(append block
					   '((pop load-path)))))

(if-load "/extra/home/packages/git/emacswiki.org" 
	(require 'apropos-fn+var)
	(require 'icomplete+)
	(require 'mb-depth+))

(if-load "/extra/home/packages/git/xah-replace-pairs"
	;; in load thou
	(require 'curlify-quotes)) 

(require 'package-require)

(if-load "/extra/home/packages/git/lua-mode/"
  (require 'lua-mode))

(if-load "/extra/home/packages/hg/wisp/"
  (require 'wisp-mode)
  (add-to-list 'auto-mode-alist '("\\.wisp\\'" . wisp-mode)))

;; (if-load "~user/packages/bzr/components-python-mode"
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

(dolist (type programmy-types)
         (add-hook (type->hook type)
                   (if (eq type 'c)
                       '(lambda ()
                          (local-set-key (kbd "<return>") 'newline-and-indent)
                          (local-set-key (kbd "C-<return>") 'c-indent-new-comment-line)
						  (local-set-key (kbd "M-<return>") 'electric-indent-just-newli
										 ne)
						  )
                     '(lambda ()
                        (local-set-key (kbd "<return>") 'newline-and-indent)
                        (local-set-key (kbd "C-<return>") 'newline)
                        (local-set-key (kbd "M-<return>") 'electric-indent-just-newline)
			))))

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
   ((setq ad-return-value (concat "~/.emacs.d/undo-history/"
										(substring
					   (bonglify (and )d-return-value)
					   (+ 3 (string-bytes "~/.emacs.d/undo-history/")))
										".gz")))
 (setq undo-tree-auto-save-history t)
 
 (setq undo-limit 800000)))

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

(setq buffer-face-mode-face '(:family "Bitstream Vera Sans Mono" :height 140 :width semi-expanded))

(global-set-key (kbd "C-f") 'buffer-face-mode)

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

(dolist (type edity-types)
  (add-hook (type->hook type)
			#'(lambda ()
				(require 'xmlstuff))))
(package-require 'bookmark+)

;; (push "~user/packages/git/autopair/" load-path)
;; (require 'autopair)
;; (dolist (type programmy-types)
;;          (add-hook (type->hook type)
;;                    'autopair-mode))

;; (require 'python-skeleton)

;; (push "~user/packages/git/rust-mode/" load-path)
;; (require 'rust-mode)

;;(require 'haskell-mode-autoloads)

;;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;(push "~user/packages/git/emacs-haskell-unicode-input-method" load-path)
;;(require 'haskell-unicode-input-method)

;;(add-hook 'haskell-mode-hook 'quail-activate)

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)

;; (load "~user/packages/git/lyqi/lyqi")
;; (setq auto-mode-alist (append
;;                        '(("\\.ly$" . lyqi-mode)
;;                          ("\\.ily$" . lyqi-mode))
;;                        auto-mode-alist))

(if-load "~user/packages/git/nim-mode"
;;(if-load "~user/code/"
  (require 'nim-mode))

(if-load "~user/packages/git/go-mode.el/"
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


;; (push "~user/packages/git/slime" load-path)
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

(line-number-mode 1)
(column-number-mode 1)
(global-linum-mode -1)
;; (load "~user/quicklisp/slime-helper.el")

;;(icy-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backward-delete-char-untabify-method nil)
 '(bmkp-last-as-first-bookmark-file "/extra/hacker/.emacs.d/bookmarks")
 '(cursor-type (quote bar))
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-visual-line-mode t)
 '(gofmt-command "goimports")
 '(ibuffer-always-compile-formats t)
 '(icicle-modal-cycle-down-action-keys (quote ([nil (control mouse-5)] [(control mouse-5)])))
 '(icicle-modal-cycle-down-keys (quote ([control down] [nil mouse-5] [mouse-5])))
 '(icicle-modal-cycle-up-keys (quote ([control up] [nil mouse-4] [mouse-4])))
 '(indent-tabs-mode t)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(kill-whole-line t)
 '(max-specpdl-size 3000)
 '(nxml-slash-auto-complete-flag t)
 '(py-complete-function (lambda (&rest args) nil))
 '(py-electric-colon-active-p t)
 '(py-electric-colon-greedy-p t)
 '(python-indent-offset 1)
 '(safe-local-variable-values (quote ((encoding . utf8))))
 '(scheme-program-name "csi -:c")
 '(sgml-xml-mode t)
 '(slime-auto-start (quote always))
 '(tab-width 2)
 '(track-eol t)
 '(typopunct-buffer-language (quote english))
 '(visual-line-fringe-indicators (quote (nil right-curly-arrow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Serif" :foundry "unknown" :slant normal :weight normal :height 143 :width normal))))
 '(py-decorators-face ((t (:inherit font-lock-keyword-face :foreground "orange"))))
 '(py-def-class-face ((t (:inherit font-lock-keyword-face :foreground "hot pink"))))
 '(py-exception-name-face ((t (:inherit font-lock-builtin-face :weight bold))))
 '(py-import-from-face ((t (:inherit font-lock-keyword-face :underline t))))
 '(py-number-face ((t (:inherit default :foreground "orange red"))))
 '(py-object-reference-face ((t (:inherit py-pseudo-keyword-face :foreground "blue"))))
 '(py-pseudo-keyword-face ((t (:inherit font-lock-keyword-face :foreground "lime green"))))
 '(py-try-if-face ((t (:inherit font-lock-keyword-face :weight bold))))
 '(py-variable-name-face ((t (:inherit default :foreground "light sea green"))))
 '(whitespace-line ((t (:background "light gray" :foreground "dark magenta")))))
(put 'upcase-region 'disabled nil)
