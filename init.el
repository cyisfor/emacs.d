(when (file-exists-p "~/.emacs.d/init.elc")
  (delete-file "~/.emacs.d/init.elc"))

(push "~/.emacs.d/lisp" load-path)
(byte-recompile-directory (expand-file-name "~/.emacs.d/lisp") 0)
(require 'essential-stuff)

(require 'typopunct)
(typopunct-change-language 'english t)
;(dolist (type programmy-types)
;  (add-hook (type->hook type) 'buffer-face-mode))
(add-hook 'prog-mode-hook 'buffer-face-mode)
;; ibuffer-mode etc:
(add-hook 'special-mode-hook 'buffer-face-mode)
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
			(typopunct-mode 1)))

(require 'nostupidbackups)

(add-hook 'prog-mode-hook
					'(lambda ()
						 (local-set-key (kbd "<return>") 'newline-and-indent)
						 (local-set-key (kbd "C-<return>") 'c-indent-new-comment-line)
						 (local-set-key (kbd "M-<return>") 'electric-indent-just-newline)))

(when (eq nil (getenv "quick"))
	(require 'server)
	(defadvice server-start (before load-extra-stuff
																	activate preactivate compile)
		(message "LOADAN COOL STUFFFS")
		(require 'extra-stuff)))

(setq buffer-face-mode-face '(:family "Bitstream Vera Sans Mono" :height 140 :width semi-expanded))

(global-set-key (kbd "C-f") 'buffer-face-mode)

(dolist (type edity-types)
  (add-hook (type->hook type)
			#'(lambda ()
				(require 'xmlstuff))))

(line-number-mode 1)
(column-number-mode 1)
(global-linum-mode -1)

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
 '(haskell-mode-hook
	 (quote
		(turn-on-haskell-indent turn-on-font-lock setup-yas
														(lambda nil
															(local-set-key
															 (kbd "<return>")
															 (quote newline-and-indent))
															(local-set-key
															 (kbd "C-<return>")
															 (quote newline))
															(local-set-key
															 (kbd "M-<return>")
															 (quote electric-indent-just-newline)))
														buffer-face-mode gitcommit-enhooken)) t)
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
 '(switch-to-visible-buffer t)
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
