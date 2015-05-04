(global-set-key (kbd "C-z") ctl-x-map)

(defun kill-visual-line-or-region (beg end &optional region direction)
  (interactive (list (mark) (point) 'region) "P")
  (if (region-active-p)
      (kill-region beg end region)
    (kill-visual-line direction)))

(global-set-key (kbd "M-<right>") 'forward-list)
(global-set-key (kbd "M-<left>") 'backward-list)
(global-set-key (kbd "C-k") 'kill-visual-line-or-region)
(global-set-key (kbd "C-w") 'delete-other-windows)
(global-set-key (kbd "M-l") 'redraw-display)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-<pause>") 'delete-trailing-whitespace)
(global-set-key (kbd "C-<backspace>") 'fixup-whitespace)

(global-set-key (kbd "C-g") 'abort-recursive-edit)
(global-set-key (kbd "C-]") 'keyboard-quit)

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key "\C-co" 'switch-to-minibuffer) ;; Bind to `C-c o'

(push "~/packages/bzr/python-mode/" load-path)
(push "~/.emacs.d/lisp" load-path)

(require 'python-mode)
(require 'types)

(dolist (type programmy-types)
         (add-hook (type->hook type)
                   (if (eq type 'c)
                       '(lambda ()
                          (local-set-key (kbd "<return>") 'c-indent-new-comment-line)
                          (local-set-key (kbd "C-<return>") 'newline))
                          (local-set-key (kbd "M-<return>") 'electric-indent-just-newline))
                     '(lambda ()
                        (local-set-key (kbd "<return>") 'newline-and-indent)
                        (local-set-key (kbd "C-<return>") 'newline)
                        (local-set-key (kbd "M-<return>") 'electric-indent-just-newline))))
                                        
(savehist-mode 1)
;(require 'savekill)
(require 'lazyclipboard)

(require 'quote-display)
(global-set-key (kbd "M-q") 'toggle-hide-outside-quotes)

(require 'undo-tree)
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

(setq undo-limit 800000)

(setq auto-mode-alist (append
		       '(("\.lua$" . lua-mode)
			 ("\.hish$" . html-mode))
		       auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua Editing mode." t)

(require 'xmlstuff)
(require 'gitcommit)

(setq buffer-face-mode-face '(:family "Bitstream Vera Sans Mono" :height 140 :width semi-expanded))

(global-set-key (kbd "C-f") 'buffer-face-mode)

(dolist (type programmy-types)
  (add-hook (type->hook type) 'buffer-face-mode))
(add-hook (type->hook 'dired) 'buffer-face-mode)

(require 'typopunct)
(typopunct-change-language 'english t)

(require 'loctitle)
(message "hi")

(defun my-typo-init ()
  (typopunct-mode 1))

(dolist (type edity-types)
  (add-hook (type->hook type) 'my-typo-init))

(autoload 'bison-mode "bison-mode.el")
(autoload 'flex-mode "flex-mode.el")

(setq auto-mode-alist (append
		       '(("\\.y$" . bison-mode)
			 ("\\.l$" . flex-mode))
		       auto-mode-alist))

(require 'package-require)

(package-require 'bookmark+)
;(package-require 'yasnippet)
;(yas-global-mode 1)

;; (push "~/packages/git/autopair/" load-path)
;; (require 'autopair)
;; (dolist (type programmy-types)
;;          (add-hook (type->hook type)
;;                    'autopair-mode))

;; (require 'python-skeleton)

(push "~/packages/git/rust-mode/" load-path)
(require 'rust-mode)

;; (load "~/packages/git/lyqi/lyqi")
;; (setq auto-mode-alist (append
;;                        '(("\\.ly$" . lyqi-mode)
;;                          ("\\.ily$" . lyqi-mode))
;;                        auto-mode-alist))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "/extra/hacker/.emacs.d/bookmarks")
 '(cursor-type (quote bar))
 '(global-visual-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(max-specpdl-size 3000)
 '(nxml-slash-auto-complete-flag t)
 '(py-complete-function (lambda (&rest args) nil))
 '(py-electric-colon-active-p t)
 '(py-electric-colon-greedy-p t)
 '(py-electric-kill-backward-p t)
 '(safe-local-variable-values (quote ((encoding . utf8))))
 '(track-eol t)
 '(visual-line-fringe-indicators (quote (nil right-curly-arrow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Bitstream Vera Sans" :foundry "bitstream" :slant normal :weight normal :height 143 :width normal))))
 '(py-decorators-face ((t (:inherit font-lock-keyword-face :foreground "orange"))))
 '(py-def-class-face ((t (:inherit font-lock-keyword-face :foreground "hot pink"))))
 '(py-exception-name-face ((t (:inherit font-lock-builtin-face :weight bold))))
 '(py-import-from-face ((t (:inherit font-lock-keyword-face :underline t))))
 '(py-number-face ((t (:inherit default :foreground "orange red"))))
 '(py-object-reference-face ((t (:inherit py-pseudo-keyword-face :foreground "blue"))))
 '(py-pseudo-keyword-face ((t (:inherit font-lock-keyword-face :foreground "lime green"))))
 '(py-try-if-face ((t (:inherit font-lock-keyword-face :weight bold))))
 '(py-variable-name-face ((t (:inherit default :foreground "light sea green")))))
