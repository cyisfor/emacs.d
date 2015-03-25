(push "~/.emacs.d/lisp" load-path)

(savehist-mode 1)
(require 'savekill)
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
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-<return>") 'delete-trailing-whitespace)
(autoload 'bison-mode "bison-mode.el")
(autoload 'flex-mode "flex-mode.el")

(setq auto-mode-alist (append
		       '(("\\.y$" . bison-mode)
			 ("\\.l$" . flex-mode))
		       auto-mode-alist))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor-type (quote bar))
 '(global-visual-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(safe-local-variable-values (quote ((encoding . utf8))))
 '(track-eol t)
 '(visual-line-fringe-indicators (quote (nil right-curly-arrow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Bitstream Vera Sans" :foundry "bitstream" :slant normal :weight normal :height 143 :width normal)))))
