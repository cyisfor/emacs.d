;; fast loading stuff without dependencies
;; that is important to the user interface

(global-set-key (kbd "C-z") ctl-x-map)
(global-set-key (kbd "M-z") 'execute-extended-command)
(global-set-key (kbd "M-k") 'zap-to-char) ; more useful than kill-sentence
;; could set M-x to kill-sentence to complete the circle, meh
(define-key ctl-x-map (kbd "C-b") 'ibuffer)

(defun kill-whole-line-or-region (beg end &optional region direction)
  (interactive (list (mark) (point) 'region) "P")
  (if (region-active-p)
      (kill-region beg end region)
    (if (= (point) (line-end-position))
		(kill-line)
	  (kill-region (point) (line-end-position)))))

(global-set-key (kbd "M-<right>") 'forward-list)
(global-set-key (kbd "M-<left>") 'backward-list)
(global-set-key (kbd "M-<up>") 'up-list)
(global-set-key (kbd "C-k") 'kill-whole-line-or-region)
(global-set-key (kbd "C-w") 'delete-other-windows)
(global-set-key (kbd "M-l") 'redraw-display)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-<pause>") 'delete-trailing-whitespace)
(global-set-key (kbd "C-<backspace>") 'fixup-whitespace)

(global-set-key (kbd "C-g") 'abort-recursive-edit)
(global-set-key (kbd "C-]") 'keyboard-quit)

(dolist (type '(
				backward-kill-sentence
				set-goal-column
				))
  (substitute-key-definition
   type
   'abort-recursive-edit
   (current-global-map)))

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key "\C-co" 'switch-to-minibuffer) ;; Bind to `C-c o'

(require 'exit-saver)

(global-set-key (kbd "<f5>") 'save-buffers)
;;; auggggggh
(define-key ctl-x-map [(s)] 'save-buffers)

(setq-default indent-tabs-mode t)

(add-hook 'python-mode-hook
		  #'(lambda ()
			  (setq
					indent-tabs-mode t
					python-indent-offset 2
					tab-width 2
					py-indent-tabs-mode t)
			  (do-smart-tabs)))

(electric-indent-mode -1)
(savehist-mode 1)


(setq auto-mode-alist (append
											 '(("\.md$" . html-mode)
												 ("\.hish$" . html-mode))
											 auto-mode-alist))

;; these are mine, shouldn't have other dependencies to them
(require 'htmlfixes)
(require 'xmlstuff)
(require 'gitcommit)


(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

(c-add-style "javascript" '("gnu"
													(c-basic-offset . 1)))
(setq c-default-style (cons '(javascript . "javascript")
														c-default-style))

(setq-default c-basic-offset 2
							tab-width 2
							indent-tabs-mode t)


(defadvice other-buffer (before switch-to-visible-dammit
																activate preactivate compile)
	(ad-set-arg 1 t))

(provide 'essential-stuff)
