(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "Linux")
            (setq-local c-basic-offset 4)
            (setq-local tab-width 4)
            (setq-local c-toggle-hungry-state t)
            (setq-local indent-tabs-mode t)))

;; http://emacswiki.org/emacs/guess-offset.el
(require 'guess-offset)

(package-require 'smart-tabs-mode)
(smart-tabs-insinuate 'c 'javascript)

(provide 'c-stuff)
