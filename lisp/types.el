(setq edity-types '(text))
(setq programmy-types '(c python lisp lua perl java javascript))
(defun type->hook (type)
  (intern (concat (symbol-name type) "-mode-hook")))
(provide 'types)