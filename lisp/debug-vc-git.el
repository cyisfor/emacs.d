(defun vc-git-mode-line-string (file)
  "Return a string for `vc-mode-line' to put in the mode line for FILE."
  (let* ((rev (vc-working-revision file))
         (detached (vc-file-getprop file 'vc-git-detached))
         (def-ml (vc-default-mode-line-string 'Git file))
         (help-echo (get-text-property 0 'help-echo def-ml)))
		(condition-case e
				(propertize (if detached
												(if (eq rev nil)
														def-ml
													(substring def-ml 0 (- 7 (length rev))))
											def-ml)
										'help-echo
										(if (eq rev nil)
												help-echo
											(concat help-echo "\nCurrent revision: " rev)))
			(args-out-of-range
			 (error "args out of range meh %s %s %s %s"
							rev detached def-ml help-echo)))))
