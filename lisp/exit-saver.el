(defun save-buffers ()
  (interactive)
  (dolist (buffer (buffer-list))
    (when (buffer-modified-p buffer)
      (with-current-buffer buffer
        (when (not (eq nil buffer-file-number))
          ;; has a file, file is modified
          (save-buffer))))))

(defun save-kill-emacs ()
  (interactive)
  (save-buffers)
  (kill-emacs))

(provide 'exit-saver)
