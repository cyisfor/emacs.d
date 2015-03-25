(setq hidden-overlays '())

(defun create-hidden-overlay (start end)
  (let ((overlay (make-overlay start end)))
    (setq-local hidden-overlays (cons overlay hidden-overlays))
                                        ;(overlay-put overlay 'face '(background-color . "purple")
    (overlay-put overlay 'invisible t)))

(defun noverlay-quotes ()
  (dolist (overlay hidden-overlays)
    (delete-overlay overlay))
  (setq-local hidden-overlays '()))

(defun hide-quotes (inside)
  (noverlay-quotes)
  (when (not (mark))
    (goto-char (point-min)))
  (let ((inquote nil) (last (point)))
    (cl-loop until (or (eobp)
                       (eq (point) (mark))
                       (not (if inquote
                           (search-forward "”" nil t)
                         (search-forward "“" nil t))))
             do
             (message "derp %s %s %s" inside inquote last)
             (if inquote
                 (when inside
                   (create-hidden-overlay last (- (point) 1)))
               (when (not inside)
                 (create-hidden-overlay last (- (point) 1))))
             (setq last (point))
             (setq inquote (not inquote)))))

(defun hide-around-quotes ()
  (hide-quotes nil))

(defun hide-inside-quotes ()
  (hide-quotes t))

(defun toggle-hide-around-quotes ()
  (interactive)
  (if hidden-overlays
      (noverlay-quotes)
    (hide-around-quotes)))

(provide 'quote-display)

