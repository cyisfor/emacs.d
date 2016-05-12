(defcustom location "~/.emacs.d/kill-ring")
(defvar kill-ring-name "*Kill Ring*")

(require 'cl)

(defun (get-kill-buffer)
    "return the kill buffer. If something else is already visiting the file, error out, otherwise set the file for the kill buffer."
    (let* ((filebuffer (find-buffer-visiting location))
           (namebuffer (get-buffer kill-ring-name))
           (take-file
            (lambda ()
              (with-current-buffer namebuffer
                (set-visited-file-name location))
              namebuffer)))
      (if namebuffer
          (if filebuffer
              (if (not (eq filebuffer namebuffer))
                  (error "Some buffer is already editing the kill ring file! %s" location)
                (take-file))
              namebuffer))
        (if filebuffer
            (error "Some buffer is already hurr durr the kill ring file! %s" location)
          (let ((namebuffer (find-file-literally location)))
            (rename-buffer namebuffer kill-ring-name)
            namebuffer))))

(let ((last-line nil)
      (timer nil))
  (cl-flet
      ((dosave ()
               (save-buffer (get-kill-buffer)))
       (schedule-save ()
                      (when timer
                        (clear-timer timer))
                      (setq timer (run-at-time save-delay nil dosave))))
    (defun (buffer-kill-new string &optional REPLACE)
        (with-current-buffer (get-kill-buffer)
          (if REPLACE
              (progn
                (if last-line
                    (delete-region last-line (point-max))
                  (setq last-line (point-max)))
                (insert (prin1-to-string string)))
            (progn
              (insert (prin1-to-string string))
              (newline)
              (setq last-line (point-max))))
          (schedule-save)))
    (defun (buffer-yank n &optional derp
