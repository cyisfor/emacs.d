;; -*- lexical-binding: t; -*-

;;; usage: (require 'lazyclipboard)

;; when we append to the kill ring, if it creates a new
;; x-selection in the clipboard manager for every append operation,
;; you'll get a history like
;; '("this is","this is\ntwo lines","this is\ntwo lines\nthree lines","this is\ntwo lines\nthree lines\nso redundant")
;; in your clipboard manager. This turns one copy of a lot of C-k's into a dozen clipboard entries,
;; filling up my clipboard history.

;; So instead, just wait 2s before considering a kill ring append worthy of making a new clipboard
;; entry.
;; New kills will cancel this timer and immediately copy to clipboard, so this only changes
;; kill-append, i.e. C-k repeatedly, or the like.

;; ...still need to override kill-new to cancel that timer though.

(require 'cl)

(let ((loadup
       (lambda () 
         (when interprogram-cut-function
           (let* ((save-x-select "copy to the X clipboard")
                  (timer nil)
                  (pending-selection nil)
                  (select-it (lambda ()
                               ;;(message "here we goooo %s" (current-kill 0))
                               (save-x-select pending-selection)
                               (setq timer nil)))
                  (force-timer (lambda ()
                                 (when timer
                                   (cancel-timer timer)
                                   (select-it))))
                  ;; at first kill-new is called with replace nil, and the first bit
                  ;; then kill-new is called with replace t, and the text plus appended
                  ;; kill-append is also called redundantly (and uselessly)
                  ;; finally kill-new is called with replace nil, and the next thing
                  ;; so, if replace is nil, then save the selection
                  ;; then kill (pushing a new entry into the ring)
                  ;; also reset the timer.
                  ;; if replace is t, then don't save the selection, as it's assuredly an append
                  ;; just reset the timer and kill
                  (my-kill-new
                   (lambda (old-kill-new something &optional replace)
                     "kill, then copy to X clipboard in about 2 seconds of idleness"
                     
                     (when (eq replace nil)
                       ;; a new kill is ready to happen, so x-select the old one first
                       ;; ...don't if the old one is just going to be replaced.
                       (force-timer))
                     ;;(message "kill new %s %s" something replace)
                     (let ((interprogram-cut-function nil)) ; make sure not to cut to clipboard the old way
                       (funcall old-kill-new something replace))
                     ;; we have to save the pending selection ourselves, because we may need
                     ;; to copy it to the clipboard, during advice before (current-kill)
                     (setq pending-selection something)
                     (setq timer (run-at-time "2 sec" nil select-it))))
                  (pop-on-current-kill
                   (lambda (n &optional do-not-move)
                     "flush pending X copy operation"
                     ;; current-kill will immediately paste from the clipboard, and create a new kill
                     ;; so we need to copy to the clipboard before current kill is called, if we're pending
                     ;; copying on hopes of a kill append.
                     (force-timer))))
             ;; we're handling interprogram paste, so we can't let this variable have a value.
             ;; (otherwise you get the old behavior, plus ours appending MORE entries to the clipboard manager)
             (fset 'save-x-select interprogram-cut-function) ; meh!
             (fset 'select-it select-it) ; blhlh
             (fset 'force-timer force-timer) ; blhlh
             ;; push the current X selection into our kill ring on load, for convenience
             (let ((interprogram-paste
                    (and interprogram-paste-function
                         (funcall interprogram-paste-function))))
               (when interprogram-paste
                 (let ((interprogram-cut-function nil))
                   (if (listp interprogram-paste)
                       (mapc 'kill-new (nreverse interprogram-paste))
                     (kill-new interprogram-paste)))))
             (advice-add 'kill-new :around my-kill-new)
             (advice-add 'current-kill :before pop-on-current-kill))))))
  (if after-init-time
      (funcall loadup)
    (add-hook 'after-init-hook loadup)))

(provide 'lazyclipboard)
