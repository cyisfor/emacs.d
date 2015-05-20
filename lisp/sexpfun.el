(require 'macro-utils) ;; once-only

(defmacro checkthp (predicate compare a b)
  (once-only (a b)
    `(let ((isb (,predicate ,b)))
       (if (,predicate ,a)
           (if isb
               (,compare ,a ,b)
             (progn
               (message (prin1-to-string (list "not isb" (quote ,predicate) ,a ,b)))
               t))
         (progn
           (message (prin1-to-string '("not isa" ,predicate ,a ,b)))
           nil)))))

(defmacro check (a b &rest rest)
  `(or ,@(cl-map 'list
                 #'(lambda (clause)
                     (message "um %s" (prin1-to-string (cons 'uhhhh clause)))
                     (list 'checkthp (car clause) (cadr clause) a b))
                 rest)))
    
(defun test-check ()
  (let ((checks
         '((23 42)
           ((1 2 3) (a b c))
           ((1 2 3) (3 2 1))
           ((3 2 1) "contact")
           ("111" "9" "66")))
        (lexical-binding nil))
    (let ((compare
           (lambda (q p)
             (message (prin1-to-string (list 'umm q p (stringp q) (stringp p))))
             (check q p
                    (consp (lambda (a b) compare (car q) (car p)))
                    (numberp <)
                    (stringp (lambda (a b)
                               (message "both strings %S %S" a b)
                               (string< a b)))))))
      (fset 'compare compare)
      (dolist (check checks)
        (message "sorted %S" (sort check compare))))))

;; (defun compare-stuff (a b)
;;   (check
;;    a b
;;    (numberp (< a b))
;;    (stringp (compare-strings a nil nil b nil nil t) a b)
;;    (consp (check (car a) (car b)) a b))
;;   (if (numberp a)
;;       (if (numberp b)
;;           (< a b)
;;         t)
;;     (if (numberp b)
;;         nil
;;       (let ((cmp 
;;         (and (not (eq cmp t)) (< cmp 0))))))

;; (defun sort-current-sexp ()
;;   (interactive)
;;   (save-excursion
;;    (up-list)
;;    (push-mark)
;;    (backward-sexp)
;;    (let ((sexp (read (current-buffer))))
;;      (if (consp sexp)
;;          (let ((sorted
;;                 (if (and (cdr sexp)
;;                          (eq (cddr sexp) nil)
;;                          (eq (car sexp) 'quote))
;;                     (cons (car sexp) (sort (cadr sexp)))
;;                   (sort sexp 'compare))
;;    (message (prin1-to-string 

(defun derp ()
    (let (herp)
      (fset 'herp (lambda () 42))
      (herp)))
(derp)
