(require 'macro-utils) ;; once-only

(defmacro checkthp (predicate compare a b)
  (once-only (a b)
    ;; once-only for functions is hard...
    (let ((mypred (gensym)))
      `(let (,mypred)
         (fset (quote ,mypred) ,(if (symbolp predicate)
                                    `(function ,predicate)
                                  predicate))
         (and (,mypred ,a)
              (and (,mypred ,b)
                   (,compare ,a ,b)))))))

(defmacro check (a b &rest rest)
  (once-only (a b)
    `(or ,@(cl-map 'list
                   #'(lambda (clause)
                       (list 'checkthp (car clause) (cadr clause) a b))
                   rest))))
    
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
             (check q p
                    (consp (lambda (a b) compare (car q) (car p)))
                    (numberp <)
                    (stringp string<)))))
      (fset 'compare compare)
      (dolist (check checks)
        (message "sorted %S" (sort check compare))))))

(defun compare-stuff (q p)
  ;;(print (list 'compare q p))
  (check q p
         (consp (lambda (a b) (compare (car a) (car b))))
         (numberp <)
         (symbolp (lambda (a b)
                    (string< (symbol-name a) (symbol-name b))))
         (stringp string<)))

(defun quotep (sexp)
  (and
   (not (eq (cdr sexp) nil))
   (eq (cddr sexp) nil)
   (eq (car sexp) 'quote)
   (consp (cadr sexp))))

(defun sort-current-sexp ()
  (interactive)
  (save-excursion
   (up-list)
   (let ((end (point)))
     (backward-sexp)
     (push-mark)
     (let ((start (point)))
       (let ((sexp (read (current-buffer))))
         (if (consp sexp)
             (let ((sorted
                    (if (quotep sexp)
                        (list 'quote (sort (cadr sexp) #'compare-stuff))
                      (sort sexp #'compare-stuff)))
                   (print-quoted t))
               (let ((s (prin1-to-string sorted)))
                 (delete-region start end)
                 (insert s)))))))))
(provide 'sexpfun)
