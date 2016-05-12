(require 'cl)
(require 'sexpfun) ;; for compare-stuff

;; assoc-add and assoc-union assume the alists are proper
;; i.e. has unique keys and is lexically sorted

(defun assoc-< (a b)
  (compare-stuff
   (car a)
   (car b)))

(defun assoc-union (a b)
  (let ((result nil))
    ;; emacs suuucks
    (cl-block nil 
      (while (or a b)
        (print (list "uhh" a b result))
        (when (not a)
          (cl-return (append (reverse result) b)))
        (when (not b)
          (cl-return (append (reverse result) a)))
        (let ((ahead (car a))
              (bhead (car b)))
          (if (eq (car ahead) (car bhead))
              (progn
                (setq result (cons bhead result))
                (setq a (cdr a))
                (setq b (cdr b)))
            (if (assoc-< ahead bhead)
                (progn
                  (setq result (cons ahead result))
                  (setq a (cdr a)))
              (progn
                (setq result (cons bhead result))
                (setq b (cdr b)))))))
      (reverse result))))

(defun assoc-unique (l)
  (prog1 l
    (while l
      (let ((key (car (car l)))
            (tail (cdr l)))
        (while (and tail (eq key (car (car tail))))
          (setcdr l (cdr tail))
          (setq tail (cdr l)))
        (setq l tail)))))

(defun assoc-add (i l)
  ;; emacs sucks
  (cl-block nil
    (when (eq l nil)      
      (cl-return (list i)))
    (let ((key (car i))
          (cur l)
          (last nil))
      (while cur
        (let ((head (car cur)))
          (if (eq key (car head))
              (progn
                (setcdr head (cdr i))
                (cl-return l))
            (when (assoc-< i head)
              ;; passed where the element would be
              ;; i should insert before head
              (progn
                (if last
                    (progn
                      (setcdr last (cons i cur))
                      (cl-return l))
                  (cl-return (cons i l)))))))
        (setq last cur)
        (setq cur (cdr cur))))))

(defun proper-alist (l)
  (assoc-unique
   (sort l 'assoc-<)))
(error "emacs sucks and attempt to modify read only object")

(defmacro assert-equal (expected actual)
  (let ((op (list 'quote actual)))
    `(let ((expected ,expected)
           (actual ,actual))
       (when (not (equal expected actual))
         (print (list ,op "Expected" expected "got" actual))
         (error "%s" ,op)))))

(defun test-assoc-add ()
  (let ((standard-output (get-buffer-create "*scratch*"))
        (print-quoted t))
    (assert-equal
     '((a . 3)
       (b . 2)
       (c . 5)
       (d . 6))
     (assoc-add '(b . 2)
                '((a . 3)
                  (b . 4)
                  (c . 5)
                  (d . 6))))
    (assert-equal
     '((a . 5)
       (b . 3)
       (c . 6)
       (d . 7))
     (assoc-union
      '((a . 2)
        (b . 3)
        (c . 4))
      '((a . 5)
        (c . 6)
        (d . 7))))
    (assert-equal
     '((a . b)
       (c . d)
       (e . f))
     (proper-alist
      '((a . b)
        (e . f)
        (c . d))))))
(test-assoc-add)

(provide 'assoc-add)
