(defun alist->hash-table (a)
    (let ((table (make-hash-table :test 'equal)))
      (while a
        (puthash (car (car a)) (cdr (car a)) table)
        (setq a (cdr a)))
      table))

(defun hash-table->alist (table)
    (let ((result nil))
      (maphash
       #'(lambda (k v)
           (setq result (cons (cons k v) result)))
       table)
      result))

(defun hash-table-extend (table new)
    (when (not (hash-table-p new))
      (setq new (alist->hash-table new)))
  (maphash
   #'(lambda (k v)
       (puthash k v table))
   new)
  table)

(defun alist-uniquify (derp)
    (hash-table->alist (alist->hash-table derp)))

(provide 'hash-table-stuff)
