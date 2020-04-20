(defun val (node) (car node))
(defun left (node) (cadr node))
(defun right (node) (caddr node))

;; 10.5a
(defun tcomp (t1 t2)
  "Compares two integer btrees"
  (cond ((and (null t1) (null t2)) t)
        ((or (null t1) (null t2)) nil)
        ((= (val t1) (val t2)) (and
                                (tcomp (left t1) (left t2))
                                (tcomp (right t1) (right t2))))
        (t nil)))
(tcomp
 `(1 (2 (4 () ()) (6 () ())) (3 (5 () ()) (7 () ())))
 `(1 (2 (4 () ()) (6 () ())) (3 (5 () ()) (7 () ())))
 )

;; 10.5b
(defun tfind (t1 t2)
  "indicates if t1 is a subtree of t2"
  (cond ((null t1) t)
        ((null t2) nil)
        ((tcomp t1 t2) t)
        ((< (val t1) (val t2)) (tfind t1 (left t2)))
        (t (tfind t1 (right t2)))))

(tfind
 `(3 (5 () ()) (7 () ()))
 `(1 (2 (4 () ()) (6 () ())) (3 (5 () ()) (7 () ())))
 )

;; 10.5c
(defun dtraverse (tree)
  "traverses a btree to produce a list of node values in desc order"
  (if (null (val tree))
      nil
      (append (dtraverse (right tree))
              (cons (val tree) (dtraverse (left tree))))))

(dtraverse `(5 (4 (3 () ()) ()) (10 (6 () ()) (20 () ()))))
