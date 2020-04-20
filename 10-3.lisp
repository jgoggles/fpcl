;; 10-3.a
(defun rmerge (l1 l2)
  "Merges two ordered lists to produce an ordered list"
  (cond ((null l1) l2)
        ((null l2) l1)
        ((< (car l1) (car l2)) (cons (car l1) (rmerge (cdr l1) l2)))
        (t (cons (car l2) (rmerge l1 (cdr l2))))))

(rmerge `(1 3 5) `(2 4 6))

;; 10-3.b
(defun lmerge (l)
  "Merges a list of ordered lists"
  (if (null l) nil
      (rmerge (car l) (lmerge (cdr l)))))

(lmerge `((2 5 8) (1 4 7) (3 6 9)))
