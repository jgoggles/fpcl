;; 10.2a
(defun lstarts (l1 l2)
  "returns true if l2 starts with l1"
  (cond ((null l1) t)
        ((null l2) nil)
        ((= (car l1) (car l2)) (lstarts (cdr l1) (cdr l2)))
        (t nil)))

;; 10.2b
(defun lcontains (l1 l2)
  "returns true if l1 is contained in l2"
  (cond ((null l2) nil)
        ((lstarts l1 l2) t)
        (t (lcontains l1 (cdr l2)))))

;; 10.2c
(defun lcount (l1 l2)
  "counts how often l1 appears in l2"
  (cond ((null l2) 0)
        ((lstarts l1 l2) (1+ (lcount l1 (cdr l2))))
        (t (lcount l1 (cdr l2)))))

;; 10.2d
(defun lremove (l1 l2)
  "remove l1 from the start of l2 assuming you know l1 starts l2"
  (if (null l1)
      l2
      (lremove (cdr l1) (cdr l2))))

;; 10.2e
(defun ldelete (l1 l2)
  "remove the first occurrence of l1 in l2"
  (cond ((null l2) nil)
        ((lstarts l1 l2) (lremove l1 l2))
        (t (cons (car l2) (ldelete l1 (cdr l2))))))

;; 10.2f
(defun linsert (l1 l2 l3)
  "insert l1 into l3 after first occurrence of l2"
  (cond ((null l3) nil)
        ((lstarts l2 l3) (append l2 (append l1 (lremove l2 l3))))
        (t (cons (car l3) (linsert l1 l2 (cdr l3))))))

;; 10.2g
(defun lreplace (l1 l2 l3)
  "replace l1 with l2 in l3"
  (cond ((null l3) nil)
        ((lstarts l1 l3) (append l2 (lremove l1 l3)))
        (t (cons (car l3) (lreplace l1 l2 (cdr l3))))))

(lreplace `(4 5) `(1 2) `(1 2 3 4 5 6 7))
