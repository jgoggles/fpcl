;; 10.1a
(defun nsum (n)
  "find the sum of integers between 0 and n"
  (if (= n 0)
      0
      (+ n (nsum (- n 1)))))

;; 10.1b
(defun nprod (n)
  "find the product of the integers between 1 and n"
  (if (= n 1)
      1
      (* n (nprod (- n 1)))))

;; 10.1c
(defun napply (fun n)
  "find the sum of applying fun to the numbers between 0 and n"
  (if (= n 0)
      (funcall fun 0)
      (+ (funcall fun n) (napply fun (- n 1)))))

;; 10.1d
(defun nsapply (fun n s)
  "find the sum of applying fun to the numbers between 0 and n in steps of s"
  (if (<= n 0)
      (funcall fun 0)
      (+ (funcall fun n) (nsapply fun (- n s) s))))

(defun sq (n) (* n n))

(nsapply #'sq 10 2)
