;;;; Times of day represented as:
;;;; (<hours> <minutes> <seconds>)

;; 10.4a.1
(defun to_seconds (time)
  "convert from a time of day to seconds"
  (let ((hours (car time))
        (minutes (cadr time))
        (seconds (caddr time)))
    (+ (* (+ (* hours 60) minutes) 60) seconds)))

(to_seconds `(2 30 25))

;; 10.4a.2
;; (defun from_seconds (seconds)
;;   "convert from seconds to a time of day"
;;   (let* ((sec (rem seconds 60))
;;          (minutes (floor seconds 60))
;;          (hours (multiple-value-list (floor minutes 60))))
;;     (cons (car hours) (append (cdr hours) (cons sec nil)))))

(defun from_seconds (seconds)
  "convert from seconds to a time of day"
  (list (truncate seconds 3600)
        (truncate (rem seconds 3600) 60)
        (rem seconds 60)))

(from_seconds 48975)

;; 10.4b
(defun tick (time)
  "increment time by one second"
  (let ((hours (car time))
        (minutes (cadr time))
        (seconds (1+ (caddr time))))
    (when (> seconds 59)
      (setf seconds 0)
      (setf minutes (1+ minutes))
      (when (> minutes 59)
        (setf minutes 0)
        (setf hours (1+ hours))
        (when (> hours 23)
          (setf hours 0)
          (setf minutes 0)
          (setf seconds 0))))
    (list hours minutes seconds)))

(tick `(23 59 59))

;; 10.4c
;; Transactions are stored in the following format (<item> <timestamp h m s>)
(defun timestamp (transaction) (cadr transaction))
(defun hours (timestamp) (car timestamp))
(defun minutes (timestamp) (cadr timestamp))
(defun seconds (timestamp) (cddr timestamp))

(defun tless (tr1 tr2)
  (let ((t1 (timestamp tr1))
        (t2 (timestamp tr2)))
        (cond ((< (hours t1) (hours t2)) t)
              ((= (hours t1) (hours t2))
               (cond ((< (minutes t1) (minutes t2)) t)
                     ((= (minutes t1) (minutes t2))
                      (< (seconds t1) (seconds t2)))
                     (t nil)))
              (t nil))))

;; (tless `(milk (20 12 12)) `(eggs (13 13 13)))

(defun tinsert (tr l)
  (cond ((null l) (cons tr nil))
        ((tless tr (car l)) (cons tr l))
        (t (cons (car l) (tinsert tr (cdr l))))))

;; (tinsert
 ;; `(milk (12 12 12))
 ;; `((eggs (9 9 9)) (chips (10 10 10)) (bread (20 20 20))))

(defun tsort (transactions)
       (if (null transactions)
           transactions
           (tinsert (car transactions) (tsort (cdr transactions)))))

(tsort `((coffee (0 12 48))
         (milk (3 45 13))
         (chips (3 5 19))
         (onions (10 56 29))))
