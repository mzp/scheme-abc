;;; 3
;;; 2
(external +)
(external print)

(let ((inc (lambda (x) (+ 1 x))))
  (print (inc 2)))

(print ((lambda (x) (+ x 1)) 1))