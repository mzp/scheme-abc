;;; 3
;;; 2


(let ((inc (lambda (x) (+ 1 x))))
  (trace (inc 2)))

(trace ((lambda (x) (+ x 1)) 1))