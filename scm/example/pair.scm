;;; 1
;;; 2
;;; 3
;;; 2


(define x (cons 1 2))
(trace (car x))
(trace (cdr x))

(define xs (list 1 2 3))
(trace (length xs))
(trace (cadr xs))