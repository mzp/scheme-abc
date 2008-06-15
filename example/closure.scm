;;; 42
;;; 10
(define (f)
  (let ([x 42])
    (lambda () x)))

(define (const x)
  (lambda (y)
    x))

(define g (f))
(print (g))

(define h (const 10))
(print (h 20))