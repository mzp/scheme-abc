;;; 42
;;; 10

(external print)

(define (f)
  (let ([x 42])
    (lambda () x)))

(define (const x)
  (lambda (y)
    x))

(print ((f)))
(print ((const 10) 20))