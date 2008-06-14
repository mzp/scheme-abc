(define (f)
  (let ([x 42])
    (lambda () x)))

(define g (f))
(print (g))