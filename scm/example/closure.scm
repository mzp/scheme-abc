;;; 42
;;; 10

(define (f)
  (let ([x 42])
    (lambda () x)))

(define (const x)
  (lambda (y)
    x))

(trace ((f)))
(trace ((const 10) 20))