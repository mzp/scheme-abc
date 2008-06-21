;;; 42

;; check if define has lexcical scope
(define x 42)
(define (f) x)
(define x 100)

(print (f))