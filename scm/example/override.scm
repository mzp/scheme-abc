;;; 4
(define-class Foo (Object) ())
(define-method f ((self Foo) x))


(define-class Bar (Foo)    ())
(define-method f ((self Bar) x)
  (+ x 1))

(trace (f (new Bar) 3))
