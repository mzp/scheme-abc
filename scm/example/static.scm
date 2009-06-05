;;; 42
(define-class Foo (Object) ())
(define-static-method f (Foo)
  42)

(trace (f Foo))
