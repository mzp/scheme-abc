;;; 42
(define-class Foo (Object) ())
(define-static-method f ([self Foo])
  42)

(f Foo)