;;; 42
(define-class Foo (Object) ())
(define-static-method f (Foo)
  42)

(print (f Foo))
