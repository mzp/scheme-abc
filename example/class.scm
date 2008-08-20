;;; [class Foo]
;;; 42
(define-class Foo Object
  ((init x) (print x))
  ((f    x) (print x)))

(print Foo)
(new Foo 42)