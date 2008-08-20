;; Example for class definition
;;; [class Foo]
;;; 42
;;; [object Foo]
;;; 10

(define-class Foo Object
  ((init x) (print x))
  ((f    x) (print x)))

(print Foo)
(define foo (new Foo 42))
(print foo)
(invoke foo f 10)