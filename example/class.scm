;; Example for class definition
;;; [class Foo]
;;; 42
;;; [object Foo]
;;; 10

(define-class Foo Object
  ((init x) (let ((t 10)) 
	      (let ((t 12))
		(print t))))
  ((f    x) (print x)))

(define foo (new Foo 42))
(print foo)