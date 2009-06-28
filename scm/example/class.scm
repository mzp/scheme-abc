;; Example for class definition
;;; 42
;;; 12
;;; [object Foo]
;;; 10
;;; 10
;;; [object Foo]

(class Foo (Object) (x y)
  (method init (self x)
	  (trace x)
	  (let ((t 10))
	    (let ((t 12))
	      (trace t))))
  (method f (self x)
	  (trace x))
  (method g (self)
	  (trace self)))

(define foo (new Foo 42))
(trace foo)
(. foo (f 10))
(. foo (f 10))
(. foo (g))