;;; 12
;;; 10
;;; 42
;;; 12

(module Foo ()
	(class Foo (Object) (x y)
	       (method init (self x)
		       (trace x)
		       (let ((t 10))
			 (let ((t 12))
			   (trace t))))
	       (method f (self x)
		       (trace x)))
	(define x 10)
	(define (g) Foo.x)
	(define x 12))

;; scope
(trace Foo.x)
(trace (Foo.g))

;; class
(define obj (new Foo.Foo 42))
;;(f obj 1)
