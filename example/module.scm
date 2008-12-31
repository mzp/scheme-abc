;;; 12
;;; 10
;;; 42
;;; 12

(external print)
(external-class Object ())

(module foo ()
	(define-class Foo (Object) (x y))
	 (define-method init ([self Foo] x)
	   (print x)
	   (let ((t 10))
	     (let ((t 12))
	       (print t))))

	 (define-method f ((self Foo) x)
	   (print x))
	 (define x 10)
	 (define (g) foo.x)
	 (define x 12))

;; scope
(print foo.x)
(print (foo.g))

;; class
(define obj (new foo.Foo 42))
;;(f obj 1)
