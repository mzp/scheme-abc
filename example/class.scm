;; Example for class definition
;;; 42
;;; 12
;;; [object Foo]
;;; 10
;;; 10
;;; [object Foo]


(define-class Foo (Object) ())
(define-method init ([self Foo] x)
  (print x)
  (let ((t 10)) 
    (let ((t 12))
      (print t))))

(define-method f ((self Foo) x)
  (print x))

(define-method g ((self Foo))
  (print self))

(define foo (new Foo 42))
(print foo)
(. foo (f 10))
(f foo 10)
(. foo (g))