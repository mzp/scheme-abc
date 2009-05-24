;; Example for class definition
;;; 42
;;; 12
;;; [object Foo]
;;; 10
;;; 10
;;; [object Foo]

(define-class Foo (Object) (x y))
(define-method init ([self Foo] x)
  (trace x)
  (let ((t 10))
    (let ((t 12))
      (trace t))))

(define-method f ((self Foo) x)
  (trace x))

(define-method g ((self Foo))
  (trace self))

(define foo (new Foo 42))
(trace foo)
(. foo (f 10))
(f foo 10)
(. foo (g))