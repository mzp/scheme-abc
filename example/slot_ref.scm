;; Slot access example
;;; 42
;;; 10
;;; 1
(define-class Foo (Object) (x y z))

(define foo (new Foo))

(slot-set! foo x 42)
(slot-set! foo y 10)
(slot-set! foo z 1)
(print (slot-ref foo x))
(print (slot-ref foo y))
(print (slot-ref foo z))
