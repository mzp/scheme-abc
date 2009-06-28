;; Slot access example
;;; 42
;;; 10
;;; 1

(class Foo (Object) (x y z))

(define foo (new Foo))

(slot-set! foo x 42)
(slot-set! foo y 10)
(slot-set! foo z 1)
(trace (slot-ref foo x))
(trace (slot-ref foo y))
(trace (slot-ref foo z))
