;; Slot access example
;;; 42
(define-class Foo (Object) (x))

(define foo (new Foo))

(slot-set! foo x 42)
(print (slot-ref foo x))
