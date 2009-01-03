;;; [class A]
;;; [class B]
;;; [class C]
(external print)
(external-class Object ())

(define-class A (Object) ())
(define-class B (Object) ())
(define-class C (Object) ())
(print A)
(print B)
(print C)