;;; hello
;;; [object Foo]
(define-class Foo Object
  ((init) (print "hello")))

(let ([k (new Foo)])
  (print k))


