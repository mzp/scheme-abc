(define-class List (Object) ())
(define-class Cons (List) (head tail))
(define-class Nil  (List) ())

(define-method null? ([self Nil])
  #t)

(define-method null? ([self Cons])
  #f)

(define-method length ([self Nil])
  0)

(define-method length ([self Cons])
  (+ 1 (length (slot-ref self tail))))

(define nil (new Nil))

(define (cons a b)
  (let ([obj (new Cons)])
    (slot-set! obj head a)
    (slot-set! obj tail b)
    obj))

(define (car pair)
  (slot-ref pair head))

(define (cdr pair)
  (slot-ref pair tail))


(define (map f xs)
  (if (null? xs)
      nil
      (cons (f (car xs))
	    (map f (cdr xs)))))


