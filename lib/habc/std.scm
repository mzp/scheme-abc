;;; map

(define-class List (Object) ())
(define-class Cons (List) (head tail))
(define-class Nil  (List) ())

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

(define-method null? ([self Nil])
  #t)

(define-method null? ([self Cons])
  #f)

(define (length xs)
  (if (null? xs)
      0
      (+ 1 (length (cdr xs)))))

(define (map f xs)
  (if (null? xs)
      nil
      (cons (f (car xs))
	    (map f (cdr xs)))))


