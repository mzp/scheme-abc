(class List (Object) ())

(class Cons (List) (head tail)
       (method null? (self)
	       #f))

(class Nil  (List) ()
       (method null? (self)
	       #t))

(define (cons a b)
  (let ([obj (new Cons)])
    (slot-set! obj head a)
    (slot-set! obj tail b)
    obj))

(define (car pair)
  (slot-ref pair head))

(define (cdr pair)
  (slot-ref pair tail))

(define nil (new Nil))
(define (null? x)
  (. x (null?)))

(define (length xs)
  (if (null? xs)
      0
      (+ 1 (length (cdr xs)))))

(define (map f xs)
  (if (null? xs)
      nil
      (cons (f (car xs))
	    (map f (cdr xs)))))


