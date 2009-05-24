;;; 1
;;; 2
;;; 3

(define-class List (Object) ())
(define-class Cons (List) (head tail))
(define-class Nil (List) ())

(define nil (new Nil))
(define (cons a b)
  (let ([obj (new Cons)])
    (slot-set! obj head a)
    (slot-set! obj tail b)
    obj))

(define-method car ([self Cons])
  (slot-ref self head))

(define-method cdr ([self Cons])
  (slot-ref self tail))

(define-method empty? ([self Nil])
  #t)

(define-method empty? ([self Cons])
  #f)

; -------------------
(define xs (cons 1 (cons 2 (cons 3 nil))))

(define (length xs)
  (if (empty? xs)
      0
      (+ 1 (length (cdr xs)))))
(define (map f xs)
  (if (empty? xs)
      nil
      (cons (f (car xs))
	    (map f (cdr xs)))))

(map trace xs)
