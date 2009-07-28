(class List (Object) ()
       (method list? (self)
	       #t))

(class Cons (List) (head tail)
       (method pair? (self)
	       #t)
       (method null? (self)
	       #f)
       (method toString (self)
	       (+. "("
		   (+. (. self (toStringImpl))
		       ")")))
       (method toStringImpl (self)
	       (+. (. (slot-ref self head) (toString))
		   (if (. (slot-ref self tail) (null?))
		       ""
		       (+. " " (. (slot-ref self tail) (toStringImpl)))))))

(class Nil  (List) ()
       (method pair? (self)
	       #f)
       (method null? (self)
	       #t)
       (method toString (self)
	       "()")
       (method toStringImpl (self)
	       ""))

(define (cons a b)
  (let ([obj (new Cons)])
    (slot-set! obj head a)
    (slot-set! obj tail b)
    obj))

(define (car pair)
  (slot-ref pair head))

(define (cdr pair)
  (slot-ref pair tail))

(define (caar xs) (car (car xs)))
(define (cdar xs) (cdr (car xs)))
(define (cadr xs) (car (cdr xs)))
(define (cddr xs) (cdr (cdr xs)))
(define (caaar xs) (car (car (car xs))))
(define (cdaar xs) (cdr (car (car xs))))
(define (cadar xs) (car (cdr (car xs))))
(define (cddar xs) (cdr (cdr (car xs))))
(define (caadr xs) (car (car (cdr xs))))
(define (cdadr xs) (cdr (car (cdr xs))))
(define (caddr xs) (car (cdr (cdr xs))))
(define (cdddr xs) (cdr (cdr (cdr xs))))
(define (caaaar xs) (car (car (car (car xs)))))
(define (cdaaar xs) (cdr (car (car (car xs)))))
(define (cadaar xs) (car (cdr (car (car xs)))))
(define (cddaar xs) (cdr (cdr (car (car xs)))))
(define (caadar xs) (car (car (cdr (car xs)))))
(define (cdadar xs) (cdr (car (cdr (car xs)))))
(define (caddar xs) (car (cdr (cdr (car xs)))))
(define (cdddar xs) (cdr (cdr (cdr (car xs)))))
(define (caaadr xs) (car (car (car (cdr xs)))))
(define (cdaadr xs) (cdr (car (car (cdr xs)))))
(define (cadadr xs) (car (cdr (car (cdr xs)))))
(define (cddadr xs) (cdr (cdr (car (cdr xs)))))
(define (caaddr xs) (car (car (cdr (cdr xs)))))
(define (cdaddr xs) (cdr (car (cdr (cdr xs)))))
(define (cadddr xs) (car (cdr (cdr (cdr xs)))))
(define (cddddr xs) (cdr (cdr (cdr (cdr xs)))))

(define nil (new Nil))
(define (null? x)
  (. x (null?)))
(define (pair? x)
  (. x (pair?)))
(define (list? x)
  (. x (list? x)))

(define (length xs)
  (if (null? xs)
      0
      (+ 1 (length (cdr xs)))))

(define (map f xs)
  (if (null? xs)
      nil
      (cons (f (car xs))
	    (map f (cdr xs)))))

(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs)
	    (append (cdr xs) ys))))

(define (reverse xs)
  (letrec [(iter (lambda(xs ys)
		   (if (null? ys)
		       xs
		       (iter (cons (car ys) xs) (cdr ys)))))]
    (iter nil xs)))

(define (memq obj list)
  (cond [(null? list) #f]
	[(= obj (car list)) #t]
	[else (memq obj (cdr list))]))

(define (assoc obj list)
  (cond [(null? list) #f]
	[(= obj (caar list)) (car list)]
	[else (assoc obj (cdr list))]))

(define (fold f init xs)
  (if (null? xs)
      init
      (fold f (f init (car xs)) (cdr xs))))

(define (for-each f xs)
  (if (null? xs)
      nil
      (begin (f (car xs))
	     (for-each f (cdr xs)))))

(define (for-each-with-index-sub i f xs)
  (if (null? xs)
      nil
      (begin (f i (car xs))
	     (for-each-with-index-sub (+ i 1) f (cdr xs)))))

(define (for-each-with-index f xs)
  (for-each-with-index-sub 0 f xs))
