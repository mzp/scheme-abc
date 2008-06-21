;; expected output
;;;  not rec
;;;  rec
;;;  3628800
;; (let ([f (lambda (n) (print "not rec"))])
;;   (let ([f (lambda (n) (if (= n 0)
;; 			   (print "rec")
;; 			   (f 0)))])
;;     (f 1)))

;; (let ([f (lambda (n) (print "not rec"))])
;;   (letrec ([f (lambda (n) (if (= n 0)
;; 			   (print "rec")
;; 			   (f 0)))])
;;     (f 1)))

;; (letrec ([fact (lambda (n) 
;; 		 (if (<= n 1)
;; 		     1
;; 		     (* n (fact (- n 1)))))])
;;   (print (fact 10)))

;; check if define has lexcical scope
(define (f) "good")
(define (g) (f))
(define (f) "bad")
(print (g))