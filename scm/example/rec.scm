;; expected output
;;;  not rec
;;;  rec
;;;  3628800
;;;  3628800

;; check if not recursion
(let ([f (lambda (n) (trace "not rec"))])
  (let ([f (lambda (n) (if (= n 0)
			   (trace "rec")
			   (f 0)))])
    (f 1)))

(let ([f (lambda (n) (trace "not rec"))])
  (letrec ([f (lambda (n) (if (= n 0)
			      (trace "rec")
			      (f 0)))])
    (f 1)))

;; check if recursion
(letrec ([fact (lambda (n)
		 (if (<= n 1)
		     1
		     (* n (fact (- n 1)))))])
  (trace (fact 10)))

;; check if define
(define (fact n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))
(trace (fact 10))