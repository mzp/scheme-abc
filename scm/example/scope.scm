;;;  3628800
(module foo ()
	(define (fact n)
	  (if (<= n 1)
	      1
	      (* n (fact (- n 1))))))
(trace (foo.fact 10))
