;; expected output
;;;  not rec
;;;  rec
(let ([f (lambda (n) (print "not rec"))])
  (let ([f (lambda (n) (if (= n 0)
			   (print "rec")
			   (f 0)))])
    (f 1)))

(let ([f (lambda (n) (print "not rec"))])
  (letrec ([f (lambda (n) (if (= n 0)
			   (print "rec")
			   (f 0)))])
    (f 1)))