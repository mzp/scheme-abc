(open flash.display)

(define (split-decimal n)
  (list (/ n 10)
	(remainder n 10)))

(define (split-bits-sub xs n)
  (if (<= n 1)
      (reverse (cons n xs))
      (split-bits-sub (cons (remainder n 2) xs)
		      (/ n 2))))

(define (split-bits n)
  (split-bits-sub nil n))

(define (time)
  (let ([t (new Date)])
    (map split-bits
	 (fold append
	       nil
	       (map split-decimal
		    (list (. t (getHours))
			  (. t (getMinutes))
			  (. t (getSeconds))))))))

(define (draw-digit g x y ns)
  (for-each-with-index
   (lambda (i n)
     (if (= n 1)
	 (begin
	   (. g (beginFill 0xFF0000))
	   (. g (drawCircle x (- y (* i 50)) 20))
	   (. g (endFill)))
	 nil))
   ns))


(class Main (Sprite) ()
        (method init (self)
		(let ([filter (new flash.filters.BlurFilter)])
		  (slot-set! self filters (array filter))
		  (. self (addEventListener "enterFrame"
					    (lambda (e)
					      (. self (show (time))))))))
        (method show (self time)
		(let [(g (slot-ref self graphics))]
		  (. g (clear))
		  (for-each-with-index (lambda (i digit)
					 (draw-digit g (* (+ i 1) 50) 200 digit))
				       time))))
