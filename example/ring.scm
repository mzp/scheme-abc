(define-class Rect (flash.display.Shape) ())
(define-method init ([self Rect])
  (let ([g (slot-ref self graphics)])
    (beginFill g 0xFF0000 0.5)
    (drawRect g 0 0 100 100)))


(define (for-each from to f)
  (if (= from to)
      0
      (begin
	(f from)
	(for-each (+ from 1) to f))))

(define-class Main (flash.display.Sprite) ())
(define-method init ([self Main])
  (trace (. Math (abs 10)))
  (for-each  0 10
	     (lambda (i)
	       (let ([r (new Rect)])
		 (slot-set! r x (* i 100))
		 (slot-set! r y (* i 100))
		 (addChild self r)))))

