(define-class Rect (flash.display.Shape) ())
(define-method init ([self Rect])
  (let ([g (slot-ref self graphics)])
    (beginFill g 0xFF0000 0.5)
    (drawRect g 0 0 40 50)))


(define (for-each from to f)
  (if (= from to)
      0
      (begin
	(f from)
	(for-each (+ from 1) to f))))

(define-class Main (flash.display.Sprite) ())
(define-method init ([self Main])
  (for-each  0 20
	     (lambda (i)
	       (let ([r     (new Rect)]
		     [angle (/ (*. i (*. 2.0 3.14)) 20)])
		 (slot-set! r x
			    (+ 300.0 (*. 100.0 (cos Math angle))))
		 (slot-set! r y
			    (+ 300.0 (*. 100.0 (sin Math angle))))
		 (addChild self r)))))


