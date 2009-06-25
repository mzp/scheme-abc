(define-class Rect (flash.display.Shape) ())
(define-method init ([self Rect])
  (let ([g (slot-ref self graphics)])
    (beginFill g 0xFF0000)
    (drawRect g 0 0 40 50)
    (slot-set! self x 0)
    (slot-set! self y 0)
    (endFill g)))



(define-class Main (flash.display.Sprite) ())
(define-method init ([self Main])
  (let ([r (new ring.Rect)]
	[t (new flash.text.TextField)])
    (. self (addChild t))
    (. self (addChild r))
    (. t (appendText "A"))))

