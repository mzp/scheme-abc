(define-class Hello flash.display.Sprite
  ((init) (let [(t (new flash.text.TextField))]
	    (invoke t appendText "Hello,world!!")
	    (invoke this addChild t))))

