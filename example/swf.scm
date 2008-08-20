(define-class Hello flash.display.Sprite
  ((init) (let [(t (new flash.text.TextField))]
	    (invoke t appendText "Happy-abc")
	    (invoke this addChild t))))

42