(define-class Hello flash.display.Sprite
  ((init) (let [(t (new flash.text.TextField))]
	    (invoke t appendText "http://happy-abc.org")
	    (invoke this addChild t))))

42