(class Main (flash.display.Sprite) ()
       (method init (self)
	       (let [(t (new flash.text.TextField))]
		 (. t (appendText "Hello,world!!"))
		 (. self (addChild t)))))

