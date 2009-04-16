(define-class Main (flash.display.Sprite) ())
(define-method init ([self Main])
  (let [(t (new flash.text.TextField))]
    (. t (appendText "Hello,world!!"))
    (. self (addChild t))))

