(define-class Main (Flash.Display.Sprite) ())
(define-method init ([self Main])
  (let [(t (new Flash.Text.TextField))]
    (. t (appendText "Hello,world!!"))
    (. self (addChild t))))

