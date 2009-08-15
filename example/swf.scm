(define (main stage)
  (let [(t (new flash.text.TextField))]
    (. t (appendText "Hello,world!!"))
    (. stage (addChild t))))

