(define (main stage)
  (let [(t (new flash.text.TextField))]
    (trace "foo!!!!!!!!!!")
    (. t (appendText "Hello,world!!"))
    (. stage (addChild t))))

