;;; 4
(class Foo (Object) ()
       (method f (self x)))
(class Bar (Foo)    ()
       (method f (self x)
	       (+ x 1)))

(trace (. (new Bar) (f 3)))
