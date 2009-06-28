;;; 42
(class Foo (Object) ()
       (static f () 42))
(trace (. Foo (f)))
