open Base
open OUnit
open ModuleTrans
open AstUtil

let ok x y =
  OUnit.assert_equal
    x @@ trans y

let define x y =
  `Define (x,y)

let _ =
  ("moduleTrans.ml" >::: [
     "define trans" >::
       (fun () ->
	  ok
	    [define (`Public (qname "Foo" "bar")) (`Block [])]
	    [foo_mod [
	       define (sname "bar") (`Block [])]]);
     "class trans" >::
       (fun () ->
	  ok
	    [klass (`Public (qname "Foo" "Bar")) (global "Object") [] []]
	    [foo_mod [
	       klass (sname "Bar") (global "Object") [] []
	     ]]);
     "baz should be internal" >::
       (fun () ->
	  ok
	    [define (`Public (qname "Foo" "bar")) (`Block []);
	     define (`Internal (qname "Foo" "baz")) (`Block [])]
	    [module_ "Foo" (`Only [sname "bar"]) [
	       define (sname "bar") (`Block []);
	       define (sname "baz") (`Block [])]]);
     "Baz should be internal" >::
       (fun () ->
	  ok
	    [klass (`Public (qname "Foo" "Bar"))   (global "Object") [] [];
	     klass (`Internal (qname "Foo" "Baz")) (global "Object") [] []] @@
	    [module_ "Foo" (`Only [sname "Bar"]) [
	       klass (sname "Bar") (global "Object") [] [];
	       klass (sname "Baz") (global "Object") [] []]])
   ]) +> run_test_tt
