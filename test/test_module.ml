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
	    [define (`Public (qname "foo" "bar")) (`Block [])]
	    [foo_mod [
	       define (sname "bar") (`Block [])]]);
     "class trans" >::
       (fun () ->
	  ok
	    [`Class (`Public (qname "foo" "bar"),global "Object",[],[])]
	    [foo_mod [
	       `Class (sname "bar",global "Object",[],[])]]);
     "external" >::
       (fun () ->
	  ok
	    [`External (qname "foo" "bar")]
	    [foo_mod [
	       `External (sname "bar")
	     ]]);
     "external-class" >::
       (fun () ->
	  ok
	    [`ExternalClass (qname "foo" "bar",[sname "f";sname "g"])]
	    [foo_mod [
	       `ExternalClass (sname "bar",[sname "f";sname "g"])
	     ]]);
     "nested" >::
       (fun () ->
	  ok
	    [`ExternalClass (qname "foo.bar" "Baz",[sname "f";sname "g"])]
	    [foo_mod [
	       bar_mod [
		 `ExternalClass (sname "Baz",[sname "f";sname "g"])
	       ]]]);
   ]) +> run_test_tt


