open Base
open OUnit
open ModuleTrans
open AstUtil

let ok x y =
  OUnit.assert_equal
    x @@ trans y

let sname =
  node

let define x y =
  `Define (x,y)

let foo_mod x =
  `Module (sname "foo",[],x)

let _ =
  ("moduleTrans.ml" >::: [
     "define trans" >::
       (fun () ->
	  ok
	    [define (node ("foo","bar")) (`Block [])]
	    [foo_mod [
	       define (sname "bar") (`Block [])]]);
     "class trans" >::
       (fun () ->
	  ok
	    [`Class (node ("foo","bar"),global "Object",[],[])]
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
   ]) +> run_test_tt


