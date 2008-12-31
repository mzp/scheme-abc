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
	    [`Class (`Public (qname "foo" "Bar"),global "Object",[],[])]
	    [foo_mod [
	       `Class (sname "Bar",global "Object",[],[])]]);
     "baz should be internal" >::
       (fun () ->
	  ok
	    [define (`Public (qname "foo" "bar")) (`Block []);
	     define (`Internal (qname "foo" "baz")) (`Block [])]
	    [module_ "foo" (Restrict [sname "bar"]) [
	       define (sname "bar") (`Block []);
	       define (sname "baz") (`Block [])]]);
     "Baz should be internal" >::
       (fun () ->
	  ok
	    [`Class (`Public (qname "foo" "Bar"),global "Object",[],[]);
	     `Class (`Internal (qname "foo" "Baz"),global "Object",[],[])]
	    [module_ "foo" (Restrict [sname "Bar"]) [
	       `Class (sname "Bar",global "Object",[],[]);
	       `Class (sname "Baz",global "Object",[],[])]])
   ]) +> run_test_tt


