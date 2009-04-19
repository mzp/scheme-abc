open Base
open OUnit
open AstUtil
open ModuleResolve

let ok x y =
  OUnit.assert_equal
    x @@ trans y

let define name =
  `Define (sname name,(`Block []))

let open_ name =
  `Open (sname name)

let _ =
  ("moduleTrans.ml" >::: [
     "module open" >::
       (fun () ->
	  ok
	    [foo_mod [ define "x"];
	     bar_mod [ expr @@ `Var (qname "foo" "x")]]
	    [foo_mod [ define "x"];
	     bar_mod [
	       open_ "foo";
	       expr @@ `Var (global "x")]]);
     "own module open" >::
       (fun () ->
	  ok
	    [foo_mod [ define "x";
		       expr @@ `Var (qname "foo" "x")]]
	    [foo_mod [ define "x";
		       expr @@ `Var (qname "" "x")]])
   ]) +> run_test_tt
