open Base
open Ast
open Override
open AstUtil
open OUnit
open Binding

let ok x y =
  assert_equal ~printer:Std.dump x (snd @@ Binding.of_module y)

let member scope ns name =
  Node.ghost @@ Member (scope,(ns,name))
let slot scope i =
  Node.ghost @@ Slot (scope,i)
let register n =
  Node.ghost @@ Register n

let class_  meths =
  AstUtil.class_ (`Public (qname [] "Foo")) (qname [] "Object") [] meths

(*
  type bind =
  Register of int
  | Slot of scope * int
  | Member of scope * name
*)
let _ =
  ("binding.ml" >::: [
     "let" >::
       (fun () ->
	  ok [expr @@ let_ ["x", int 42] @@
		`BindVar (member (Scope 1) [] "x")]
	    [expr @@ let_ ["x", int 42] @@
	       var [] "x"];
	  ok [expr @@ let_ ["x", var [] "x"] @@
		block []]
	    [expr @@ let_ ["x", var [] "x"] @@
	       block []];
	  ok [expr @@ let_ ["x", int 42] @@
		let_  ["x",int 42] @@
		`BindVar (member (Scope 2) [] "x")]
	    [expr @@ let_ ["x", int 42] @@
	       let_  ["x",int 42] @@
	       var [] "x"]);
     "letrec" >::
       (fun () ->
	  ok [expr @@ let_rec ["x", int 42] @@
		`BindVar (member (Scope 1) [] "x")]
	    [expr @@ let_rec ["x", int 42] @@
	       var [] "x"];
	  (* note! *)
	  ok [expr @@ let_rec ["x", `BindVar (member (Scope 1) [] "x")] @@
		block []]
	    [expr @@ let_rec ["x", var [] "x"] @@
	       block []];
	  ok [expr @@ let_rec ["x", int 42] @@
		let_rec  ["x",int 42] @@
		`BindVar (member (Scope 2) [] "x")]
	    [expr @@ let_rec ["x", int 42] @@
	       let_rec  ["x",int 42] @@
	       var [] "x"]);
     "lambda" >::
       (fun () ->
	  ok [expr @@ lambda [] @@ var [] "x"]
	    [expr @@ lambda [] @@ var [] "x"];
	  ok [expr @@ lambda ["x"] @@ `BindVar (register 1)]
	    [expr @@ lambda ["x"] @@ var [] "x"];
	  ok [expr @@ lambda ["x";"y"] @@ `BindVar (register 2)]
	    [expr @@ lambda ["x";"y"] @@ var [] "y"]);
     "method" >::
       (fun () ->
	  ok [class_ [public_meth "f" [] @@ var [] "x"]]
	     [class_ [public_meth "f" [] @@ var [] "x"]];
	  ok [class_ [public_meth "f" ["x"] @@ `BindVar (register 0)]]
	     [class_ [public_meth "f" ["x"] @@ var [] "x"]]);
     "define" >::
       (fun () ->
	  ok [`Define (`Public (qname [] "x"),int 42);
	      expr @@ `BindVar (slot Global 1)]
	    [`Define (`Public (qname [] "x"),int 42);
	     expr @@ var [] "x"]);
     "multiple-define" >::
       (fun () ->
	  ok [`Define (`Public (qname [] "x"),int 42);
	      expr @@ `BindVar (slot Global 1);
	      `Define (`Public (qname [] "x"),int 42);
	      expr @@ `BindVar (slot Global 2)]
	    [`Define (`Public (qname [] "x"),int 42);
	     expr @@ var [] "x";
	     `Define (`Public (qname [] "x"),int 42);
	     expr @@ var [] "x"]);
     "class" >::
       (fun () ->
	  ok [class_ [];
	     expr @@ `BindVar (member Global [] "Foo")]
	    [class_ [];
	     expr @@ var [] "Foo"]);
   ]) +> run_test_tt

