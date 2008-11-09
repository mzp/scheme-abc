open Base
open ClosTrans
open Ast
open OUnit

let ok x y =
  OUnit.assert_equal ~printer:(string_of_list $ List.map Ast.to_string_stmt) x y

let _ =
  ("clos module test" >::: [
     "basic" >:: 
       (fun () ->
	  ok [Class ("Foo",("bar","Baz"),[],
		     [("f",["self";"x"],Int 42)])] @@ 
	    trans [DefineClass ("Foo",("bar","Baz"),[]);
		   DefineMethod ("f",("self","Foo"),["x"],Int 42)]);
     "attributes" >::
       (fun () ->
	  ok [Class ("Foo",("bar","Baz"),["x";"y"],[])] @@
	    trans [DefineClass ("Foo",("bar","Baz"),["x";"y"])]);
     "plain is not change" >::
       (fun () ->
	  ok [Expr (Int 42)] @@ 
	    trans [Plain (Expr (Int 42))]);
     "define and plain is mixed" >::
       (fun () ->
	  ok [Class ("Foo",("bar","Baz"),[],
		     [("f",["self";"x"],Int 42)]);
	      Expr (Int 42)] @@
	    trans [DefineClass ("Foo",("bar","Baz"),[]);
		   Plain (Expr (Int 42));
		   DefineMethod ("f",("self","Foo"),["x"],Int 42)]);
     "invoke" >::
       (fun () ->
	  ok [Class ("Foo",("bar","Baz"),[],
		     [("f",["self";"x"],Int 42)]);
	      Expr (Invoke (Var "obj","f",[Int 10]))] @@
	    trans [DefineClass ("Foo",("bar","Baz"),[]);
		   DefineMethod ("f",("self","Foo"),["x"],Int 42);
		   Plain (Expr (Call [Var "f";Var "obj";Int 10]))]);
     "invoke deep" >::
       (fun () ->
	  ok [Expr (If (Invoke (Var "obj","f",[Int 10]),
			Block [],
			Block []))] @@
	    trans [DefineMethod ("f",("self","Foo"),["x"],Int 42);
		   Plain (Expr (If (Call [Var "f";Var "obj";Int 10],
				    Block [],
				    Block [])))])
   ]) +> run_test_tt

