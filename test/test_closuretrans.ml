open Base
open Ast
open Util
open ClosureTrans
open OUnit

let compile_string str =
  ClosTrans.trans @@ Lisp.compile_string str

let ok x y =
  OUnit.assert_equal
    ~printer:(fun x-> (Std.dump (List.map Ast.to_string_stmt x) ^ "\n"))
    x y

let _ = 
  ("closure trans" >::: [
     "arguments" >::
       (fun () ->
	  ok [Define ("f",
		      Lambda (["x"],
			      (Let (["x",Var "x"],
				    Block [Lambda ([],Block [Var "x"])]))))] @@
	    trans @@ compile_string "(define (f x) (lambda () x))");
     "class" >::
       (fun () ->
	  ok [
	    Class ("Foo",("","Object"),[],
		   ["init",["self"],
		    Let (["self",Var "self"],
			 Block [Lambda ([],Block [Var "self"])])])] @@
	    trans @@ compile_string "(define-class Foo (Object) ())
(define-method init ((self Foo)) (lambda () self))")
   ]) +> run_test_tt
