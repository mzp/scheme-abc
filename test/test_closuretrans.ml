open Base
open Ast2
open Util
open ClosureTrans2
open OUnit

let compile_string str =
  ClosTrans2.trans @@ Lisp2.compile_string str

let ok x y =
  OUnit.assert_equal
    ~printer:(fun x-> (Std.dump (List.map Ast2.to_string_stmt x) ^ "\n"))
    x y

let ident x=
  {Node.value = x; filename="<string>"; lineno=0}

let _ = 
  ("closure trans" >::: [
     "arguments" >::
       (fun () ->
	  ok [Define (ident "f",
		      Lambda ([ident "x"],
			      (Let ([ident "x",Var (ident "x")],
				    Block [Lambda ([],
						   Block [Var (ident "x")])]))))] @@
	    trans @@ compile_string "(define (f x) (lambda () x))");
     "class" >::
       (fun () ->
	  ok [
	    Class (ident "Foo",ident ("","Object"),[],
		   [ident "init",[ident "self"],
		    Let ([ident "self",Var (ident "self")],
			 Block [Lambda ([],Block [Var (ident "self")])])])] @@
	    trans @@ compile_string "(define-class Foo (Object) ())(define-method init ((self Foo)) (lambda () self))")
   ]) +> run_test_tt
