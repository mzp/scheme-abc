open Base
open Ast
open Util
open ClosureTrans
open OUnit

let compile_string str =
  ClosTrans.trans @@ Lisp.compile_string str

let ok x y =
  OUnit.assert_equal
    ~cmp:(fun a b -> List.for_all2 AstUtil.eq_stmt a b)
    ~printer:(fun x-> (Std.dump (List.map Ast.to_string_stmt x) ^ "\n"))
    x y

let ident value =
  {(Node.empty value) with Node.filename= "<string>"}

let _ = 
  ("closure trans" >::: [
     "arguments" >::
       (fun () ->
	  let lambda =
	    `Lambda ([ident "x"],
		     `Let ([ident "x",`Var (ident "x")],
			   `Block [`Lambda ([],
					    `Block [`Var (ident "x")])])) in
	    ok [`Define (ident "f",lambda)] @@
	      trans @@ compile_string "(define (f x) (lambda () x))");
     "class" >::
       (fun () ->
	  ok [
	    `Class (ident "Foo",ident ("","Object"),[],
		    [ident "init",[ident "self"],
		     `Let ([ident "self",`Var (ident "self")],
			   `Block [`Lambda ([],`Block [`Var (ident "self")])])])] @@
	    trans @@ compile_string "(define-class Foo (Object) ())(define-method init ((self Foo)) (lambda () self))")
   ]) +> run_test_tt
