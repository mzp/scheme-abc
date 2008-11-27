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
			   `Lambda ([],
				    `Var (ident "x")))) in
	    ok [`Define (ident "f",lambda)] @@
	      trans @@ [`Define (ident "f",
				`Lambda ([ident "x"],
					`Lambda ([],
						 `Var (ident "x"))))]);
     "class" >::
       (fun () ->
	  ok [
	    `Class (ident "Foo",ident ("","Object"),[],
		    [ident "init",[ident "self"],
		     `Let ([ident "self",`Var (ident "self")],
			   `Lambda ([],`Var (ident "self")))])] @@
	    trans @@ [`Class (ident "Foo",ident ("","Object"),[],
			      [ident "init",[ident "self"],
			       `Lambda ([],`Var (ident "self"))])])

   ]) +> run_test_tt
