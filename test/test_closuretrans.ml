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

let name value =
  {(Node.empty ("",value)) with Node.filename= "<string>"}

let _ =
  ("closure trans" >::: [
     "arguments" >::
       (fun () ->
	  let lambda =
	    `Lambda ([ident "x"],
		     `Let ([ident "x",`Var (name "x")],
			   `Lambda ([],
				    `Var (name "x")))) in
	    ok [`Define (name "f",lambda)] @@
	      trans @@ [`Define (name "f",
				 `Lambda ([ident "x"],
					  `Lambda ([],
						   `Var (name "x"))))]);
     "class" >::
       (fun () ->
	  ok [
	    `Class (name "Foo",ident ("","Object"),[],
		    [ident "init",[ident "self"],
		     `Let ([ident "self",`Var (name "self")],
			   `Lambda ([],`Var (name "self")))])] @@
	    trans @@ [`Class (name "Foo",ident ("","Object"),[],
			      [ident "init",[ident "self"],
			       `Lambda ([],`Var (name "self"))])])

   ]) +> run_test_tt
