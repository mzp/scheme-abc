open Base
open OUnit
open Lisp2
open Util
open Ast2
open ClosTrans2

let expr xs =
  [ClosTrans2.Plain (Ast2.Expr xs)]

let node x =
  {(Node.empty x) with Node.filename = "<string>"; Node.lineno = 0}

let ok x y =
  OUnit.assert_equal ~printer:(string_of_list $ List.map ClosTrans2.to_string) x y

let syntax_error f =
  try
    f ();
    assert_failure "not raise"
  with Syntax_error _ ->
    assert_bool "raised" true

let _ =
  ("lisp module test" >::: [
     "empty" >::
       (fun () ->
	  OUnit.assert_equal [] @@ Lisp2.compile_string "");
     "comment" >::
       (fun () ->
	  OUnit.assert_equal [] @@ 
	    Lisp2.compile_string "; foo bar");
     "string" >::
       (fun () ->
	  ok (expr (String (node "hello"))) @@ 
	    Lisp2.compile_string "\"hello\"");
     "int" >::
       (fun () ->
	  ok (expr (Int (node 42))) @@ 
	    Lisp2.compile_string "42");
     "float" >::
       (fun () ->
	  ok (expr (Float (node 42.))) @@ 
	    Lisp2.compile_string "42.";
	  ok (expr (Float (node 42.5))) @@ 
	    Lisp2.compile_string "42.5");
(*     "bool" >::
       (fun () ->
	  ok (expr (Bool true)) @@ 
	    Lisp2.compile_string "#t";
	  ok (expr (Bool false)) @@ 
	    Lisp2.compile_string "#f");
     "call" >::
       (fun () ->
	  ok (expr (Call [Var "print"])) @@ 
	    Lisp2.compile_string "(print)";
	  ok (expr (Call [Var "print";String "hello"])) @@ 
	    Lisp2.compile_string "(print \"hello\")";
	  ok (expr (Call [Var "print";String "hello";String "world"])) @@ 
	    Lisp2.compile_string "(print \"hello\" \"world\")");
     "+" >::
       (fun () ->
	  ok (expr (Call [Var "+";Int 1;Int 2])) @@ 
	    Lisp2.compile_string "(+ 1 2)";
	  ok (expr (Call [Var "-";Int 1;Int 2])) @@ 
	    Lisp2.compile_string "(- 1 2)";
	  ok (expr (Call [Var "*";Int 1;Int 2])) @@ 
	    Lisp2.compile_string "(* 1 2)";
	  ok (expr (Call [Var "/";Int 1;Int 2])) @@ 
	    Lisp2.compile_string "(/ 1 2)");
     "<" >::
       (fun () ->
	  ok (expr (Call [Var "=";Int 1;Int 2])) @@ 
	    Lisp2.compile_string "(= 1 2)";
	  ok (expr (Call [Var "<";Int 1;Int 2])) @@ 
	    Lisp2.compile_string "(< 1 2)";
	  ok (expr (Call [Var "<=";Int 1;Int 2])) @@ 
	    Lisp2.compile_string "(<= 1 2)";
	  ok (expr (Call [Var ">";Int 1;Int 2])) @@ 
	    Lisp2.compile_string "(> 1 2)";
	  ok (expr (Call [Var ">=";Int 1;Int 2])) @@ 
	    Lisp2.compile_string "(>= 1 2)");
     "if" >::
       (fun () ->
	  ok (expr (If (Int 1,Int 2,Int 3))) @@ 
	    Lisp2.compile_string "(if 1 2 3)");
     "cond" >::
       (fun () ->
	  ok (expr (If (Int 1,
			  Block [Int 2],
			  If (Int 3,
			      Block [Int 4],
			      Block [Int 5])))) @@
	    Lisp2.compile_string "(cond (1 2) (3 4) (else 5))");
     "cond without else" >::
       (fun () ->
	  ok (expr (If (Int 1,
			  Block [Int 2],
			  If (Int 3,
			      Block [Int 4],
			      Block [])))) @@
	    Lisp2.compile_string "(cond (1 2) (3 4))");
     "let" >::
       (fun () ->
	  ok (expr (Let (["x",Int 1;"y",Int 2],Block [Var "x";Var "y"]))) @@ 
	    Lisp2.compile_string "(let ((x 1) (y 2)) x y)");
     "letrec" >::
       (fun () ->
	  ok (expr (LetRec (["x",Int 1;"y",Int 2],Block [Var "x";Var "y"]))) @@ 
	    Lisp2.compile_string "(letrec ((x 1) (y 2)) x y)");
     "begin" >::
       (fun () ->
	  ok (expr (Block [Int 1;Int 2])) @@
	    Lisp2.compile_string "(begin 1 2)");
     "lambda" >::
       (fun () ->
	  ok (expr (Lambda ([],Block [Int 42]))) @@
	    Lisp2.compile_string "(lambda () 42)");
     "lambda args" >::
       (fun () ->
	  ok (expr (Lambda (["a";"b";"c"],Block [Int 42]))) @@
	    Lisp2.compile_string "(lambda (a b c) 42)");
     "new" >::
       (fun () ->
	  ok (expr (New (("","Foo"),[]))) @@
	    Lisp2.compile_string "(new Foo)");
     "new args" >::
       (fun () ->
	  ok (expr (New (("","Foo"),[Int 1;Int 2]))) @@
	    Lisp2.compile_string "(new Foo 1 2)");
     "invoke" >::
       (fun () ->
	  ok (expr (Invoke (Var "foo","baz",[Int 1;Int 2]))) @@
	    Lisp2.compile_string "(. foo (baz 1 2))");
     "define" >::
       (fun () ->
	  ok [Plain (Define ("x",Block [Int 42]))] @@
	    Lisp2.compile_string "(define x 42)";
	  ok [Plain (Define ("f",Lambda (["x"],Block [Int 42])))] @@
	    Lisp2.compile_string "(define (f x) 42)");
     "class" >::
       (fun () ->
	  ok [DefineClass ("Foo",("","Object"),["x";"y"])] @@
	    Lisp2.compile_string "(define-class Foo (Object) (x y))";
	  ok [DefineClass ("Foo",("flash.text","Object"),["x";"y"])] @@
	    Lisp2.compile_string "(define-class Foo (flash.text.Object) (x y))";
	  ok [DefineClass ("Foo",("flash.text","Object"),[])] @@
	    Lisp2.compile_string "(define-class Foo (flash.text.Object) ())");
     "method" >::
       (fun () ->
	  ok [DefineMethod ("f",("self","Object"),["x";"y"],Block [Int 42])] @@
	    Lisp2.compile_string "(define-method f ((self Object) x y) 42)");
     "slot-ref" >::
       (fun () ->
	  ok (expr (SlotRef (Var "obj","name"))) @@
	    Lisp2.compile_string "(slot-ref obj name)");
     "slot-set!" >::
       (fun () ->
	  ok (expr (SlotSet (Var "obj","name",Int 42))) @@
	    Lisp2.compile_string "(slot-set! obj name 42)");
     "syntax error" >::
       (fun () ->
	  syntax_error (fun () ->
			  Lisp2.compile_string "(if a)"))*)
   ]) +> run_test_tt
