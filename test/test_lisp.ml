open Base
open OUnit
open Lisp
open Util
open Ast
open ClosTrans

let result xs =
  [ClosTrans.Plain (Expr xs)]

let ok x y =
  OUnit.assert_equal ~printer:(string_of_list $ List.map ClosTrans.to_string) x y

let _ =
  ("lisp module test" >::: [
     "empty" >::
       (fun () ->
	  OUnit.assert_equal [] @@ Lisp.compile_string "");
     "comment" >::
       (fun () ->
	  OUnit.assert_equal [] @@ 
	    Lisp.compile_string "; foo bar");
     "string" >::
       (fun () ->
	  ok (result (String "hello")) @@ 
	    Lisp.compile_string "\"hello\"");
     "int" >::
       (fun () ->
	  ok (result (Int 42)) @@ 
	    Lisp.compile_string "42");
     "float" >::
       (fun () ->
	  ok (result (Float 42.)) @@ 
	    Lisp.compile_string "42.";
	  ok (result (Float 42.5)) @@ 
	    Lisp.compile_string "42.5");
     "bool" >::
       (fun () ->
	  ok (result (Bool true)) @@ 
	    Lisp.compile_string "#t";
	  ok (result (Bool false)) @@ 
	    Lisp.compile_string "#f");
     "call" >::
       (fun () ->
	  ok (result (Call [Var "print"])) @@ 
	    Lisp.compile_string "(print)";
	  ok (result (Call [Var "print";String "hello"])) @@ 
	    Lisp.compile_string "(print \"hello\")";
	  ok (result (Call [Var "print";String "hello";String "world"])) @@ 
	    Lisp.compile_string "(print \"hello\" \"world\")");
     "+" >::
       (fun () ->
	  ok (result (Call [Var "+";Int 1;Int 2])) @@ 
	    Lisp.compile_string "(+ 1 2)";
	  ok (result (Call [Var "-";Int 1;Int 2])) @@ 
	    Lisp.compile_string "(- 1 2)";
	  ok (result (Call [Var "*";Int 1;Int 2])) @@ 
	    Lisp.compile_string "(* 1 2)";
	  ok (result (Call [Var "/";Int 1;Int 2])) @@ 
	    Lisp.compile_string "(/ 1 2)");
     "<" >::
       (fun () ->
	  ok (result (Call [Var "=";Int 1;Int 2])) @@ 
	    Lisp.compile_string "(= 1 2)";
	  ok (result (Call [Var "<";Int 1;Int 2])) @@ 
	    Lisp.compile_string "(< 1 2)";
	  ok (result (Call [Var "<=";Int 1;Int 2])) @@ 
	    Lisp.compile_string "(<= 1 2)";
	  ok (result (Call [Var ">";Int 1;Int 2])) @@ 
	    Lisp.compile_string "(> 1 2)";
	  ok (result (Call [Var ">=";Int 1;Int 2])) @@ 
	    Lisp.compile_string "(>= 1 2)");
     "if" >::
       (fun () ->
	  ok (result (If (Int 1,Int 2,Int 3))) @@ 
	    Lisp.compile_string "(if 1 2 3)");
     "let" >::
       (fun () ->
	  ok (result (Let (["x",Int 1;"y",Int 2],Block [Var "x";Var "y"]))) @@ 
	    Lisp.compile_string "(let ((x 1) (y 2)) x y)");
     "letrec" >::
       (fun () ->
	  ok (result (LetRec (["x",Int 1;"y",Int 2],Block [Var "x";Var "y"]))) @@ 
	    Lisp.compile_string "(letrec ((x 1) (y 2)) x y)");
     "begin" >::
       (fun () ->
	  ok (result (Block [Int 1;Int 2])) @@
	    Lisp.compile_string "(begin 1 2)");
     "lambda" >::
       (fun () ->
	  ok (result (Lambda ([],Block [Int 42]))) @@
	    Lisp.compile_string "(lambda () 42)");
     "lambda args" >::
       (fun () ->
	  ok (result (Lambda (["a";"b";"c"],Block [Int 42]))) @@
	    Lisp.compile_string "(lambda (a b c) 42)");
     "new" >::
       (fun () ->
	  ok (result (New (("","Foo"),[]))) @@
	    Lisp.compile_string "(new Foo)");
     "new args" >::
       (fun () ->
	  ok (result (New (("","Foo"),[Int 1;Int 2]))) @@
	    Lisp.compile_string "(new Foo 1 2)");
     "invoke" >::
       (fun () ->
	  ok (result (Invoke (Var "foo","baz",[Int 1;Int 2]))) @@
	    Lisp.compile_string "(. foo (baz 1 2))");
     "define" >::
       (fun () ->
	  ok [Plain (Define ("x",Block [Int 42]))] @@
	    Lisp.compile_string "(define x 42)";
	  ok [Plain (Define ("f",Lambda (["x"],Block [Int 42])))] @@
	    Lisp.compile_string "(define (f x) 42)");
     "class" >::
       (fun () ->
	  ok [DefineClass ("Foo",("","Object"),["x";"y"])] @@
	    Lisp.compile_string "(define-class Foo (Object) (x y))";
	  ok [DefineClass ("Foo",("flash.text","Object"),["x";"y"])] @@
	    Lisp.compile_string "(define-class Foo (flash.text.Object) (x y))";
	  ok [DefineClass ("Foo",("flash.text","Object"),[])] @@
	    Lisp.compile_string "(define-class Foo (flash.text.Object) ())");
     "method" >::
       (fun () ->
	  ok [DefineMethod ("f",("self","Object"),["x";"y"],Block [Int 42])] @@
	    Lisp.compile_string "(define-method f ((self Object) x y) 42)");
     "slot-ref" >::
       (fun () ->
	  ok (result (SlotRef (Var "obj","name"))) @@
	    Lisp.compile_string "(slot-ref obj name)");
     "slot-set!" >::
       (fun () ->
	  ok (result (SlotSet (Var "obj","name",Int 42))) @@
	    Lisp.compile_string "(slot-set! obj name 42)")
   ]) +> run_test_tt
  


