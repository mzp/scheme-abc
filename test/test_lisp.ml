open Base
open OUnit
open Lisp
open Util
open Ast
open ClosTrans

let expr xs =
  [`Expr xs]

let node x =
  {(Node.empty x) with Node.filename = "<string>"; Node.lineno = 0}

let ok x y =
  OUnit.assert_equal 
    ~cmp:(fun a b -> List.for_all2 AstUtil.eq_clos a b)
    ~printer:(string_of_list $ List.map ClosTrans.to_string)
    x @@ Lisp.compile_string y

let check x y =
  OUnit.assert_equal 
    ~printer:(string_of_list $ List.map ClosTrans.to_string)
    x @@ Lisp.compile_string y

let syntax_error f =
  try
    f ();
    assert_failure "not raise"
  with Parsec.Syntax_error _ ->
    assert_bool "raised" true

let string x =
  `String (node x)

let int x =
  `Int (node x)

let float x =
  `Float (node x)

let bool x =
  `Bool (node x)

let var x =
  `Var (node x)

let pos x n a b =
  {(Node.empty x) with 
     Node.filename = "<string>";
     lineno        = n;
     start_pos     = a;
     end_pos       = b}

let define_class name super attrs =
  `DefineClass (node name,node super,List.map node attrs)

let define_method name self obj args body =
  `DefineMethod (node name,(node self,node obj),List.map node args,body)

let _ =
  ("lisp module test" >::: [
     "pos" >::
       (fun () ->
	  check (expr (`Int (pos 42 0 0 2))) 
	    "42";
	  check (expr (`String (pos "hoge" 0 0 6))) 
	    "\"hoge\"";
	  check (expr (`Bool   (pos true 0 0 2))) 
	    "#t";
	  check (expr (`Var    (pos "foo" 0 0 3))) 
	    "foo";
	  check (expr (`Lambda ([pos "abc" 0 9 12],`Block []))) 
	    "(lambda (abc))";
	  check (expr (`Let ([pos "foo" 0 7 10,`Int (pos 42 0 11 13)],
			     `Block []))) @@ 
	    "(let [(foo 42)] )";
	  check (expr (`LetRec ([pos "foo" 0 10 13,`Int (pos 42 0 14 16)],
				`Block []))) @@ 
	    "(letrec [(foo 42)] )";
	  check (expr (`New (pos ("","Foo") 0 5 8 ,[]))) @@
	    "(new Foo)";
	  check (expr (`Invoke (`Var (pos "foo" 0 3 6), pos "baz" 0 8 11,[]))) @@
	    "(. foo (baz))";
	  check (expr (`SlotRef (`Var (pos "obj" 0 10 13),pos "name" 0 14 18))) @@
	    "(slot-ref obj name)";
	  check (expr (`SlotSet (`Var (pos "obj" 0 11 14),
				pos "name" 0 15 19,
				 `Int (pos  42 0 20 22)))) @@
	    "(slot-set! obj name 42)";
	  check [`Define (pos "x" 0 8 9,`Block [`Int (pos 42 0 10 12)])] @@
	    "(define x 42)";
	  check [`Define (pos "f" 0 9 10,`Lambda ([pos "x" 0 11 12],`Block []))] @@
	    "(define (f x))";
	  check [`DefineClass (pos "Foo" 0 14 17,
			      pos ("","Object") 0 19 25,
			      [pos "arg" 0 28 31])] @@
	    "(define-class Foo (Object) (arg))";
	  check [`DefineMethod (pos "fun" 0 15 18,
			       (pos "self" 0 21 25,pos "Object" 0 26 32),
			       [pos "xyz" 0 34 37],
				`Block [])] @@
	    "(define-method fun ((self Object) xyz))");
     "empty" >::
       (fun () ->
	  ok [] "");
     "comment" >::
       (fun () ->
	  ok [] "; foo bar");
     "string" >::
       (fun () ->
	  ok (expr (string "hello")) 
	    "\"hello\"");
     "int" >::
       (fun () ->
	  ok (expr (int 42)) 
	    "42");
     "float" >::
       (fun () ->
	  ok (expr (float 42.)) 
	    "42.";
	  ok (expr (float 42.5))  
	    "42.5");
     "bool" >::
       (fun () ->
	  ok (expr (bool true))  
	    "#t";
	  ok (expr (bool false))  
	    "#f");
     "call" >::
       (fun () ->
	  ok (expr (`Call [var "print"]))  
	    "(print)";
	  ok (expr (`Call [var "print";string "hello"]))  
	    "(print \"hello\")";
	  ok (expr (`Call [var "print";string "hello";string "world"]))  
	    "(print \"hello\" \"world\")");
     "+" >::
       (fun () ->
	  ok (expr (`Call [var "+";int 1;int 2]))  
	    "(+ 1 2)";
	  ok (expr (`Call [var "-";int 1;int 2]))  
	    "(- 1 2)";
	  ok (expr (`Call [var "*";int 1;int 2]))  
	    "(* 1 2)";
	  ok (expr (`Call [var "/";int 1;int 2]))  
	    "(/ 1 2)");
     "<" >::
       (fun () ->
	  ok (expr (`Call [var "=";int 1;int 2]))  
	    "(= 1 2)";
	  ok (expr (`Call [var "<";int 1;int 2]))  
	    "(< 1 2)";
	  ok (expr (`Call [var "<=";int 1;int 2]))  
	    "(<= 1 2)";
	  ok (expr (`Call [var ">";int 1;int 2]))  
	    "(> 1 2)";
	  ok (expr (`Call [var ">=";int 1;int 2]))  
	    "(>= 1 2)");
     "if" >::
       (fun () ->
	  ok (expr (`If (int 1,int 2,int 3)))  
	    "(if 1 2 3)");
     "cond" >::
       (fun () ->
	  ok (expr (`If (int 1,
			 `Block [int 2],
			 `If (int 3,
			      `Block [int 4],
			      `Block [int 5])))) 
	    "(cond (1 2) (3 4) (else 5))");
     "cond without else" >::
       (fun () ->
	  ok (expr (`If (int 1,
			`Block [int 2],
			`If (int 3,
			     `Block [int 4],
			     `Block [])))) 
	    "(cond (1 2) (3 4))");
     "let" >::
       (fun () ->
	  ok (expr (`Let ([node "x",int 1;node "y",int 2],
			  `Block [var "x";var "y"])))  
	    "(let ((x 1) (y 2)) x y)");
     "letrec" >::
       (fun () ->
	  ok (expr (`LetRec ([node "x",int 1;node "y",int 2],
			     `Block [var "x";var "y"])))  
	    "(letrec ((x 1) (y 2)) x y)");
     "begin" >::
       (fun () ->
	  ok (expr (`Block [int 1;int 2])) 
	    "(begin 1 2)");
     "lambda" >::
       (fun () ->
	  ok (expr (`Lambda ([],`Block [int 42]))) 
	    "(lambda () 42)";
	  ok (expr (`Lambda ([node "a";node "b";node "c"],
			     `Block [int 42]))) 
	    "(lambda (a b c) 42)");
     "new" >::
       (fun () ->
	  ok (expr (`New (node ("","Foo"),[]))) 
	    "(new Foo)";
	  ok (expr (`New (node ("","Foo"),[int 1;int 2]))) 
	    "(new Foo 1 2)");
     "invoke" >::
       (fun () ->
	  ok (expr (`Invoke (var "foo",node "baz",[int 1;int 2]))) 
	    "(. foo (baz 1 2))");
     "define" >::
       (fun () ->
	  ok [`Define (node "x",`Block [int 42])] 
	    "(define x 42)";
	  ok [`Define (node "f",`Lambda ([node "x"],
					 `Block [int 42]))] 
	    "(define (f x) 42)");
     "bug()" >::
       (fun () ->
	  ok [`Expr (int 10);
	      `Define (node "x",`Block [int 42])] 
	    "10 (define x 42)");
     "class" >::
       (fun () ->
	  ok [define_class "Foo" ("","Object") ["x";"y"]] 
	     "(define-class Foo (Object) (x y))";
	  ok [define_class "Foo" ("flash.text","Object") ["x";"y"]] 
	     "(define-class Foo (flash.text.Object) (x y))";
	  ok [define_class "Foo" ("flash.text","Object") []] 
	     "(define-class Foo (flash.text.Object) ())");
     "method" >::
       (fun () ->
	  ok [define_method  "f" "self" "Object" ["x";"y"] (`Block [int 42])] 
	    "(define-method f ((self Object) x y) 42)");
     "slot-ref" >::
       (fun () ->
	  ok (expr (`SlotRef (var "obj",node "name"))) 
	    "(slot-ref obj name)");
     "slot-set!" >::
       (fun () ->
	  ok (expr (`SlotSet (var "obj",node "name",int 42))) 
	    "(slot-set! obj name 42)");
     "syntax error" >::
       (fun () ->
	  syntax_error (fun () ->
			  Lisp.compile_string "(if a)"))
   ]) +> run_test_tt
