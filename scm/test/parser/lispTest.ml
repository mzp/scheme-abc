open Base
open OUnit
open Lisp
open Ast
open AstUtil
open Node

let expr xs =
  [`Expr xs]

let parse_string str =
  let stream =
    Node.of_string str in
  let stream' =
    Stream.from (fun _ -> try
			    Some (Node.ghost @@ Node.value @@ Stream.next stream)
			  with Stream.Failure ->
			    None) in
    Lisp.parse @@ Sexp.of_stream @@ Lexer.lexer Lexer.scheme stream'

let ok ?msg x y =
  OUnit.assert_equal ?msg
    x @@ parse_string y

let pos_ok x y =
  OUnit.assert_equal
    x @@ Lisp.parse @@ Sexp.of_stream @@ Lexer.lexer Lexer.scheme @@ Node.of_string y

let sugar x y =
  OUnit.assert_equal (parse_string x) (parse_string y)

let syntax_error f =
  try
    f ();
    assert_failure "not raise"
  with Parsec.Syntax_error _ ->
    assert_bool "raised" true

let pos x n a b =
  {(Node.ghost x) with
     Node.filename = "<string>";
     lineno        = n;
     start_pos     = a;
     end_pos       = b}

let _ =
  ("lisp.ml" >::: [
     "pos" >::: [
       "int" >::
	 (fun () ->
	    pos_ok (expr (`Int (pos 42 0 0 2)))
	      "42");
       "string" >::
	 (fun () ->
	    pos_ok (expr (`String (pos "hoge" 0 0 6)))
	      "\"hoge\"");
       "bool" >::
	 (fun () ->
	    pos_ok (expr (`Bool   (pos true 0 0 2)))
	      "#t");
       "var" >::
	 (fun () ->
 	    pos_ok (expr (`Var    (pos ([],"foo") 0 0 3)))
	      "foo");
       "lambda" >::
	 (fun () ->
	    pos_ok (expr (`Lambda ([pos "abc" 0 9 12],`Block [])))
	      "(lambda (abc))");
       "let" >::
	 (fun () ->
	    pos_ok (expr (`Let ([pos "foo" 0 7 10,`Int (pos 42 0 11 13)], `Block []))) @@
	      "(let [(foo 42)] )");
       "letrec" >::
	 (fun () ->
	    pos_ok (expr (`LetRec ([pos "foo" 0 10 13,`Int (pos 42 0 14 16)], `Block []))) @@
	      "(letrec [(foo 42)] )");
       "new" >::
	 (fun () ->
	    pos_ok (expr (`New (pos ([],"Foo") 0 5 8 ,[]))) @@
	      "(new Foo)");
       "invpos_oke" >::
	 (fun () ->
	    pos_ok (expr (`Invoke (`Var (pos ([],"foo") 0 3 6), pos "baz" 0 8 11,[]))) @@
	      "(. foo (baz))");
       "slot-ref" >::
	 (fun () ->
	    pos_ok (expr (`SlotRef (`Var (pos ([],"obj") 0 10 13),pos "name" 0 14 18))) @@
	      "(slot-ref obj name)");
       "slot-set!" >::
	 (fun () ->
	    pos_ok (expr (`SlotSet (`Var (pos ([],"obj") 0 11 14),
				pos "name" 0 15 19,
				`Int (pos  42 0 20 22)))) @@
	      "(slot-set! obj name 42)");
       "list" >::
	 (fun () -> sugar "(cons 1 (cons 2 (cons 3 nil)))" "(list 1 2 3)");
       "define value" >::
	 (fun () ->
	    pos_ok [`Define (pos ("x") 0 8 9,`Block [`Int (pos 42 0 10 12)])] @@
	      "(define x 42)");
       "define lambda" >::
	 (fun () ->
	    pos_ok [`Define (pos ("f") 0 9 10,`Lambda ([pos "x" 0 11 12],`Block []))] @@
	    "(define (f x))");
       "class" >::
	 (fun () ->
	    pos_ok [`Class {Ast.class_name = pos ("Foo") 0 7 10;
			    super = pos ([],"Object") 0 12 18;
			    attrs = [pos "arg" 0 21 24];
			    methods = [{
			      Ast.method_name = `Public (pos "f" 0 34 35);
			      args = [pos "self" 0 37 41];
			      body = `Block [] }]
			   }] @@
	      "(class Foo (Object) (arg) (method f (self)))");
       "module" >::
	 (fun () ->
	    pos_ok [`Module {
		  Ast.module_name =pos "foo" 0 8 11;
		  exports = `Only [
		    pos "x" 0 13 14;
		    pos "y" 0 15 16
		  ];
		  stmts = []}
	       ] @@
	      "(module foo (x y))")
     ];
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
	  ok ~msg:"42." (expr (float 42.))
	    "42.";
	  ok ~msg:"42.5" (expr (float 42.5))
	    "42.5");
     "bool" >::
       (fun () ->
	  ok (expr (bool true))
	    "#t";
	  ok (expr (bool false))
	    "#f");
     "array" >::
       (fun () ->
	  ok (expr (`Array []))
	    "(array)";
	  ok (expr (`Array [int 1; int 2; int 3]))
	    "(array 1 2 3)");
     "call" >::
       (fun () ->
	  ok (expr (`Call [var [] "print"]))
	    "(print)";
	  ok (expr (`Call [var [] "print"; string "hello"]))
	    "(print \"hello\")";
	  ok (expr (`Call [var [] "print";string "hello";string "world"]))
	    "(print \"hello\" \"world\")");
     "+" >::
       (fun () ->
	  ok (expr (`Call [var [] "+";int 1;int 2]))
	    "(+ 1 2)";
	  ok (expr (`Call [var [] "-";int 1;int 2]))
	    "(- 1 2)";
	  ok (expr (`Call [var [] "*";int 1;int 2]))
	    "(* 1 2)";
	  ok (expr (`Call [var [] "/";int 1;int 2]))
	    "(/ 1 2)");
     "variable argument" >::
       (fun () ->
	  ok (expr (`Call [var [] "+"; `Call [var [] "+";int 1;int 2];
			  int 3]))
	    "(+ 1 2 3)";
	  ok (expr (`Call [var [] "*"; `Call [var [] "*";int 1;int 2];
			  int 3]))
	    "(* 1 2 3)");
     "<" >::
       (fun () ->
	  ok (expr (`Call [var [] "=";int 1;int 2]))
	    "(= 1 2)";
	  ok (expr (`Call [var [] "<";int 1;int 2]))
	    "(< 1 2)";
	  ok (expr (`Call [var [] "<=";int 1;int 2]))
	    "(<= 1 2)";
	  ok (expr (`Call [var [] ">";int 1;int 2]))
	    "(> 1 2)";
	  ok (expr (`Call [var [] ">=";int 1;int 2]))
	    "(>= 1 2)");
     "bug(var [] .)" >::
       (fun () ->
	  ok (expr @@ var [] "+.") "+.");
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
	  ok (expr (let_ ["x",int 1; "y",int 2] @@
		      `Block [var [] "x";var [] "y"]))
	    "(let ((x 1) (y 2)) x y)");
     "letrec" >::
       (fun () ->
	  ok (expr (let_rec ["x",int 1; "y",int 2]  @@
		      `Block [var [] "x";var [] "y"]))
	    "(letrec ((x 1) (y 2)) x y)");
     "begin" >::
       (fun () ->
	  ok (expr (`Block [int 1;int 2]))
	    "(begin 1 2)");
     "lambda" >::
       (fun () ->
	  ok (expr (lambda [] @@ block [int 42]))
	    "(lambda () 42)";
	  ok (expr (lambda ["a"; "b"; "c"] @@
		      block [int 42]))
	    "(lambda (a b c) 42)");
     "new" >::
       (fun () ->
	  ok (expr (new_ [] "Foo" []))
	    "(new Foo)";
	  ok (expr (new_ [] "Foo" [int 1;int 2]))
	    "(new Foo 1 2)");
     "invoke" >::
       (fun () ->
	  ok (expr (invoke (var [] "foo")  "baz" [int 1;int 2]))
	    "(. foo (baz 1 2))");
     "define" >::
       (fun () ->
	  ok [define "x" @@ block [int 42]]
	    "(define x 42)";
	  ok [define "f" @@
		lambda ["x"] @@ block [int 42]]
	    "(define (f x) 42)");
     "open" >::
       (fun () ->
	  ok [`Open (Node.ghost ["foo"])]
	    "(open foo)");
     "open(nest)" >::
       (fun () ->
	  ok [`Open (Node.ghost ["foo";"bar"])]
	    "(open foo.bar)");
     "module" >::
       (fun () ->
	  ok [foo_mod [
		define "x" @@ block [int 42 ] ]]
	    "(module foo () (define x 42))");
     "exports-module" >::
       (fun () ->
	  ok [module_ "foo"
		(`Only [Node.ghost "x"; Node.ghost "y"]) [
		  define "x" @@ block [ int 42 ] ]]
	    "(module foo (x y) (define x 42))");
     "bug" >::
       (fun () ->
	  ok [`Expr (int 10);
	      define "x" @@ block [int 42]]
	    "10 (define x 42)");
     "class" >::
       (fun () ->
	  ok [class_ "Foo" ([],"Object") ["x";"y"] []]
	    "(class Foo (Object) (x y))";
	  ok [class_ "Foo" (["flash";"text"],"Object") ["x";"y"] []]
	    "(class Foo (flash.text.Object) (x y))";
	  ok [class_ "Foo" (["flash";"text"],"Object") [] []]
	    "(class Foo (flash.text.Object) ())");
     "method" >::
       (fun () ->
	  ok [class_ "Foo" ([],"Object") [] [
		public_meth  "f" ["self";"x";"y"] (`Block [int 42])]]
	    "(class Foo (Object) () (method f (self x y) 42))");
     "static method" >::
       (fun () ->
	  ok [class_ "Foo" ([],"Object") [] [
		static_meth  "f" ["self";"x";"y"] (`Block [int 42])]]
	    "(class Foo (Object) () (static f (self x y) 42))");
     "slot-ref" >::
       (fun () ->
	  ok (expr (`SlotRef (var [] "obj",Node.ghost "name")))
	    "(slot-ref obj name)");
     "slot-set!" >::
       (fun () ->
	  ok (expr (`SlotSet (var [] "obj",Node.ghost "name",int 42)))
	    "(slot-set! obj name 42)");
     "syntax error" >::
       (fun () ->
	  syntax_error (fun () ->
			  parse_string "(if a)");
	  syntax_error (fun () ->
			  parse_string "(if a b c d)"))
   ]) +> run_test_tt
