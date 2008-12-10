open Base
open OUnit
open Lisp
open Util
open Ast
open ClosTrans
open AstUtil

open Node

let expr xs =
  [`Expr xs]

let eq_ident {value = x} {value = y} =
  x = y

let rec eq_expr a b =
  match a,b with
      `Int    {value = x}, `Int {value = y} ->
	x = y
    | `String {value = x}, `String {value = y} ->
	x = y
    | `Bool   {value = x}, `Bool {value = y} ->
	x = y
    | `Float  {value = x}, `Float {value = y} ->
	x = y
    | `Var    {value = x}, `Var {value = y} ->
	x = y
    | `Lambda (args,expr), `Lambda (args',expr') ->
	(List.for_all2 eq_ident args args') && eq_expr expr expr'
    | `Call   args, `Call args' ->
	List.for_all2 eq_expr args args'
    | `If  (a,b,c), `If (a',b',c') ->
	List.for_all2 eq_expr [a;b;c] [a';b';c']
    | `Let (decls,body), `Let (decls',body')
    | `LetRec (decls,body), `LetRec (decls',body')  ->
	let b =
	  List.for_all2
	    (fun (v,e) (v',e') -> eq_ident v v' && eq_expr e e')
	    decls' decls' in
	  b && eq_expr body body'
    | `Block xs, `Block xs' ->
	List.for_all2 eq_expr xs xs'
    | `New ({value=name},args), `New ({value=name'},args') ->
	name = name' && HList.conj @@ List.map2 eq_expr args args'
    | `Invoke (obj,name,args), `Invoke (obj',name',args') ->
	eq_expr obj obj' && eq_ident name name' &&
	  List.for_all2 eq_expr args args'
    | `SlotRef (obj,name), `SlotRef (obj',name') ->
	eq_expr obj obj' && eq_ident name name'
    | `SlotSet (obj,name,value), `SlotSet (obj',name',value') ->
	eq_expr obj obj' && eq_ident name name' && eq_expr value' value'
    | _ ->
	false

let eq_method (name,args,body) (name',args',body') =
  eq_ident name name' &&
    (List.for_all2 eq_ident args args') &&
    eq_expr body body'

let eq_stmt a b =
  match a,b with
      `Define (name,body), `Define (name',body') ->
	eq_ident name name' && eq_expr body body'
    | `Expr expr, `Expr expr' ->
	eq_expr expr expr'
    | `Class (name,{value=super},attrs,methods),
	`Class (name',{value=super'},attrs',methods') ->
	eq_ident name name' &&
	  super = super' &&
	  (List.for_all2 eq_ident attrs attrs') &&
	  (List.for_all2 eq_method methods methods')
    | _ ->
	false

let eq_clos a b =
  match a,b with
      `DefineClass (name,{value=super},attrs), `DefineClass (name',{value=super'},attrs') ->
	eq_ident name name' &&
	  super = super' && List.for_all2 eq_ident attrs attrs'
    | `DefineMethod (name,(self,obj),args,body), `DefineMethod (name',(self',obj'),args',body') ->
	eq_ident name name' &&
	  eq_ident self self' &&
	  eq_ident obj obj' &&
	  (List.for_all2 eq_ident args args') &&
	  eq_expr body body'
    | `External name , `External name' ->
	eq_ident name name'
    | `ExternalClass ({value=name},methods), `ExternalClass ({value=name'},methods') ->
	name = name' && List.for_all2 eq_ident methods methods'
    | a,b ->
	eq_stmt a b

let ok x y =
  OUnit.assert_equal ~cmp:(List.for_all2 eq_clos)
    x @@ Lisp.compile_string y

let syntax_error f =
  try
    f ();
    assert_failure "not raise"
  with Parsec.Syntax_error _ ->
    assert_bool "raised" true

let pos x n a b =
  {(Node.empty x) with
     Node.filename = "<string>";
     lineno        = n;
     start_pos     = a;
     end_pos       = b}

let _ =
  ("lisp module test" >::: [
     "pos" >::
       (fun () ->
	  ok (expr (`Int (pos 42 0 0 2)))
	    "42";
	  ok (expr (`String (pos "hoge" 0 0 6)))
	    "\"hoge\"";
	  ok (expr (`Bool   (pos true 0 0 2)))
	    "#t";
 	  ok (expr (`Var    (pos ("","foo") 0 0 3)))
	    "foo";
	  ok (expr (`Lambda ([pos "abc" 0 9 12],`Block [])))
	    "(lambda (abc))";
	  ok (expr (`Let ([pos "foo" 0 7 10,`Int (pos 42 0 11 13)],
			     `Block []))) @@
	    "(let [(foo 42)] )";
	  ok (expr (`LetRec ([pos "foo" 0 10 13,`Int (pos 42 0 14 16)],
				`Block []))) @@
	    "(letrec [(foo 42)] )";
	  ok (expr (`New (pos ("","Foo") 0 5 8 ,[]))) @@
	    "(new Foo)";
	  ok (expr (`Invoke (`Var (pos ("","foo") 0 3 6), pos "baz" 0 8 11,[]))) @@
	    "(. foo (baz))";
	  ok (expr (`SlotRef (`Var (pos ("","obj") 0 10 13),pos "name" 0 14 18))) @@
	    "(slot-ref obj name)";
	  ok (expr (`SlotSet (`Var (pos ("","obj") 0 11 14),
				pos "name" 0 15 19,
				 `Int (pos  42 0 20 22)))) @@
	    "(slot-set! obj name 42)";
	  ok [`Define (pos ("","x") 0 8 9,`Block [`Int (pos 42 0 10 12)])] @@
	    "(define x 42)";
	  ok [`Define (pos ("","f") 0 9 10,`Lambda ([pos "x" 0 11 12],`Block []))] @@
	    "(define (f x))";
	  ok [`DefineClass (pos ("","Foo") 0 14 17,
			      pos ("","Object") 0 19 25,
			      [pos "arg" 0 28 31])] @@
	    "(define-class Foo (Object) (arg))";
	  ok [`DefineMethod (pos "fun" 0 15 18,
			     (pos "self" 0 21 25,pos ("","Object") 0 26 32),
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
	  ok (expr (`Call [var @@ global "print"]))
	    "(print)";
	  ok (expr (`Call [var @@ global "print";string "hello"]))
	    "(print \"hello\")";
	  ok (expr (`Call [var @@ global "print";string "hello";string "world"]))
	    "(print \"hello\" \"world\")");
     "+" >::
       (fun () ->
	  ok (expr (`Call [var @@ global "+";int 1;int 2]))
	    "(+ 1 2)";
	  ok (expr (`Call [var @@ global "-";int 1;int 2]))
	    "(- 1 2)";
	  ok (expr (`Call [var @@ global "*";int 1;int 2]))
	    "(* 1 2)";
	  ok (expr (`Call [var @@ global "/";int 1;int 2]))
	    "(/ 1 2)");
     "<" >::
       (fun () ->
	  ok (expr (`Call [var @@ global "=";int 1;int 2]))
	    "(= 1 2)";
	  ok (expr (`Call [var @@ global "<";int 1;int 2]))
	    "(< 1 2)";
	  ok (expr (`Call [var @@ global "<=";int 1;int 2]))
	    "(<= 1 2)";
	  ok (expr (`Call [var @@ global ">";int 1;int 2]))
	    "(> 1 2)";
	  ok (expr (`Call [var @@ global ">=";int 1;int 2]))
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
			  `Block [var @@ global "x";var @@ global "y"])))
	    "(let ((x 1) (y 2)) x y)");
     "letrec" >::
       (fun () ->
	  ok (expr (`LetRec ([node "x",int 1;node "y",int 2],
			     `Block [var @@ global "x";var @@ global "y"])))
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
	  ok (expr (`Invoke (var @@ global "foo",node "baz",[int 1;int 2])))
	    "(. foo (baz 1 2))");
     "define" >::
       (fun () ->
	  ok [define (global  "x") @@ `Block [int 42]]
	    "(define x 42)";
	  ok [define (global "f") @@ `Lambda ([node "x"],
					 `Block [int 42])]
	    "(define (f x) 42)");
     "external" >::
       (fun () ->
	  ok [external_var @@ global "x"] "(external x)");
     "external-class" >::
       (fun () ->
	  ok [external_class (global "X") ["f";"g";"h"] ]
	    "(external-class X (f g h))");
     "bug()" >::
       (fun () ->
	  ok [`Expr (int 10);
	      define (global "x") @@ `Block [int 42]]
	    "10 (define x 42)");
     "class" >::
       (fun () ->
	  ok [define_class (global "Foo") (global "Object") ["x";"y"]]
	     "(define-class Foo (Object) (x y))";
	  ok [define_class (global "Foo") (qname "flash.text" "Object") ["x";"y"]]
	     "(define-class Foo (flash.text.Object) (x y))";
	  ok [define_class (global "Foo") (qname "flash.text" "Object") []]
	     "(define-class Foo (flash.text.Object) ())");
     "method" >::
       (fun () ->
	  ok [define_method  "f" "self" (global "Object") ["x";"y"] (`Block [int 42])]
	    "(define-method f ((self Object) x y) 42)");
     "slot-ref" >::
       (fun () ->
	  ok (expr (`SlotRef (var @@ global "obj",node "name")))
	    "(slot-ref obj name)");
     "slot-set!" >::
       (fun () ->
	  ok (expr (`SlotSet (var @@ global "obj",node "name",int 42)))
	    "(slot-set! obj name 42)");
     "syntax error" >::
       (fun () ->
	  syntax_error (fun () ->
			  Lisp.compile_string "(if a)");
	  syntax_error (fun () ->
			  Lisp.compile_string "(if a b c d)");
	  syntax_error (fun () ->
			  Lisp.compile_string "(external a b)"))
   ]) +> run_test_tt
