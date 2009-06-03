open Base
open OUnit
open Lisp
open Ast
open Clos
open AstUtil
open Node

let expr xs =
  [`Expr xs]

let eq_ident {value = x} {value = y} =
  x = y

let ok =
  assert_equal ~printer:Std.dump

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

let eq_method
    {Ast.method_name=mname;  args=args;  body=body}
    {Ast.method_name=mname'; args=args'; body=body'} =
  match mname,mname' with
      `Public name,`Public name' ->
	eq_ident name name' &&
	  (List.for_all2 eq_ident args args') &&
	  eq_expr body body'
    | `Static name,`Static name' ->
	eq_ident name name' &&
	  (List.for_all2 eq_ident args args') &&
	  eq_expr body body'
    | `Public _,`Static _ | `Static _,`Public _ ->
	false

let eq_exports a b =
  match a,b with
      `All,`All ->
	true
    | `Only xs, `Only ys ->
	List.for_all2 eq_ident xs ys
    | _ ->
	false

let rec eq_clos a b =
  match a,b with
      `DefineClass {Clos.class_name = name;
		    super = {value=super};
		    attrs = attrs},
      `DefineClass {Clos.class_name = name';
		    super = {value=super'};
		    attrs = attrs'} ->
	eq_ident name name' &&
	  super = super' && List.for_all2 eq_ident attrs attrs'
    | `DefineMethod {Clos.method_name=name;
		     to_class=obj;
		     args = args;
		     body = body},
      `DefineMethod {Clos.method_name=name';
		     to_class=obj';
		     args = args';
		     body = body'} ->
	eq_ident name name' &&
	  eq_ident obj obj' &&
	  (List.for_all2 eq_ident args args') &&
	  eq_expr body body'
    | `DefineStaticMethod {Clos.method_name=name;
			   to_class=obj;
			   args = args;
			   body = body},
      `DefineStaticMethod {Clos.method_name=name';
			   to_class=obj';
			   args = args';
			   body = body'} ->
	eq_ident name name' &&
	  eq_ident obj obj' &&
	  (List.for_all2 eq_ident args args') &&
	  eq_expr body body'
    | `Define (name,body), `Define (name',body') ->
	eq_ident name name' && eq_expr body body'
    | `Expr expr, `Expr expr' ->
	eq_expr expr expr'
    | `Module {Ast.module_name= name; exports=exports; stmts=stmts},
	`Module {Ast.module_name= name'; exports=exports'; stmts=stmts'} ->
	eq_ident name name' && eq_exports exports exports' &&
	  List.for_all2 eq_clos stmts stmts'
    | `Open {Node.value=x},`Open {Node.value=y} ->
	x = y
    | _ ->
	false

let ok x y =
  OUnit.assert_equal ~cmp:(List.for_all2 eq_clos)
    x @@ Lisp.parse_string y

let syntax_error f =
  try
    f ();
    assert_failure "not raise"
  with Parsec.Syntax_error _ ->
    assert_bool "raised" true


let define_class k super attrs =
  `DefineClass {Clos.class_name = Node.ghost k;
		super = Node.ghost super;
		attrs = List.map Node.ghost attrs}

let define_method f self obj args body =
  `DefineMethod {Clos.method_name = Node.ghost f;
		 to_class = Node.ghost obj;
		 args = List.map Node.ghost (self::args);
		 body = body}

let define_static_method f obj args body =
  `DefineStaticMethod {Clos.method_name = Node.ghost f;
		       to_class = Node.ghost obj;
		       args = List.map Node.ghost args;
		       body = body}


let pos x n a b =
  {(Node.empty x) with
     Node.filename = "<string>";
     lineno        = n;
     start_pos     = a;
     end_pos       = b}

let _ =
  ("lisp.ml" >::: [
     "pos" >::: [
       "int" >::
	 (fun () ->
	    ok (expr (`Int (pos 42 0 0 2)))
	      "42");
       "string" >::
	 (fun () ->
	    ok (expr (`String (pos "hoge" 0 0 6)))
	      "\"hoge\"");
       "bool" >::
	 (fun () ->
	    ok (expr (`Bool   (pos true 0 0 2)))
	      "#t");
       "var" >::
	 (fun () ->
 	    ok (expr (`Var    (pos ([],"foo") 0 0 3)))
	      "foo");
       "lambda" >::
	 (fun () ->
	    ok (expr (`Lambda ([pos "abc" 0 9 12],`Block [])))
	      "(lambda (abc))");
       "let" >::
	 (fun () ->
	    ok (expr (`Let ([pos "foo" 0 7 10,`Int (pos 42 0 11 13)], `Block []))) @@
	      "(let [(foo 42)] )");
       "letrec" >::
	 (fun () ->
	    ok (expr (`LetRec ([pos "foo" 0 10 13,`Int (pos 42 0 14 16)], `Block []))) @@
	      "(letrec [(foo 42)] )");
       "new" >::
	 (fun () ->
	    ok (expr (`New (pos ([],"Foo") 0 5 8 ,[]))) @@
	      "(new Foo)");
       "invoke" >::
	 (fun () ->
	    ok (expr (`Invoke (`Var (pos ([],"foo") 0 3 6), pos "baz" 0 8 11,[]))) @@
	      "(. foo (baz))");
       "slot-ref" >::
	 (fun () ->
	    ok (expr (`SlotRef (`Var (pos ([],"obj") 0 10 13),pos "name" 0 14 18))) @@
	      "(slot-ref obj name)");
       "slot-set!" >::
	 (fun () ->
	    ok (expr (`SlotSet (`Var (pos ([],"obj") 0 11 14),
				pos "name" 0 15 19,
				`Int (pos  42 0 20 22)))) @@
	      "(slot-set! obj name 42)");
       "define value" >::
	 (fun () ->
	    ok [`Define (pos ("x") 0 8 9,`Block [`Int (pos 42 0 10 12)])] @@
	      "(define x 42)");
       "define lambda" >::
	 (fun () ->
	    ok [`Define (pos ("f") 0 9 10,`Lambda ([pos "x" 0 11 12],`Block []))] @@
	    "(define (f x))");
       "define-class" >::
	 (fun () ->
	    ok [`DefineClass {Clos.class_name = pos ("Foo") 0 14 17;
			      super = pos ([],"Object") 0 19 25;
			      attrs = [pos "arg" 0 28 31]}] @@
	      "(define-class Foo (Object) (arg))");
       "define-method" >::
	 (fun () ->
	    ok [`DefineMethod {Clos.method_name = pos "fun" 0 15 18;
			       to_class = pos ("Object") 0 26 32;
			       args = [pos "self" 0 21 25;pos "xyz" 0 34 37];
			       body = `Block []}] @@
	      "(define-method fun ((self Object) xyz))");
       "module" >::
	 (fun () ->
	    ok [`Module {
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
	  ok [define_class "Foo" ([],"Object") ["x";"y"]]
	     "(define-class Foo (Object) (x y))";
	  ok [define_class "Foo" (["flash";"text"],"Object") ["x";"y"]]
	     "(define-class Foo (flash.text.Object) (x y))";
	  ok [define_class "Foo" (["flash";"text"],"Object") []]
	     "(define-class Foo (flash.text.Object) ())");
     "method" >::
       (fun () ->
	  ok [define_method  "f" "self" "Object" ["x";"y"] (`Block [int 42])]
	    "(define-method f ((self Object) x y) 42)");
     "static method" >::
       (fun () ->
	  ok [define_static_method  "f" "Object" ["x";"y"] (`Block [int 42])]
	    "(define-static-method f (Object x y) 42)");
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
			  Lisp.parse_string "(if a)");
	  syntax_error (fun () ->
			  Lisp.parse_string "(if a b c d)"))
   ]) +> run_test_tt
