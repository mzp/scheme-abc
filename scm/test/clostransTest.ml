open Base
open ClosTrans
open Ast
open OUnit
open AstUtil

let ok x y =
  OUnit.assert_equal x y

let pos x n a b =
  {(Node.empty x) with
     Node.filename = "<string>";
     lineno        = n;
     start_pos     = a;
     end_pos       = b}

let trans =
  ClosTrans.trans InterCode.empty

let _ =
  ("closTrans.ml" >::: [
     "pos" >::
       (fun () ->
	  let name =
	    pos "Foo" 0 1 3 in
	  let super =
	    pos ("bar","Baz") 0 5 8 in
	  let attrs =
	    [pos "x" 0 9 10] in
	  let f =
	    pos "f" 1 0 1 in
	  let self =
	    pos "self" 1 3 5 in
	  let obj  =
	    pos "Foo" 1 6 8 in
	  let args =
	    [pos "x" 1 9 10] in
	    ok [`Class {Ast.class_name = name;
			super = super;
			attrs = attrs;
			methods = [{
			  Ast.method_name = `Public f;
			  args = self::args;
			  body = `Block []}]}] @@
	      trans [`DefineClass {ClosTrans.class_name = name;
				   super=super;
				   attrs=attrs};
		     `DefineMethod {ClosTrans.method_name = f;
				    to_class = obj;
				    args = self::args;
				    body = `Block []}]);
     "basic" >::
       (fun () ->
	  ok [klass (sname "Foo") (global "Baz") []
		[meth "f" ["self";"x"] (int 42)]] @@
	    trans [define_class  (sname "Foo") (global "Baz") [];
		   define_method "f"   "self" (sname "Foo") ["x"] (int 42)]);
     "static" >::
       (fun () ->
	  ok [klass (sname "Foo") (global "Baz") []
		[static_meth "f" ["x"] (int 42)]] @@
	    trans [define_class  (sname "Foo") (global "Baz") [];
		   define_static_method "f" (sname "Foo") ["x"]
		     (int 42)]);
     "module" >::
       (fun () ->
	  ok [foo_mod [
		klass (sname "Foo") (global "Baz") []
		  [meth "f" ["self";"x"] (int 42)]]] @@
	    trans [foo_mod
		     [define_class  (sname "Foo") (global "Baz") [];
		      define_method "f" "self" (sname "Foo") ["x"] (int 42)]]);
     "module with class 2" >::
       (fun () ->
	  ok [foo_mod [
		klass (sname "Foo") (global "Baz") []
		  [meth "f" ["self";"x"] (int 42);
		   meth "g" ["self";"x"] (int 42);]];
	      bar_mod [
		klass (sname "Foo") (global "Bar") []
		  [meth "f" ["self";"x"] (int 40)]]] @@
	    trans [foo_mod [define_class  (sname "Foo") (global "Baz") [];
			    define_method "g" "self" (sname "Foo") ["x"] (int 42);
			    define_method "f" "self" (sname "Foo") ["x"] (int 42)];
		   bar_mod [define_class  (sname "Foo") (global "Bar") [];
			    define_method "f" "self" (sname "Foo") ["x"] (int 40)]]);
     "attributes" >::
       (fun () ->
	  ok [klass (sname "Foo") (global "Baz") ["x";"y"] []] @@
	    trans [define_class  (sname "Foo") (global "Baz") ["x";"y"]]);
     "plain is not change" >::
       (fun () ->
	  ok [`Expr (int 42)] @@
	    trans [`Expr (int 42)]);
     "define and plain is mixed" >::
       (fun () ->
	  ok [klass (sname "Foo") (global "Baz") []
		[meth "f" ["self";"x"] (int 42)];
	      `Expr (int 42)] @@
	    trans [define_class (sname "Foo") (global "Baz") [];
		   `Expr (int 42);
		   define_method "f" "self" (sname "Foo") ["x"] (int 42)]);
     "invoke" >::
       (fun () ->
	  ok [klass (sname "Foo") (global "Baz") []
		[meth "f" ["self";"x"] (int 42)];
	      `Expr (`Invoke (var @@ global "obj",node "f",[int 10]))] @@
	    trans [define_class (sname "Foo") (global "Baz") [];
		   define_method "f" "self" (sname "Foo") ["x"] (int 42);
		   `Expr (`Call [var @@ global "f";
				 var @@ global "obj";
				 int 10])]);
     "invoke deep" >::
       (fun () ->
	  ok [`Expr (`If (invoke (var @@ global "obj") "f" [int 10],
			`Block [],
			`Block []))] @@
	    trans [define_method "f" "self" (sname "Foo") ["x"] (int 42);
		   `Expr (`If (`Call [var @@ global "f";var @@ global "obj";int 10],
					       `Block [],
					       `Block []))]);
     "invoke in methods" >::
       (fun () ->
	  ok [klass (sname "Foo") (global "Baz") []
		[meth "f" ["self"] @@
		   invoke (var @@ global "self") "f" []]] @@
	    trans [define_class (sname "Foo") (global "Baz") [];
		   define_method "f" "self" (sname "Foo") []
		     (`Call [var @@ global "f"; var @@ global "self"])]);
     "invoke with inter code" >::
       (fun () ->
	  let k m =
	    klass (sname "Bar") (global "Object") [] m in
	  let tbl =
	    InterCode.add_program InterCode.empty "Foo" @@
	      [k [meth "f" [] (int 42)]] in
	    ok [`Expr (`Invoke (var @@ global "obj",node "f",[]))] @@
	      ClosTrans.trans tbl [`Expr (`Call [var @@ global "f";
				       var @@ global "obj"])]);
   ]) +> run_test_tt

