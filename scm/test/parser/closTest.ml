open Base
open Clos
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
  Clos.to_ast (object
		 method mem_method _ = false
	       end)

let foo_class =
  class_ "Foo" ([],"Baz") []

let define_foo =
  define_class  "Foo" ([],"Baz") []

let _ =
  ("clos.ml" >::: [
     "pos" >::
       (fun () ->
	  let name =
	    pos "Foo" 0 1 3 in
	  let super =
	    pos (["bar"],"Baz") 0 5 8 in
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
	      trans [`DefineClass {Clos.class_name = name;
				   super=super;
				   attrs=attrs};
		     `DefineMethod {Clos.method_name = f;
				    to_class = obj;
				    args = self::args;
				    body = `Block []}]);
     "basic" >::
       (fun () ->
	  ok [foo_class
		[public_meth "f" ["self";"x"] (int 42)]] @@
	    trans [define_foo;
		   define_method "f" "Foo" ["self";"x"] (int 42)]);
     "static" >::
       (fun () ->
	  ok [foo_class
		[static_meth "f" ["x"] (int 42)]] @@
	    trans [define_foo;
		   define_static_method "f" "Foo" ["x"]  (int 42)]);
     "module" >::
       (fun () ->
	  ok [foo_mod [
		foo_class
		  [public_meth "f" ["self";"x"] (int 42)]]] @@
	    trans [foo_mod
		     [define_foo;
		      define_method "f" "Foo" ["self";"x"] (int 42)]]);
     "module with class 2" >::
       (fun () ->
	  ok [foo_mod [
		foo_class
		  [public_meth "f" ["self";"x"] (int 42);
		   public_meth "g" ["self";"x"] (int 42)]];
	      bar_mod [
		foo_class
		  [public_meth "f" ["self";"x"] (int 40)]]] @@
	    trans [foo_mod [define_foo;
			    define_method "g" "Foo" ["self";"x"] (int 42);
			    define_method "f" "Foo" ["self";"x"] (int 42)];
		   bar_mod [define_foo;
			    define_method "f" "Foo" ["self";"x"] (int 40)]]);
     "attributes" >::
       (fun () ->
	  ok [class_ "Foo" ([],"Baz") ["x";"y"] []] @@
	    trans [define_class "Foo" ([],"Baz") ["x";"y"]]);
     "plain is not change" >::
       (fun () ->
	  ok [`Expr (int 42)] @@
	    trans [`Expr (int 42)]);
     "define and plain is mixed" >::
       (fun () ->
	  ok [foo_class
		[public_meth "f" ["self";"x"] (int 42)];
	      `Expr (int 42)] @@
	    trans [define_foo;
		   `Expr (int 42);
		   define_method "f" "Foo" ["self";"x"] (int 42)]);
     "invoke" >::
       (fun () ->
	  ok [foo_class
		[public_meth "f" ["self";"x"] (int 42)];
	      expr (invoke (var [] "obj") "f" [int 10])] @@
	    trans [define_foo;
		   define_method "f" "Foo" ["self";"x"] (int 42);
		   expr @@ call [var [] "f"; var [] "obj"; int 10]]);
     "invoke deep" >::
       (fun () ->
	  ok [expr (`If (invoke (var [] "obj") "f" [int 10],
			 block [],
			 block []))] @@
	    trans [define_method "f" "Foo" ["self";"x"] (int 42);
		   expr (`If (call [var [] "f";var [] "obj";int 10],
			      `Block [],
			      `Block []))]);
     "invoke in methods" >::
       (fun () ->
	  ok [foo_class
		[public_meth "f" ["self"] @@
		   invoke (var [] "self") "f" []]] @@
	    trans [define_foo;
		   define_method "f" "Foo" ["self"] @@
		     call [var [] "f"; var [] "self"]]);
     "invoke with inter code" >::
       (fun () ->
	  let tbl = object
	    method mem_method f = f = "f"
	  end in
	    ok [expr @@ invoke (var [] "obj") "f" []] @@
	      Clos.to_ast tbl [expr @@ call [var [] "f";
					     var [] "obj"]]);
   ]) +> run_test_tt


