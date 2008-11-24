open Base
open ClosTrans
open Ast
open OUnit

let ok x y =
  OUnit.assert_equal ~printer:(string_of_list $ List.map Ast.to_string_stmt) x y

let node x =
  {(Node.empty x) with Node.filename = "<string>"; Node.lineno = 0}

let pos x n a b =
  {(Node.empty x) with 
     Node.filename = "<string>";
     lineno        = n;
     start_pos     = a;
     end_pos       = b}

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

let meth name args body =
  (node name,List.map node args,body)

let klass name super attrs methods =
  `Class (node name,node super,List.map node attrs,methods)

let define_class name super attrs =
  `DefineClass (node name,node super,List.map node attrs)

let define_method name self obj args body =
  `DefineMethod (node name,(node self,node obj),List.map node args,body)

let _ =
  ("clos module test" >::: [
     "pos" >::
       (fun () ->
	  let klass =
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
	    ok [`Class (klass,super,attrs,
		       [f,self::args,`Block []])] @@
	      trans [`DefineClass (klass,super,attrs);
		     `DefineMethod(f,(self,obj),args,`Block [])]);
     "basic" >:: 
       (fun () ->
	  ok [klass "Foo" ("bar","Baz") []
		[meth "f" ["self";"x"] (int 42)]] @@ 
	    trans [define_class  "Foo" ("bar","Baz") [];
		   define_method "f"   "self" "Foo" ["x"] (int 42)]);
     "attributes" >::
       (fun () ->
	  ok [klass "Foo" ("bar","Baz") ["x";"y"] []] @@
	    trans [define_class  "Foo" ("bar","Baz") ["x";"y"]]);
     "plain is not change" >::
       (fun () ->
	  ok [`Expr (int 42)] @@ 
	    trans [`Expr (int 42)]);
     "define and plain is mixed" >::
       (fun () ->
	  ok [klass "Foo" ("bar","Baz") []
		[meth "f" ["self";"x"] (int 42)];
	      `Expr (int 42)] @@
       trans [define_class "Foo" ("bar","Baz") [];
	      `Expr (int 42);
	      define_method "f" "self" "Foo" ["x"] (int 42)]);
     "invoke" >::
       (fun () ->
	  ok [klass "Foo" ("bar","Baz") []
		[meth "f" ["self";"x"] (int 42)];
	      `Expr (`Invoke (var "obj",node "f",[int 10]))] @@
	    trans [define_class  "Foo" ("bar","Baz") [];
		   define_method "f" "self" "Foo" ["x"] (int 42);
		   `Expr (`Call [var "f";var "obj";int 10])]);
     "invoke deep" >::
       (fun () ->
	  ok [`Expr (`If (`Invoke (var "obj",node "f",[int 10]),
			`Block [],
			`Block []))] @@
	    trans [define_method "f" "self" "Foo" ["x"] (int 42);
		   `Expr (`If (`Call [var "f";var "obj";int 10],
			       `Block [],
			       `Block []))])
   ]) +> run_test_tt

