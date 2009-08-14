open Base
open OUnit
open AstUtil

let ok x y =
  assert_equal x (Module.of_ast [y])

let define_ name =
  `Define (name,int 0)

let class_  name =
  AstUtil.class_ name (qname [] "Object") [] []

let _ = ("module.ml" >::: [
	   "expr" >::
	     (fun () ->
		ok [ expr @@ int 42 ] @@
		  expr @@ int 42);
	   "define" >::
	     (fun () ->
		ok [ define_ (`Public (qname [] "foo")) ] @@
		  define_ (Node.ghost "foo"));
	   "class" >::
	     (fun () ->
		ok [ class_ (`Public (qname [] "Foo")) ] @@
		  class_ (Node.ghost "Foo"));
	   "module-all" >::
	     (fun () ->
		ok [
		  expr @@ int 0;
		  define_ (`Public (qname ["foo"] "foo")) ;
		  class_  (`Public (qname ["foo"] "Foo"))
		] @@
		  module_ "foo" `All [
		    expr @@ int 0;
		    define_ (Node.ghost "foo");
		    class_ (Node.ghost "Foo");
		  ]);
	   "module-nest" >::
	     (fun () ->
		ok [
		  expr @@ int 0;
		  define_ (`Public (qname ["foo";"bar"] "foo")) ;
		  class_  (`Public (qname ["foo";"bar"] "Foo"))
		] @@
		  module_ "foo" `All [
		    module_ "bar" `All [
		      expr @@ int 0;
		      define_ (Node.ghost "foo");
		      class_ (Node.ghost "Foo");
		    ]]);
	   "module-only" >::
	     (fun () ->
		ok [
		  expr @@ int 0;
		  define_ (`Public (qname ["foo"] "foo")) ;
		  class_  (`Internal (qname ["foo"] "Foo"))
		] @@
		  module_ "foo" (`Only [Node.ghost "foo"]) [
		    expr @@ int 0;
		    define_ (Node.ghost "foo");
		    class_ (Node.ghost "Foo");
		  ]);
	 ]) +> run_test_tt
