open Base
open OUnit
open AstUtil

let ok x y =
  assert_equal x @@ Rename.rename y
let tests =
  [
    "other" >::
      (fun () ->
	 ok [expr @@ var [] "x";
	     expr @@ int 42]
	   [expr @@ var [] "x";
	    expr @@ int 42]);
    "no-rename" >::
      (fun () ->
	 ok [define "x" @@ int 0;
	     expr @@ var [] "x";
	     define "y" @@ int 1;
	     expr @@ var [] "y"]
	   [define "x" @@ int 0;
	    expr @@ var [] "x";
	    define "y" @@ int 1;
	    expr @@ var [] "y"]);
    "let" >::
      (fun () ->
	 ok [define "x$0" @@ int 0;
	     expr @@ let_ ["x",int 1] @@ var [] "x";
	     define "x" @@ int 1]
	   [define "x" @@ int 0;
	     expr @@ let_ ["x",int 1] @@ var [] "x";
	     define "x" @@ int 1]);
    "rename" >::
      (fun () ->
	 ok [define "x$0" @@ int 0;
	     expr @@ var [] "x$0";
	     define "x" @@ int 1;
	     expr @@ var [] "x"]
	   [define "x" @@ int 0;
	    expr @@ var [] "x";
	    define "x" @@ int 1;
	    expr @@ var [] "x"]);
    "seq" >::
      (fun () ->
	 ok [define "x$2" @@ int 0;
	     define "x$1" @@ int 0;
	     define "x$0" @@ int 0;
	     define "x" @@ int 0]
	   [define "x" @@ int 0;
	    define "x" @@ int 0;
	    define "x" @@ int 0;
	    define "x" @@ int 0]);
    "scope" >::
      (fun () ->
	 ok [define "x$0" @@ var [] "x$0";
	     define "x" @@ int 1]
	   [define "x" @@ var [] "x";
	    define "x" @@ int 1]);
    "module" >::
      (fun () ->
	 ok [foo_mod [
	      define "x$0" @@ int 0;
	      expr @@ var ["foo"] "x$0";
	      expr @@ var ["bar"] "x";
	      define "x" @@ int 0
	     ]]
	   [foo_mod [
	      define "x" @@ int 0;
	      expr @@ var ["foo"] "x";
	      expr @@ var ["bar"] "x";
	      define "x" @@ int 0
	    ]]);
    "class name" >::
      (fun () ->
	 ok [class_ "Foo$1" ([],"Object") [] [];
	     class_ "Foo$0" ([],"Object") [] [];
	     class_ "Foo" ([],"Object") [] []]
	   [class_ "Foo" ([],"Object") [] [];
	    class_ "Foo" ([],"Object") [] [];
	    class_ "Foo" ([],"Object") [] []]);
    "class method" >::
      (fun () ->
	 ok [ define "x$0" @@ int 0;
	      class_ "Foo" ([],"Object") [] [public_meth "f" [] (var [] "x$0")];
	      define "x" @@ int 0]
	   [ define "x" @@ int 0;
	     class_ "Foo" ([],"Object") [] [public_meth "f" [] (var [] "x")];
	     define "x" @@ int 0; ]);
  ]

let _  =
  run_test_tt ("rename.ml" >::: tests)

