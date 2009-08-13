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
    "rename" >::
      (fun () ->
	 ok [define "x_0" @@ int 0;
	     expr @@ var [] "x_0";
	     define "x" @@ int 1;
	     expr @@ var [] "x"]
	   [define "x" @@ int 0;
	    expr @@ var [] "x";
	    define "x" @@ int 1;
	    expr @@ var [] "x"]);
    "scope" >::
      (fun () ->
	 ok [define "x_0" @@ var [] "x_0";
	     define "x" @@ int 1]
	   [define "x" @@ var [] "x";
	    define "x" @@ int 1])
  ]

let _  =
  run_test_tt ("rename.ml" >::: tests)

