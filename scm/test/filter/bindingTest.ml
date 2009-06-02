open Base
open OUnit
open Binding
open AstUtil

let module_ ns xs =
  List.map (fun name -> ([ns],name)) xs

let table = object
  method mem_symbol qname =
    List.mem qname @@ List.concat [
      module_ "foo" [
	"x"; "Bar"
      ];
      module_ "std" [
	"Object"; "obj"
      ]
    ]

  method mem_method name =
    List.mem name ["f"; "g"]
end

let empty = object
  method mem_symbol qname = false
  method mem_method name = false
end

let ok x y =
  OUnit.assert_equal ~printer:Std.dump
    x (Binding.bind table y)

let any = block []

let ok_s program =
  ignore @@ Binding.bind table program

let ok_e expr =
  ok_s [`Expr expr]

let ng_s exn s =
  assert_raises exn
    (fun () ->
       ignore @@ Binding.bind empty s)

let ng_e exn xs =
  ng_s exn [`Expr xs]

let unbound_x =
  Unbound_var (qname [] "x")

let _ =
  ("bindCheck.ml" >::: [
     "external" >::: [
       "binds x" >::
	 (fun () ->
	    ok_e @@ var ["foo"] "x");
       "binds class Bar" >::
	 (fun () ->
	    ok_e @@ new_ ["foo"] "Bar" []);
       "binds method f" >::
	 (fun () ->
	    ok_e @@
	      invoke (var ["std"] "obj") "f" []);
     ];
     "var" >::: [
       "unbound" >::
	 (fun () ->
	    ng_e unbound_x @@
	      var [] "x");
       "acess nested module" >::
	 (fun () ->
	    ok_s [foo_mod [
		    bar_mod [
		      define "x" @@ block []]];
		  expr (var ["foo";"bar"] "x")])
     ];
     "let/let-rec" >::: [
       "binds x" >::
	 (fun () ->
	    ok_e @@ let_ ["x", int 42] @@ var  [] "x";
	    ok_e @@ let_ ["x", int 42] @@ call [var [] "x"]);
       "cloud nest" >::
	 (fun () ->
	    ok_e @@
	      let_ ["x", int 42] @@
	        let_ ["y", int 10] @@
	          var [] "x");
       "binds its declation" >::
	 (fun () ->
	    ok_e @@ let_rec ["xyz", int 42]   @@ var [] "xyz";
	    ok_e @@ let_rec ["x", var [] "x"] @@ var [] "x");
       "not bind other" >::
	 (fun () ->
	    ng_e (Unbound_var (qname [] "x")) @@
	      let_ ["not-x", int 42] @@ var [] "x");
       "letrec-var" >::
	 (fun () ->
	    ng_e unbound_x @@
	      let_rec ["not-x", int 42] @@ var [] "x";
	    ng_e unbound_x @@
	      let_rec ["not-x", var [] "x"] @@ block []);
     ];
     "define" >::: [
       "binds its name" >::
	 (fun () ->
	    ok_s [define "x" @@
		    block [var [] "x"]];
	    ok_s [define "x" @@ block [];
		  expr @@ var [] "x"]);
       "not binds other name" >::
	 (fun () ->
	    ng_s unbound_x @@
	      [define "y" @@ block [];
	       expr @@ var [] "x"]);
     ];
     "class" >::: [
       "class" >::
	 (fun () ->
	    ok_s [class_ "Foo" (["std"],"Object") [] [];
		  expr (new_ [] "Foo" [])];
	    ok_s [class_ "Foo" (["std"],"Object") [] [public_meth "f" [] (block [])];
		  define "obj" (int 42);
		  expr (invoke (var [] "obj") "f" [] )];
	    ok_s [define "obj" (int 42);
		  class_ "Foo" (["std"],"Object") [] [
		    public_meth "f" [] (invoke (var [] "obj") "f" [])] ] );
       "class should be first class" >::
	 (fun () ->
	    ok_s [expr (var ["std"] "Object")]);
       "new" >::
	 (fun () ->
	    ng_e (Unbound_var (qname [] "Fuga")) @@
	      new_ [] "Fuga" [];
	    ng_s (Unbound_var (qname [] "Fuga")) @@ [
	      class_ "Foo" ([],"Fuga") [] []]);
       "method" >::
	 (fun () ->
	    ng_e (Unbound_method (Node.ghost "f")) @@
	      let_ ["hoge", int 42] (
	        invoke (var [] "hoge") "f" []))
     ];
     "module" >::: [
       "scope" >::
	 (fun () ->
	    ok_s [AstUtil.module_ "foo" (`Only []) [
		    define "x" @@ block [];
		    expr (var [] "x")]]);
       "deep scope" >::
	 (fun () ->
	    ok_s [foo_mod [
		    bar_mod [
		      define "x" @@ block []
		    ];
		    expr (var ["bar"] "x")]]);
       "internal" >::
	 (fun () ->
	    ok_s [AstUtil.module_ "foo" (`Only []) [
		    define "x" @@ block [];
		    expr (var ["foo"] "x")]]);
       "internal(borbidden)" >::
	 (fun () ->
	    ng_s (Forbidden_var (qname ["foo"] "x"))
	      [AstUtil.module_ "foo" (`Only []) [
		 define "x" @@ block []];
	       expr (var ["foo"] "x")]);
     ];
     "other" >::: [
       "lambda" >::
	 (fun () ->
	    ok_e ( lambda ["x"; "y"] (var [] "x"));
	    ok_e ( lambda ["x"; "y"] (var [] "y")));
     ];
     "binding" >::: [
       "expr" >::
	 (fun () ->
	    ok
	      [foo_mod [ define "x" any;
			 expr (var ["foo"] "x")]]
	      [foo_mod [ define "x" any;
			 expr (var [] "x")]]);
       "expr(not module)" >::
	 (fun () ->
	    ok
	      [foo_mod [ define "x" any];
	       expr (var ["foo"] "x")]
	      [foo_mod [ define "x" any];
	       expr (var ["foo"] "x")]);
       "define" >::
	 (fun () ->
	    ok
	      [foo_mod [ define "x" @@ var ["foo"] "x" ]]
	      [foo_mod [ define "x" @@ var [] "x" ]]);
       "class" >::
	 (fun () ->
	    let c expr =
	      class_ "Foo" (["std"],"Object") []
		[public_meth "f" [] expr] in
	      ok
		[foo_mod [ define "x" any;
			   c (var ["foo"] "x")]]
		[foo_mod [ define "x" any;
			   c (var [] "x")]])];
     "open" >::
       (fun () ->
	  ok
	    [ `Open (Node.ghost ["foo"]);
	      expr (var ["foo"] "x")]
	    [ `Open (Node.ghost ["foo"]);
	      expr (var [] "x" )]);
     "open(std)" >::
       (fun () ->
	  ok
	    [ expr (var ["std"] "obj")]
	    [ expr (var [] "obj" )]);

   ]) +> run_test_tt

