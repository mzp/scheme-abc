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

let ok_s program =
  ignore @@ Binding.check table program

let ok_e expr =
  ok_s [`Expr expr]

let ng_s exn s =
  assert_raises exn
    (fun () ->
       ignore @@ Binding.check empty s)

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
	      invoke (var [] "obj") "f" []);
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
		  expr (var ["foo";"foo"] "x")])
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
     ];
     "other" >::: [
       "lambda" >::
	 (fun () ->
	    ok_e (`Lambda ([sname "x";sname "y"],var @@ global "x"));
	    ok_e (`Lambda ([sname "x";sname "y"],var @@ global "y")));
       "class" >::
	 (fun () ->
	    ok_s [klass (sname "Foo") (global "Object") [] [];
		  `Expr (new_klass (global "Foo") [])];
	    ok_s [klass (sname "Foo") (global "Object") [] [public_meth "f" [] (`Block [])];
		  define (sname "obj") (int 42);
		  `Expr (invoke (var @@ global "obj") "f" [] )];
	    ok_s [define (sname "obj") (int 42);
		  klass (sname "Foo") (global "Object") []
		    [public_meth "f" [] (invoke (var @@ global "obj") "f" [])] ] );
       "class should be first class" >::
	 (fun () ->
	    ok_s [`Expr (var @@ global "Object")]);
       "internal should be accessed from inner moudle" >::
	 (fun () ->
	    ok_s [module_ "foo" (`Only []) [
		    define (sname "x") @@ `Block [];
		    `Expr (var @@ qname "foo" "x")]]);
     ];
     "invalid phase" >:::
       let x =
	 global "x" in
       let f =
	 sname "f" in
       let fuga_klass =
	 global "Fuga" in
	 [
	   "new" >::
	     (fun () ->
		ng_e (Unbound_var fuga_klass) @@
		  `New (fuga_klass,[]);
		ng_s (Unbound_var fuga_klass) @@
		  [klass (sname "x") fuga_klass [] []]);
	   "meth" >::
	     (fun () ->
		ng_e (Unbound_method f) @@
		  `Let ([sname "hoge",int 42],
			`Invoke ((var @@ global "hoge"),f,[])));
	   "define" >::
	     (fun () ->
		ng_s (Unbound_var x)
		  [define (sname "y") @@ `Block [];
		   `Expr (var @@ x)]);
	   "internal should not access from outter-moudle" >::
	     (fun () ->
		ng_s (Forbidden_var (qname "foo" "x"))
		  [module_ "foo" (`Only []) [
		     define (sname "x") @@ `Block []];
		   `Expr (var @@ qname "foo" "x")])

	 ]
   ]) +> run_test_tt

