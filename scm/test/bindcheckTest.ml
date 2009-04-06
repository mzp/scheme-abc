open Base
open OUnit
open BindCheck
open AstUtil

let table =
  let x =
    InterCode.add InterCode.empty "foo" @@
      InterCode.of_program
      [define (`Public (global "x")) (int 42);
       klass (`Public (global "Bar")) (global "Object") [] [
	 public_meth "f" [] (int 42);
	 static_meth "g" [] (int 42);
       ]] in
    InterCode.add x "std" @@
      InterCode.of_program
      [
	klass (`Public (global "Object")) (global "Object") [] [
	  public_meth "f" [] (int 42)
	]]

let ok_s s =
  ignore @@ BindCheck.check table s

let ok_e xs =
  ok_s [`Expr xs]

let ng_s exn s =
  assert_raises exn
    (fun () ->
       ignore @@ BindCheck.check InterCode.empty s)

let ng_e exn xs =
  ng_s exn [`Expr xs]

let _ =
  ("bindCheck.ml" >::: [
     "external" >::: [
       "external module bind x" >::
	 (fun () ->
	    ignore @@ BindCheck.check table [`Expr (var (qname "foo" "x"))]);
       "external module bind class Bar" >::
	 (fun () ->
	    ignore @@ BindCheck.check table
	      [`Expr (new_klass (qname "foo" "Bar") [])]);
       "external module bind method f" >::
	 (fun () ->
	    ignore @@ BindCheck.check table
	      [define (sname "obj") (int 42);
	       `Expr (invoke (var @@ global "obj") "f" [])]);
       "static method" >::
	 (fun () ->
	    ignore @@ BindCheck.check table
	      [define (sname "obj") (int 42);
	       `Expr (invoke (var @@ global "obj") "g" [])]);
     ];
     "valid phase" >::: [
       "let should bind x" >::
	 (fun () ->
	    ok_e (`Let([sname "x",int 42],var @@ global "x"));
	    ok_e (`Let([sname "x",int 42],`Call [var @@ global "x"])));
       "nested let should work well" >::
	 (fun () ->
	    ok_e (`Let ([sname "x",int 42],
			`Let ([sname "y",int 10],
			      var @@ global "x"))));
       "letrec" >::
	 (fun () ->
	    ok_e (`LetRec([sname "xyz",int 42],var @@ global "xyz"));
	    ok_e (`LetRec([sname "x",var @@ global "x"],var @@ global "x")));
       "lambda" >::
	 (fun () ->
	    ok_e (`Lambda ([sname "x";sname "y"],var @@ global "x"));
	    ok_e (`Lambda ([sname "x";sname "y"],var @@ global "y")));
       "define" >::
	 (fun () ->
	    ok_s [define (sname "x") @@
		    `Block [var @@ global "x"]];
	    ok_s [define (sname "x") @@ `Block [];
		  `Expr (var @@ global "x")]);
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
       "foo.foo.x should be accessed" >::
	 (fun () ->
	    ok_s [foo_mod [
		    foo_mod [
		      define (sname "x") @@ `Block []]];
		  `Expr (var @@ qname "foo.foo" "x")])
     ];
     "invalid phase" >:::
       let x =
	 global "x" in
       let f =
	 sname "f" in
       let fuga_klass =
	 global "Fuga" in
	 [
	   "var" >::
	     (fun () ->
		ng_e (Unbound_var x) @@
		  `Var x);
	   "let-var" >::
	     (fun () ->
		ng_e (Unbound_var x) @@
		  `Let([sname "not-x",int 42],`Var x));
	   "letrec-var" >::
	     (fun () ->
		ng_e (Unbound_var x) @@
		  `LetRec([sname "not-x",int 42],`Var x);
		ng_e (Unbound_var x) @@
		  `LetRec([sname "not-x",`Var x],`Block []));
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

