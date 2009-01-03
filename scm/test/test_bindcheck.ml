open Base
open OUnit
open BindCheck
open AstUtil

let ok_s s =
  ignore @@ BindCheck.check s

let ok_e xs =
  ok_s [`Expr xs]

let ng_s exn s =
  assert_raises exn
    (fun () ->
       ignore @@ BindCheck.check s)

let ng_e exn xs =
  ng_s exn [`Expr xs]

let _ =
  ("bindCheck.ml" >::: [
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
       "external" >::
	 (fun () ->
	    ok_s [external_var @@ sname "x";
		  `Expr (var @@ global "x")]);
       "external-class" >::
	 (fun () ->
	    ok_s [external_class (sname "Object") [];
		  klass (sname "Foo") (global "Object") [] []];
	    ok_s [external_class (sname "Object") [];
		  `Expr (new_klass (global "Object") [])];
	    ok_s [external_class (sname "Object") ["f";"g"];
		  external_var @@ sname "obj";
		  `Expr (invoke (var @@ global "obj") "f" [])]);
       "class" >::
	 (fun () ->
	    ok_s [external_class (sname "Object") [];
		  klass (sname "Foo") (global "Object") [] [];
		  `Expr (new_klass (global "Foo") [])];
	    ok_s [external_class (sname "Object") [];
		  klass (sname "Foo") (global "Object") [] [public_meth "f" [] (`Block [])];
		  external_var @@ sname "obj";
		  `Expr (invoke (var @@ global "obj") "f" [] )];
	    ok_s [external_class (sname "Object") [];
		  external_var @@ sname "obj";
		  klass (sname "Foo") (global "Object") []
		    [public_meth "f" [] (invoke (var @@ global "obj") "f" [])] ] );
       "class should be first class" >::
	 (fun () ->
	    ok_s [external_class (sname "Object") [];
		  `Expr (var @@ global "Object")]);
       "internal should be accessed from inner moudle" >::
	 (fun () ->
	    ok_s [module_ "foo" (ModuleTrans.Restrict []) [
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
       let klass =
	 global "Fuga" in
	 [
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
		ng_e (Unbound_var klass) @@
		  `New (klass,[]);
		ng_s (Unbound_var klass) @@
		  [`Class (sname "x",klass,[],[])]);
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
		  [module_ "foo" (ModuleTrans.Restrict []) [
		     define (sname "x") @@ `Block []];
		   `Expr (var @@ qname "foo" "x")])

	 ]
   ]) +> run_test_tt

