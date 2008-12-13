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
       "let" >::
	 (fun () ->
	    ok_e (`Let([sname "x",int 42],var @@ global "x"));
	    ok_e (`Let([sname "x",int 42],`Call [var @@ global "x"])));
       "let-let-var" >::
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
	    ok_s [define (`Public (global "x")) @@ `Block [var @@ global "x"]];
	    ok_s [define (`Public (global "x")) @@ `Block [];
		  `Expr (var @@ global "x")]);
       "external" >::
	 (fun () ->
	    ok_s [external_var @@ global "x";
		  `Expr (var @@ global "x")]);
       "external-class" >::
	 (fun () ->
	    ok_s [external_class (global "Object") [];
		  klass (`Public (global "Foo")) (global "Object") [] []];
	    ok_s [external_class (global "Object") [];
		  `Expr (new_klass (global "Object") [])];
	    ok_s [external_class (global "Object") ["f";"g"];
		  external_var @@ global "obj";
		  `Expr (invoke (var @@ global "obj") "f" [])]);
       "class" >::
	 (fun () ->
	    ok_s [external_class (global "Object") [];
		  klass (`Public (global "Foo")) (global "Object") [] [];
		  `Expr (new_klass (global "Foo") [])];
	    ok_s [external_class (global "Object") [];
		  klass (`Public (global "Foo")) (global "Object") [] [public_meth "f" [] (`Block [])];
		  external_var @@ global "obj";
		  `Expr (invoke (var @@ global "obj") "f" [] )];
	    ok_s [external_class (global "Object") [];
		  external_var @@ global "obj";
		  klass (`Public (global "Foo")) (global "Object") []
		    [public_meth "f" [] (invoke (var @@ global "obj") "f" [])] ] );
       "class should be first class" >::
	 (fun () ->
	    ok_s [external_class (global "Object") [];
		  `Expr (var @@ global "Object")]);
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
		ng_e (Unbound_class klass) @@
		  `New (klass,[]);
		ng_s (Unbound_class klass) @@
		  [`Class (`Public x,klass,[],[])]);
	   "meth" >::
	     (fun () ->
		ng_e (Unbound_method f) @@
		  `Let ([sname "hoge",int 42],
			`Invoke ((var @@ global "hoge"),f,[])))
	 ]
   ]) +> run_test_tt

