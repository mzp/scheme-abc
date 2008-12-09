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
	    ok_e (`Let([node "x",int 42],var "x"));
	    ok_e (`Let([node "x",int 42],`Call [var "x"])));
       "let-let-var" >::
	 (fun () ->
	    ok_e (`Let ([node "x",int 42],
			`Let ([node "y",int 10],
			      var "x"))));
       "letrec" >::
	 (fun () ->
	    ok_e (`LetRec([node "xyz",int 42],var "xyz"));
	    ok_e (`LetRec([node "x",var "x"],var "x")));
       "lambda" >::
	 (fun () ->
	    ok_e (`Lambda ([node "x";node "y"],var "x"));
	    ok_e (`Lambda ([node "x";node "y"],var "y")));
       "define" >::
	 (fun () ->
	    ok_s [`Define (node ("","x"),`Block [var "x"])];
	    ok_s [`Define (node ("","x"),`Block []);
		  `Expr (var "x")]);
       "external" >::
	 (fun () ->
	    ok_s [external_var "x";
		  `Expr (var "x")]);
       "external-class" >::
	 (fun () ->
	    ok_s [external_class "Object" [];
		  klass "Foo" "Object" [] []];
	    ok_s [external_class "Object" [];
		  `Expr (new_klass "Object" [])];
	    ok_s [external_class "Object" ["f";"g"];
		  external_var "obj";
		  `Expr (invoke (var "obj") "f" [])]);
       "class" >::
	 (fun () ->
	    ok_s [external_class "Object" [];
		  klass "Foo" "Object" [] [];
		  `Expr (new_klass "Foo" [])];
	    ok_s [external_class "Object" [];
		  klass "Foo" "Object" [] [meth "f" [] (`Block [])];
		  external_var "obj";
		  `Expr (invoke (var "obj") "f" [] )];
	    ok_s [external_class "Object" [];
		  external_var "obj";
		  klass "Foo" "Object" []
		    [meth "f" [] (invoke (var "obj") "f" [])] ] );
       "class should be first class" >::
	 (fun () ->
	    ok_s [external_class "Object" [];
		  `Expr (var "Object")]);
     ];
     "invalid phase" >:::
       let x =
	 qname "x" in
       let f =
	 node "f" in
       let klass =
	 node ("","Fuga") in
	 [
	   "let-var" >::
	     (fun () ->
		ng_e (Unbound_var x) @@
		  `Let([node "not-x",int 42],`Var x));
	   "letrec-var" >::
	     (fun () ->
		ng_e (Unbound_var x) @@
		  `LetRec([node "not-x",int 42],`Var x);
		ng_e (Unbound_var x) @@
		  `LetRec([node "not-x",`Var x],`Block []));
	   "new" >::
	     (fun () ->
		ng_e (Unbound_class klass) @@
		  `New (klass,[]);
		ng_s (Unbound_class klass) @@
		  [`Class (x,klass,[],[])]);
	   "meth" >::
	     (fun () ->
		ng_e (Unbound_method f) @@
		  `Let ([node "hoge",int 42],`Invoke (var "hoge",f,[])))
	 ]
   ]) +> run_test_tt
