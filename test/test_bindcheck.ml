open Base
open OUnit
open BindCheck

let count =
  ref 0
let pos ()=
  incr count;
  !count

let node x =
  {(Node.empty x) with
     Node.filename = "<string>";
     Node.lineno   = 0;
     start_pos     = pos ();
     end_pos       = pos ()}

let name x =
  node ("",x)


let string x =
  `String (node x)

let int x =
  `Int (node x)

let float x =
  `Float (node x)

let bool x =
  `Bool (node x)

let var x =
  `Var (node ("",x))

let meth name args body : Ast.method_ =
  (node name,List.map node args,body)

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
	    ok_s [`External (node ("","x"));
		  `Expr (var "x")]);
       "external-class" >::
	 (fun () ->
	    ok_s [`ExternalClass ((name "Object"),[]);
		  `Class ((name "Foo"),(name "Object"),[],[])];
	    ok_s [`ExternalClass ((name "Object"),[]);
		  `Expr (`New (node ("","Object"),[]))];
	    ok_s [`ExternalClass ((name "Object"),[node "f"; node "g"]);
		  `External (name "obj");
		  `Expr (`Invoke (var "obj",node "f",[]))]);
       "class" >::
	 (fun () ->
	    ok_s [`ExternalClass (name "Object",[]);
		  `Class (name "Foo",name "Object",[],[]);
		  `Expr (`New (name "Foo",[]))];
	    ok_s [`ExternalClass (name "Object",[]);
		  `Class (name "Foo",name "Object",[],
			  [(node "f",[],`Block [])]);
		  `External (name "obj");
		  `Expr (`Invoke (var "obj",node "f",[]))];
	    ok_s [`ExternalClass (node ("","Object"),[]);
		  `External (name "obj");
		  `Class (name "Foo",node ("","Object"),[],
			  [(node "f",[],`Invoke (var "obj",node "f",[]))])]);
       "class should be first class" >::
	 (fun () ->
	    ok_s [`ExternalClass (name "Object",[]);
		  `Expr (`Var (name "Object"))])
     ];
     "invalid phase" >:::
       let x =
	 name "x" in
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
