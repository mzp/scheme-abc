open Base
open OUnit

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

let string x =
  `String (node x)

let int x =
  `Int (node x)

let float x =
  `Float (node x)

let bool x =
  `Bool (node x)

let var x =
  `Var (node x)

let meth name args body : Ast.method_ =
  (node name,List.map node args,body)


let printer = 
  function 
      Val x -> 
	string_of_list @@ List.map Ast.to_string_stmt x
    | Err x -> 
	Node.to_string id x

let ok_s s =
  assert_equal 
    ~cmp:(fun a b -> 
	    match a,b with
		Val _,Val _ ->
		  true
	      | _ ->
		  false)
    ~printer:printer
    (Val []) @@
    BindCheck.trans s

let ok_e xs =
  ok_s [`Expr xs]

let _ =
  ("bindCheck.ml" >::: [
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
	  ok_s [`Define (node "x",`Block [var "x"])];
	  ok_s [`Define (node "x",`Block []);
		`Expr (var "x")]);
     "external" >::
       (fun () ->
	  ok_s [`External (node "x");
		`Expr (var "x")]);
     "external-class" >::
       (fun () ->
	  ok_s [`ExternalClass (node ("","Object"),[]);
		`Class (node "Foo",node ("","Object"),[],[])];
	  ok_s [`ExternalClass (node ("","Object"),[]);
		`Expr (`New (node ("","Object"),[]))];
	  ok_s [`ExternalClass (node ("","Object"),[node "f"; node "g"]);
		`External (node "obj");
		`Expr (`Invoke (var "obj",node "f",[]))]);
     "class" >::
       (fun () ->
	  ok_s [`ExternalClass (node ("","Object"),[]);
		`Class (node "Foo",node ("","Object"),[],[]);
		`Expr (`New (node ("","Foo"),[]))];
	  ok_s [`ExternalClass (node ("","Object"),[]);
		`Class (node "Foo",node ("","Object"),[],
			[(node "f",[],`Block [])]);
		`External (node "obj");
		`Expr (`Invoke (var "obj",node "f",[]))];
	  ok_s [`ExternalClass (node ("","Object"),[]);
		`External (node "obj");
		`Class (node "Foo",node ("","Object"),[],
			[(node "f",[],`Invoke (var "obj",node "f",[]))])])
   ]) +> run_test_tt
