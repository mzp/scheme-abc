open Base
open OUnit
open VarResolve
open AstUtil

let ok x y =
  OUnit.assert_equal
    x @@ trans y

let ok_e x y =
  ok [`Expr x] [`Expr y]

let define x y =
  `Define (x,y)

let redefine x expr =
  `ReDefine (x,expr)


let x =
  global "x"

let y =
  global "y"

let member i name =
  `BindVar (node (Member ((Scope i),name)))

let slot i j =
  `BindVar (node (Slot ((Scope i),j)))

let register i =
  `BindVar (node (Register i))

let _ =
  ("varResolve.ml" >::: [
     "Var" >::: [
       "free variable should be preserved" >::
	 (fun () ->
	    ok_e (var x) (var x));
       "free variable in let should be preserved" >::
	 (fun () ->
	    ok_e (let1 "x" (int 42) (var y)) @@
	      (let1 "x" (int 42) (var y)));
       "intialize parts of let should be out-of let scope." >::
	 (fun () ->
	    ok_e
	      (let1 "x" (var x) @@ block []) @@
	       let1 "x" (var x) @@ block []);
       "lambda should reset environment" >::
	 (fun () ->
	    ok_e
	      (let1 "x" (int 42) @@ lambda [] @@ block [var x]) @@
	       let1 "x" (int 42) @@ lambda [] @@ block [var x]);
       "class method should not be bound by define " >::
	 (fun () ->
	    ok [ define (`Public x) @@ block [];
		 klass (`Public x) y [] [public_meth "f" [] (var x)]]
	       [ define (`Public x) @@ block [];
		 klass (`Public x) y [] [public_meth "f" [] (var x)]]);
     ];
     "BindVar" >::: [
       "let-binding should bind variable" >::
	 (fun () ->
	    ok_e
	      (let1 "x" (int 42) (member 1 "x")) @@
	       let1 "x" (int 42) (var x));
       "initialize parts of letrec should be in scope" >::
	 (fun () ->
	    ok_e
	      (letrec1 "x" (member 1 "x") @@ block []) @@
	       letrec1 "x" (var x) @@ block []);
       "letrec should bind variable" >::
	 (fun () ->
	    ok_e
	      (letrec1 "x" (int 42) (member 1 "x")) @@
	       letrec1 "x" (int 42) (var x));
       "nest-let should works right" >::
	 (fun () ->
	    ok_e
	      (let1 "x" (int 0) (
		 let1 "x" (int 10) @@
		   member 2 "x")) @@
	       let1 "x" (int 0) (
		 let1 "x" (int 10) @@
		   var x));
       "lambda should bind varible" >::
	 (fun () ->
	    ok_e
	      (lambda ["x";"y"] @@ block [register 1;register 2]) @@
	       lambda ["x";"y"] @@ block [var x;var y]);
       "define should bind its own name" >::
	 (fun () ->
	    ok [define (`Public x) (int 42);
		`Expr (member 0 "x")]
	      [define (`Public x) (int 42);
	       `Expr (var x)]);
       "define scope should contain its own body" >::
	 (fun () ->
	    ok [define (`Public x) (member 0 "x")]
	      [define (`Public x) (var x)]);
       "multiple define should be converted to redefine" >::
	 (fun () ->
	    ok [define   (`Public x) @@ block [];
		redefine (`Public x) @@ block [];
		`Expr (member 1 "x")]
	      [define (`Public x) @@ block [];
	       define (`Public x) @@ block [];
		`Expr (var x)]);
       "define scope should not increment" >::
	 (fun () ->
	    ok [define (`Public x) @@ block [];
		define (`Public y) @@ block [];
		`Expr (member 0 "y")]
	      [define (`Public x) @@ block [];
	       define (`Public y) @@ block [];
		`Expr (var y)]);
       "class should bind its own name" >::
	 (fun () ->
	    ok [klass (`Public x) y [] [];
		`Expr (member 0 "x")]
	      [klass (`Public x) y [] [];
	       `Expr (var x)]);
     ]
   ]) +> run_test_tt

