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

let x =
  global "x"

let y =
  global "y"

let member i name =
  `BindVar (node (Member ((Scope i),name)))

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
     ];
     "BindVar" >::: [
       "let-binding should bind variable" >::
	 (fun () ->
	    ok_e
	      (let1 "x" (int 42) (member 0 "x")) @@
	       let1 "x" (int 42) (var x));
       "initialize parts of letrec should be in scope" >::
	 (fun () ->
	    ok_e
	      (letrec1 "x" (member 0 "x") @@ block []) @@
	       letrec1 "x" (var x) @@ block []);
       "letrec should bind variable" >::
	 (fun () ->
	    ok_e
	      (letrec1 "x" (int 42) (member 0 "x")) @@
	       letrec1 "x" (int 42) (var x));
       "nest-let should works right" >::
	 (fun () ->
	    ok_e
	      (let1 "x" (int 0) (
		 let1 "x" (int 10) @@
		   member 1 "x")) @@
	       let1 "x" (int 0) (
		 let1 "x" (int 10) @@
		   var x));
       "lambda should bind varible" >::
	 (fun () ->
	    ok_e
	      (lambda ["x";"y"] @@ block [register 1;register 2]) @@
	       lambda ["x";"y"] @@ block [var x;var y])
     ]
   ]) +> run_test_tt

