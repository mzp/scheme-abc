open Base
open Ast

let set_of_list xs =
  PSet.set_of_list @@ List.map Node.value xs

let union xs =
  List.fold_left PSet.union PSet.empty xs

let (--) =
  PSet.diff

let (++) =
  PSet.union

let rec fold' f g env expr =
  Ast.fold f g (fold' f g) env expr

let free_variable expr =
  expr +> fold' const
    (fun env x ->
       match x with
	   `Var {Node.value = ("",x)} ->
	     PSet.singleton x
	 | `Int _ | `String _ | `Bool _ | `Float _ | `Var _ ->
	       PSet.empty
	 | `Lambda (args,expr) ->
	     expr -- (set_of_list args)
	 | `Let (decl,expr) ->
	     let xs =
	       union @@ List.map snd decl in
	     let vars =
	       set_of_list @@ List.map fst decl in
	       xs ++ (expr -- vars)
	 | `LetRec (decl,expr) ->
	     let xs =
	       union @@ List.map snd decl in
	     let vars =
	       set_of_list @@ List.map fst decl in
	       (xs ++ expr) -- vars
	 | `Call exprs | `Block exprs | `New (_,exprs) ->
	     List.fold_left (++) PSet.empty exprs
	 | `If (a,b,c) ->
	     a ++ b ++ c
	 | `Invoke (obj,_,args) ->
	     obj ++ List.fold_left (++) PSet.empty args
	 | `SlotRef (obj,_)  ->
	     obj
	 | `SlotSet (obj,_,value) ->
	     obj ++ value)
    PSet.empty

let add_let args body =
  match args with
      [] ->
	body
    | node::_ ->
	let fv =
	  PSet.to_list @@ PSet.inter (set_of_list args) (free_variable body) in
	  if fv = [] then
	    body
	  else
	    let decls =
	      List.map (fun var ->
			  let x =
			    {node with Node.value = var} in
			    (x,`Var {x with Node.value = ("",var)})) fv in
	      `Let (decls,body)

let lambda_wrap =
  Ast.map (function
	       `Lambda (args,body) ->
		 `Lambda (args,add_let args body)
	     | #Ast.expr as e ->
		 e)

let stmt_trans =
  function
      `Class ({Ast.methods=methods} as k) ->
	let methods' =
	  methods +> List.map
	    (fun ({Ast.body=body; args=args} as m) ->
	       {m with Ast.body=
		   add_let args body}) in
	  `Class {k with
		    methods = methods'}
    | #Ast.stmt as stmt ->
	lift lambda_wrap stmt

let trans program =
  List.map stmt_trans program
