open Base
open PSet

let set_of_list xs =
  PSet.set_of_list @@ List.map Node.value xs

let free_variable expr =
  expr +> Ast.fix_fold Module.fold
    begin fun env x ->
      match x with
	  `Var {Node.value = ([],x)} ->
	    PSet.singleton x
	| `Int    _ | `String _ | `Bool   _ | `Float   _ | `Var     _
	| `Lambda _ | `Let    _ | `LetRec _ | `Call    _ | `Block   _
	| `New    _ | `If     _ | `Invoke _ | `SlotRef _ | `SlotSet _
	| `Array  _ ->
	    PSet.empty
    end
    begin fun env x ->
       match x with
	   `Lambda (args,expr) ->
	     expr -- (set_of_list args)
	 | `Let (decl,expr) ->
	     let xs =
	       union_list @@ List.map snd decl in
	     let vars =
	       set_of_list @@ List.map fst decl in
	       xs ++ (expr -- vars)
	 | `LetRec (decl,expr) ->
	     let xs =
	       union_list @@ List.map snd decl in
	     let vars =
	       set_of_list @@ List.map fst decl in
	       (xs ++ expr) -- vars
	 | `Array exprs | `Call exprs | `Block exprs | `New (_,exprs) ->
	     List.fold_left (++) PSet.empty exprs
	 | `If (a,b,c) ->
	     a ++ b ++ c
	 | `Invoke (obj,_,args) ->
	     obj ++ List.fold_left (++) PSet.empty args
	 | `SlotRef (obj,_)  ->
	     obj
	 | `SlotSet (obj,_,value) ->
	     obj ++ value
	 | `Var _ | `Int _ | `String _ | `Bool _ | `Float _  ->
	     env
    end
    PSet.empty

let add_let args body =
  match args with
      [] ->
	body
    | node::_ ->
	open Node in
	let decls =
	  body
	  +> free_variable
	  +> PSet.inter (set_of_list args)
	  +> PSet.to_list
	  +> List.map (fun var ->
			 let name =
			   {node with value = var} in
			 let var =
			   {node with value = ([],var)} in
			   (name,`Var var)) in
	  if decls = [] then
	    body
	  else
	    `Let (decls,body)

let lambda_wrap =
  Ast.map Module.fold begin function
      `Lambda (args,body) ->
	`Lambda (args,add_let args body)
    | #Module.expr as e ->
	e
  end

let stmt_trans s =
  open Ast in
  match s with
      `Class c ->
	let methods' =
	  c.methods
	  +> List.map (fun m ->
			 {m with body =
			     add_let m.args @@ lambda_wrap m.body}) in
	  `Class {c with methods = methods'}
    | #Module.stmt as stmt ->
	Module.lift lambda_wrap stmt

let trans program =
  List.map stmt_trans program
