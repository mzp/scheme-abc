open Base
open Ast

let set_of_list xs =
  List.fold_left (flip PSet.add) PSet.empty @@ 
    List.map Node.value xs

let union xs =
  List.fold_left PSet.union PSet.empty xs

let rec free_variable =
  function
      Lambda (args,expr) ->
	PSet.diff (free_variable expr) (set_of_list args)
    | Let (decl,expr) ->
	let xs = 
	  union @@ List.map (free_variable$snd) decl in
	let vars =
	  set_of_list @@ List.map fst decl in
	let ys =
	  PSet.diff (free_variable expr) vars in
	  PSet.union xs ys
    | LetRec (decl,expr) ->
	let xs =
	  union @@ List.map (free_variable$snd) decl in
	let vars =
	  set_of_list @@ List.map fst decl in
	let ys =
	  free_variable expr in
	  PSet.diff (PSet.union xs ys) vars
    | Var {Node.value = x} ->
	PSet.singleton x
    | Ast.Call args ->
	union @@ List.map free_variable args
    | If (cond,seq,alt) ->
	union [
	  free_variable cond;
	  free_variable seq;
	  free_variable alt;
	]
    | Block xs ->
	union @@ List.map free_variable xs
    | _ ->
	PSet.empty


let rec closure_fv =
  function
      Lambda (_,body) as exp ->
	free_variable exp
    | Ast.Call args ->
	union @@ List.map closure_fv args
    | If (a,b,c) ->
	union [
	  closure_fv a;
	  closure_fv b;
	  closure_fv c]
    | Let (decls,body) | LetRec (decls,body) ->
	let vars =
	  set_of_list @@ List.map fst decls in
	  PSet.diff (closure_fv body) vars
    | Block exprs ->
	union @@ List.map closure_fv exprs
    | _ ->
	PSet.empty

let wrap args body =
  match args with
      [] ->
	body
    | node::_ ->
	let fv =
	  PSet.to_list @@ PSet.inter (set_of_list args) (closure_fv body) in
	  if fv = [] then
	    body
	  else
	    let decls =
	      List.map (fun var -> 
			  let x = 
			    {node with Node.value = var} in
			    (x,Var x)) fv in
	      Let (decls,body)

let expr_trans =
  function
      Lambda (args,body) ->
	  Lambda (args,wrap args body)
    | e ->
	e

let stmt_trans =
  function
      Class (name,super,attrs,methods) ->
	Class (name,super,attrs,
	       List.map (fun (name,args,body) ->
			   (name,args,wrap args body)) methods)
    | stmt -> 
	lift_stmt (Ast.map expr_trans) stmt

let trans =
  List.map stmt_trans

