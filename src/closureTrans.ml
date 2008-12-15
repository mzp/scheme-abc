open Base
open Ast

let set_of_list xs =
  List.fold_left (flip PSet.add) PSet.empty @@
    List.map Node.value xs

let union xs =
  List.fold_left PSet.union PSet.empty xs

let (--) =
  PSet.diff

let (++) =
  PSet.union

let free_variable expr =
  let branch =
    function
	`Lambda (args,expr) ->
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
      | `Int _     | `String _ | `Bool _  | `Float _ | `Var _
      | `Call _    | `If _     | `Block _ | `New _   | `Invoke _
      | `SlotRef _ | `SlotSet _ ->
	  PSet.empty in
  let leaf  =
    function
       `Var {Node.value = ("",x)} ->
	 PSet.singleton x
      | `Int _     | `String _ | `Bool _   | `Float _   | `Var _
      | `Call _    | `If _     | `Block _  | `New _     | `Invoke _
      | `LetRec _  | `Let _    | `Lambda _ | `SlotRef _ | `SlotSet _ ->
	  PSet.empty
  in
    Ast.fold branch leaf expr


let wrap args body =
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

let expr_trans =
  function
      `Lambda (args,body) ->
	`Lambda (args,wrap args body)
    | e ->
	e

let stmt_trans =
  function
      `Class (name,super,attrs,methods) ->
	`Class (name,super,attrs,
		List.map (fun (name,args,body) ->
			    (name,args,wrap args body)) methods)
    | stmt ->
	lift_stmt (Ast.map expr_trans) stmt

let trans =
  List.map stmt_trans
