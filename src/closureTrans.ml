open Base
open Ast

module Set = Core.Std.Set
type 'a set = 'a Set.t

let set_of_list xs =
  List.fold_left (flip Set.add) Set.empty xs

let union xs =
  List.fold_left Set.union Set.empty xs

let rec free_variable =
  function
      Lambda (args,expr) ->
	Set.diff (free_variable expr) (set_of_list args)
    | Let (decl,expr) ->
	let xs = 
	  union @@ List.map (free_variable$snd) decl in
	let vars =
	  set_of_list @@ List.map fst decl in
	let ys =
	  Set.diff (free_variable expr) vars in
	  Set.union xs ys
    | LetRec (decl,expr) ->
	let xs =
	  union @@ List.map (free_variable$snd) decl in
	let vars =
	  set_of_list @@ List.map fst decl in
	let ys =
	  free_variable expr in
	  Set.diff (Set.union xs ys) vars
    | Var x ->
	Set.singleton x
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
	Set.empty

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
	  Set.diff (closure_fv body) vars
    | Block exprs ->
	union @@ List.map closure_fv exprs
    | _ ->
	Set.empty

let wrap_closure =
  function
      Lambda (args,body) ->
	let fv =
	  Set.elements @@ Set.inter (set_of_list args) (closure_fv body) in
	let body' =
	  if fv = [] then
	    body
	  else
	    Let (List.map (fun x->x,Var x) fv,body) in
	  Lambda (args,body')
    | e ->
	e

let trans =
  lift_program wrap_closure

