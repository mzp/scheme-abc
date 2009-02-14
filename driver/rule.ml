open Base
exception NoRuleFailure

type t = {
  src : string;
  dest: string;
  cmd : string -> string -> string list
}

let (=>) a b =
  (a,b)
let (<>) (a,b) f = {
  src  = a;
  dest = b;
  cmd  = f
}

let is_reach dest {dest=dest'} =
  dest = dest'

let reachable dest rules =
  List.filter (is_reach dest) rules

let minimum_by f xs =
  let min a b =
    if f a b then a else b in
    match xs with
      | [] ->
	  invalid_arg "empty list"
    | y::ys ->
	List.fold_left min y ys

let rec shortest rules src dest =
  if src = dest then
    0,[]
  else
    let reachable =
      List.map
	(fun ({src=src'} as r) ->
	   let (cost,routes) =
	     shortest rules src src' in
	     cost+1,r::routes) @@
	reachable dest rules in
      minimum_by (fun (a,_) (b,_) -> a < b) reachable
