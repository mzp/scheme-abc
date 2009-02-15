open Base
exception NoRuleFailure

type 'a t = {
  src : string;
  dest: string;
  cmd : 'a -> string -> string -> string list
}

let (=>) a b =
  (a,b)

let ($$) (a,b) f = {
  src  = a;
  dest = b;
  cmd  = f
}

let reachable dest rules =
  List.filter (fun {dest=dest'} -> dest = dest') rules

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

let suffix filename =
  let regexp =
    Str.regexp ".*\\.\\(.*\\)$" in
    if Str.string_match regexp filename 0 then
      Some (Str.matched_group 1 filename)
    else
      None

let commands rules opt src dest =
  let src' =
    match suffix src with
	Some s -> s
      | None   -> "scm" in
  let dest' =
    match suffix dest with
	Some s -> s
      | None   -> "swf" in
  let routes =
    snd @@ shortest rules src' dest' in
    routes +>
      map_accum_left
      (fun input {src=src; dest=dest; cmd=cmd} ->
	 let output =
	   Printf.sprintf "%s.%s" (Filename.chop_suffix input src) dest in
	   output,cmd opt input output) src +>
      snd +> List.concat



