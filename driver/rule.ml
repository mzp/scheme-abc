open Base
exception NoRuleFailure
type filetype = string
type filename = string
type file = filename * filetype

type 'a cmd = 'a -> filename -> string list

type 'a rule =
    Single of filetype * filetype * filename cmd
  | Multi  of filetype list * filetype * filename list cmd
type 'a t  = 'a rule

let single src dest cmd =
  Single (src,dest,cmd)
let multi src_list dest cmd =
  Multi (List.sort compare src_list,dest,cmd)

let reachable dest rules =
  rules +>
    List.filter
    (function
	 Single (_,dest',_) ->
	   dest = dest'
       | Multi (_,dest',_) ->
	   dest = dest')

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
    0,[]
(*
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

let tmp name suffix =
  Printf.sprintf "%s.%s" name suffix

let commands rules opt (iname,itype) (oname,otype) =
  let routes =
    snd @@ shortest rules itype otype in
    routes +>
      map_accum_left  (fun input {dest=dest; cmd=cmd} ->
			 let oname' =
			   if dest = otype then
			     oname
			   else
			     tmp oname dest in
			   oname',cmd opt input oname')
      iname +>
      snd +> List.concat



*)
