open ExtList

open Base
exception NoRuleFailure
type filetype = string
type filename = string

type node =
    One of filetype
  | Many  of filetype list

type ('a,'b) cmd = 'a -> 'b -> filename -> string list

type 'a rule = {
  src : node;
  dest: filetype;
  cmd : 'a -> filename list -> filename -> string list
}
type 'a t  = 'a rule

let one_to_one src dest cmd = {
  src  = One src;
  dest = dest;
  cmd  = (fun a -> function [x] -> cmd a x | _ -> invalid_arg "")
}


let many xs =
  Many (List.unique @@ List.sort ~cmp:compare xs)

let many_to_one src dest cmd = {
  src  = many src;
  dest = dest;
  cmd  = cmd
}

let is_loop {src=src; dest=dest} =
  match src with
      One x   -> x = dest
    | Many xs -> xs = [dest]

let is_reach dest {dest=dest'} =
  match dest with
      One x ->
	x = dest'
    | Many xs ->
	xs = [dest']

let reachable dest rs =
  List.filter (is_reach dest) rs

let minimum_by f xs =
  let min a b =
    if f a b then a else b in
    match xs with
      | [] ->
	  invalid_arg "empty list"
    | y::ys ->
	List.fold_left min y ys

let remove_rule xs x =
  List.remove_if (fun {src=src; dest=dest}-> x.src=src && x.dest=dest) xs

let rec shortest rs src dest =
  match src,dest with
      One x,One y when x = y ->
	Some []
    | One x,Many ys when [x] = ys ->
	Some []
    | Many xs,Many ys when xs = ys ->
	Some []
    | One _,One _ | Many _,Many _| One _,Many _ | Many _,One _ ->
	let shortests =
	  reachable dest rs
	  +> HList.concat_map begin fun r ->
	    match shortest (remove_rule rs r) src r.src with
		None -> []
	      | Some rs -> [r::rs]
	  end in
	  if shortests = [] then
	    None
	  else
	    Some (minimum_by (fun a b -> List.length a < List.length b) shortests)

let suffix x =
  let regexp =
    Str.regexp ".*\\.\\(.*\\)$" in
    if Str.string_match regexp x 0 then
      Str.matched_group 1 x
    else
      invalid_arg "no suffix"

let tmp name s =
  Printf.sprintf "%s%s"
    (Filename.chop_suffix name (suffix name))
    s

let route rs inputs output =
  let src =
    match inputs with
	[x] ->
	  One (suffix x)
      | xs  ->
	  many (List.map suffix xs) in
  let dest =
    One (suffix output) in
    shortest rs src dest

let commands ctx rs inputs output =
  match route rs inputs output with
      None ->
	raise NoRuleFailure
    | Some r ->
	r +> List.rev +> map_accum_left
	  (fun inputs' {dest=dest; cmd=cmd} ->
	     [tmp output dest],cmd ctx inputs' @@ tmp output dest)
	  inputs +>
	  snd +>
	  List.concat

let temp_files _ rs inputs output =
  match route rs inputs output with
    | None | Some [] | Some [_] ->
	[]
    | Some (_::rs) ->
	List.map
	  (fun {dest=dest} ->
	     tmp output dest) rs
