open Base
exception NoRuleFailure
type filetype = string
type filename = string
type file = filename * filetype

type 'a cmd = 'a -> filename -> string list

type node =
    One of filetype
  | Many  of filetype list

type 'a rule = {
  src : node;
  dest: node;
  cmd : 'a -> filename list -> filename list
}
type 'a t  = 'a rule

let is_reach dest {dest=dest'} =
  match dest,dest' with
      One x, One y ->
	x = y
    | Many xs, Many ys ->
	xs = ys
    | One _ , Many _ ->
	false
    | Many xs, One y ->
	xs = [y]

let reachable dest rs =
  rs +> List.filter (is_reach dest)

let minimum_by f xs =
  let min a b =
    if f a b then a else b in
    match xs with
      | [] ->
	  invalid_arg "empty list"
    | y::ys ->
	List.fold_left min y ys

let rec route rs src dest =
  if src = dest then
    Some []
  else
    let routes =
      reachable dest rs +>
	HList.concat_map (fun r ->
			    match route rs src r.src with
				None -> []
			      | Some rs -> [r::rs]) in
      if routes = [] then
	None
      else
	Some (minimum_by (fun a b -> List.length a < List.length b) routes)

let rules = [
  {src=One ".c";dest=One ".o";cmd=fun _ _ -> []};
  {src=Many [".c"];dest=One ".s";cmd=fun _ _ -> []};
  {src=Many [".o"];dest=One ".s";cmd=fun _ _ -> []};
  {src=Many [".c";".o"];dest=One ".s";cmd=fun _ _ -> []};
  {src=One ".s";dest=One ".exe";cmd=fun _ _ -> []};
]
