open Base
exception NoRuleFailure
type filetype = string
type filename = string

type node =
    One of filetype
  | Many  of filetype list
type ('a,'b) cmd =
    'a -> 'b -> filename -> string list
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

let many_to_one src dest cmd = {
  src  = Many src;
  dest = dest;
  cmd  = cmd
}

let is_reach dest {dest=dest'} =
  match dest with
      One x ->
	x = dest'
    | Many xs ->
	xs = [dest']

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
  match src,dest with
      One x,One y when x = y ->
	Some []
    | One x,Many ys when [x] = ys ->
	Some []
    | One _,One _ | Many _,Many _| One _,Many _ | Many _,One _ ->
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
  {src=One ".c";dest=".o";cmd=fun _ _ _ -> []};
  {src=Many [".c"];dest=".s";cmd=fun _ _ _ -> []};
  {src=Many [".o"];dest=".s";cmd=fun _ _ _-> []};
  {src=Many [".c";".o"];dest=".s";cmd=fun _ _ _-> []};
  {src=One ".s";dest=".exe";cmd=fun _ _ _-> []};
]

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

let commands ctx rs inputs output =
  let src =
    match inputs with
	[x] ->
	  One (suffix x)
      | xs  ->
	  Many (xs +> List.map suffix +> List.sort compare +>
	    ExtList.List.unique) in
  let dest =
    One (suffix output) in
    match route rs src dest with
	None ->
	  raise NoRuleFailure
      | Some r ->
	  r +> List.rev +> map_accum_left
	    (fun inputs' {dest=dest; cmd=cmd} ->
	       [tmp output dest],cmd ctx inputs' @@ tmp output dest)
	    inputs +>
	    snd +>
	    List.concat

let rules = [
  one_to_one ".scm" ".ho"
    (fun _ _ _ -> [])
]
