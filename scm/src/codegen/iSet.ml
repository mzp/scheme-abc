(**
Index immutable Set.

If you add some elements to a set, [index] is not change.
*)
open Base

type 'a t = 'a list

let empty =
  []

let add set x=
  if List.mem x set then
    set
  else
    x::set

let add_list set xs =
  List.fold_left add set xs

let rec index xs x =
  match xs with
      [] ->
	raise Not_found
    | y::ys ->
	if x = y then
	  List.length ys
	else
	  index ys x

let to_list xs =
  List.rev xs
