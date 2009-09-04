(**
Index immutable Set.

If you add some elements to a set, [index] is not change.
*)
open Base

type 'a t = 'a list

let empty =
  []

let add x xs =
  x::xs

let add_list xs ys =
  List.fold_left (flip add) ys xs

let rec index x =
  function
      [] ->
	raise Not_found
    | y::ys ->
	if x = y then
	  List.length ys
	else
	  index x ys

let to_list xs =
  List.rev xs

let mem x xs =
  List.mem x xs
