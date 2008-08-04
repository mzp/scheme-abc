open Base

let rec last =
  function
      [] ->
	invalid_arg "HList.last"
    | [x] -> 
	x
    | x::xs -> 
	last xs

let init xs =
  let rec init' ys =
    function
	[]  ->
	  invalid_arg "HList.init"
      |	[_] ->
	  List.rev ys
      | x::xs ->
	  init' (x::ys) xs in
    init' [] xs

let null =
  function
      [] -> 
	true
    | _ -> 
	false

let fold_left1 f (x::xs) =
  List.fold_left f x xs

let rec fold_right1 f =
  function
      []    ->
	invalid_arg "HList.fold_right1"
    | [x]   ->
	x
    | x::xs ->
	f x (fold_right1 f xs)



let conj =
  List.fold_left (&&) true

let disj =
  List.fold_left (||) false

let sum =
  List.fold_left (+) 0

let product =
  List.fold_left ( * ) 1

let concat_map f xs =
  List.fold_right ((@) $ f) xs []
