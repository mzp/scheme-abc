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

let fold_left1 f =
  function
      [] ->
	invalid_arg "HList.fold_left1"
    | x::xs ->
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

let maximum xs =
  fold_left1 max xs

let minimum xs =
  fold_left1 min xs

let rec scanl f y =
  function
      [] ->
	[y]
    | x::xs ->
	y::scanl f (f y x) xs

let scanl1 f =
  function
      [] ->
	[]
    | x::xs ->
	scanl f x xs

let rec scanr f z =
  function
      [] ->
	[z]
    | x::xs ->
	let y::_ as yss = 
	  scanr f z xs in
	  (f x y) :: yss

let scanr1 f =
  function
    [] -> 
      []
  | x::xs -> 
      scanr f x xs

let replicate n x =
  let rec loop i ys = 
    if i = 0 then
      ys
    else
      loop (i-1) (x::ys) in
    loop n []
    
let rec take n =
  function
      [] ->
	[]
    | x::xs ->
	if n <= 0 then
	  []
	else
	  x :: take (n - 1) xs

let rec drop n =
  function
      [] ->
	[]
    | xs when n <= 0 -> 
	xs
    | x::xs -> 
      drop (n-1) xs






