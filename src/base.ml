let (@@) f g = f g
let (+>) f g = g f
let ($) f g x = f (g x)
let id x = x
let (!$) = Lazy.force


let uncurry f a b = f (a,b)
let curry f (a,b) = f a b
let flip f a b = f b a
let const a _ = a

let maybe f x = try Some (f x) with _ -> None
let tee f x = try ignore @@ f x; x with _ -> x

let string_of_list xs =
  Printf.sprintf "[%s]"
    @@ String.concat ";" xs

let rec unfold f init =
     match f init with
	 Some (a, b) -> a :: unfold f b
       | None        -> []

let rec range a b =
  if a >= b then
    []
  else
    a::range (a+1) b

let rec interperse delim =
  function
      []  -> []
    | [x] -> [x]
    | x::xs -> x::delim::interperse delim xs

let map_accum_left f init xs = 
  let f (accum,ys) x =
    let accum',y = 
      f accum x in
      (accum',y::ys) in
  let accum,ys = 
    List.fold_left f (init,[]) xs in
    accum,List.rev ys

let rec group_by f =
  function
      [] ->
	[]
    | x1::x2::xs when f x1 x2 ->
	begin match group_by f @@ x2::xs with
	    y::ys ->
	      (x1::y)::ys
	  | _ ->
	      failwith "must not happen"
	end
    | x::xs ->
	[x]::group_by f xs
  
let index x xs = 
  let rec loop i = 
    function
	[] -> 
	  raise Not_found
      | y::ys -> 
	  if x = y then
	    i
	  else
	    loop (i+1) ys in
    loop 0 xs
