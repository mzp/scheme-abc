let (@@) f g = f g
let ($) f g x = f (g x)
let id x = x

let uncurry f a b = f (a,b)
let curry f (a,b) = f a b
let flip f a b = f b a
let const a b = a

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

type ('a,'b) either = Left of 'a | Right of 'b
let left = 
  function 
      Left a -> a
    | _ -> invalid_arg "left"
let right =
  function
      Right a -> a
    | _ -> invalid_arg "right"

let concatMap f xs = 
  List.concat @@ List.map f xs

let concat_map f xs = 
  List.concat @@ List.map f xs

let map_accum_left f init xs = 
  let f (accum,ys) x =
    let accum',y = 
      f accum x in
      (accum',y::ys) in
  let accum,ys = 
    List.fold_left f (init,[]) xs in
    accum,List.rev ys
