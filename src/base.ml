let (@@) f g = f g
let ($) f g x = f (g x)
let id x = x

let rec unfold f init =
     match f init with
	 Some (a, b) -> a :: unfold f b
       | None        -> []

let rec uniq = 
  function
      [] -> []
    | [x] -> [x]
    | x::(y::xs as xss) ->
	if x = y then
	  uniq xss
	else
	  x::(uniq xss)

let uncurry f a b = f (a,b)
let curry f (a,b) = f a b
let flip f a b = f b a

let rec range a b =
  if a = b then
    []
  else
    a::range (a+1) b

let foldmap_left f init =
  let g (ctx,xs) x =
    let ctx',x' = 
      f ctx x in
      (ctx',x'::xs) in
  List.fold_left g (init,[]);;      

type ('a,'b) either = Left of 'a | Right of 'b
let left = 
  function 
      Left a -> a
    | _ -> failwith "invalid argument: left"
let right =
  function
      Right a -> a
    | _ -> failwith "invalid argument: right"

let concatMap f xs = 
  List.concat @@ List.map f xs
