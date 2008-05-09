let (@@) f g = f g
let ($) f g x = f (g x)
let id x = x

let uncurry f a b = f (a,b)
let curry f (a,b) = f a b
let flip f a b = f b a

let rec unfold f init =
     match f init with
	 Some (a, b) -> a :: unfold f b
       | None        -> []

let rec range a b =
  if a = b then
    []
  else
    a::range (a+1) b

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
