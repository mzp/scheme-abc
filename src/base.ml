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

let unzip xs =
  List.fold_right (fun (x,y) (xs,ys)->(x::xs),(y::ys)) xs ([],[])

let uncurry f a b = f (a,b)
let curry f (a,b) = f a b
let flip f a b = f b a

let rec range a b =
  if a = b then
    []
  else
    a::range (a+1) b

let mapi f xs =
  let rec inner i =
    function 
	[] -> []
      | y::ys -> (f y i)::inner (i+1) ys
  in
    inner 0 xs

let foldmap_left f init =
  let g (ctx,xs) x =
    let ctx',x' = 
      f ctx x in
      (ctx',x'::xs) in
  List.fold_left g (init,[]);;      

let explode str = 
  List.map (fun i-> String.get str i) @@ range 0 @@ String.length str

let implode xs =
  let length =
    List.length xs in
  let str = 
    String.make length ' ' in
    ignore @@ mapi (fun c i->str.[i] <- c) xs;
    str

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
