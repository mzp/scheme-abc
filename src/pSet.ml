open Base
type 'a t = 'a list

let empty =
  []

let singleton x =
  [x]

let rec add x = function
    [] ->
      [x]
  | y::_ as xs when x = y ->
      xs
  | y::_ as ys when x < y ->
      x::ys
  | y::ys ->
      y::add x ys

let rec remove x = function
    y::ys when x = y ->
      ys
  | y::ys ->
      y::remove x ys
  | [] ->
      []

let to_list x =
  x

let of_list x =
  ExtList.List.unique @@ List.sort compare x

let rec mem x = function
    y::_ when x = y ->
      true
  | _::ys ->
      mem x ys
  | [] ->
      false

let union x y =
  List.fold_left (flip add) y x

let diff x y =
  List.fold_left (flip remove) x y

let inter xs ys =
  List.filter (fun x -> mem x ys) xs
