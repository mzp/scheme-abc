open Base
type 'a t = 'a list
type 'a map = ('a*int) list

let empty = 
  []

let append =
  (@)

let add value tbl = 
  value::tbl

let uniq_by f tbl =
  ExtList.List.unique @@ List.sort f tbl

let uniq tbl = 
  uniq_by compare tbl

let of_list lst =
  lst

let to_list tbl =
  tbl

let to_map tbl =
  ExtList.List.mapi (fun i x-> x,i) tbl

let get =
  List.assoc
