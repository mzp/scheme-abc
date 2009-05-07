type t = int

let count =
  ref 0

let make () =
  count := !count+1;
  !count

let peek n =
  !count+1+n

let to_string n =
  Printf.sprintf "$%d" n
