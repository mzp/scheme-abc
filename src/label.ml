type t = {count:int}

let count =
  ref 0

let make () = 
  count := !count+1;
  {count= !count}

let peek n =
  {count= !count+1+n}
