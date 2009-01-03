open OUnit
let table =
  Hashtbl.create 10

let safe_find t key =
  try
    Hashtbl.find t key
  with Not_found ->
    []

let add fname label fn = 
  let new_test = 
    label >:: fn in
  Hashtbl.replace table fname (new_test::safe_find table fname)
  
let keys t =
  Hashtbl.fold (fun key _ xs -> key::xs) t []

let all_tests () =
  let fnames =
    List.sort compare (keys table) in
  let tests =
    List.map (fun fname->  fname  >::: List.rev (Hashtbl.find table fname)) fnames in
    TestList tests
  
let run_test () =
  run_test_tt_main (all_tests ())
