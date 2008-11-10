open OUnit
open Util
open Base

let _ =
  ("Poly Set module" >::: [
     "empty" >::
       (fun () ->
	  ok []  @@ PSet.to_list PSet.empty);
     "singleton" >::
       (fun () ->
	  ok [1]     @@ PSet.to_list @@ PSet.singleton 1;
	  ok ["foo"] @@ PSet.to_list @@ PSet.singleton "foo");
     "union" >::
       (fun () ->
	  ok [1;2;3] @@ PSet.to_list @@ PSet.union (PSet.from_list [1;2]) (PSet.from_list [2;3]));
     "diff" >::
       (fun () ->
	  ok [1;2] @@ PSet.to_list @@ PSet.diff (PSet.from_list [1;2;3;4]) (PSet.from_list [3;4;5]));
     "to_list should sorted" >::
       (fun () ->
	  ok [1;2;3] @@ PSet.to_list @@ PSet.from_list [3;2;1])
   ]) +> run_test_tt
