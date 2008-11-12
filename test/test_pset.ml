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
     "add" >::
       (fun () ->
	  ok [1]     @@ PSet.to_list @@ PSet.add 1 PSet.empty);
     "union" >::
       (fun () ->
	  ok [1;2;3] @@ PSet.to_list @@ 
	    PSet.union (PSet.of_list [1;2]) (PSet.of_list [2;3]));
     "inter" >::
       (fun () ->
	  ok [2] @@ PSet.to_list @@ 
	    PSet.inter (PSet.of_list [1;2]) (PSet.of_list [2;3]));
     "diff" >::
       (fun () ->
	  ok [1;2] @@ PSet.to_list @@ PSet.diff (PSet.of_list [1;2;3;4]) (PSet.of_list [3;4;5]));
     "to_list should sorted" >::
       (fun () ->
	  ok [1;2;3] @@ PSet.to_list @@ PSet.of_list [3;2;1]);
     "mem" >::
       (fun () ->
	  ok true @@ PSet.mem 1 @@ PSet.of_list [3;2;1];
	  ok false @@ PSet.mem 100 @@ PSet.of_list [3;2;1])
   ]) +> run_test_tt
