open OUnit
open Base

let ok x y =
  assert_equal x @@ PSet.to_list y

let _ =
  ("Poly Set module" >::: [
     "empty" >::
       (fun () ->
	  ok []  @@ PSet.empty);
     "singleton" >::
       (fun () ->
	  ok [1]     @@ PSet.singleton 1;
	  ok ["foo"] @@ PSet.singleton "foo");
     "add" >::
       (fun () ->
	  ok [1]     @@ PSet.add 1 PSet.empty);
     "union" >::
       (fun () ->
	  ok [1;2;3] @@
	    PSet.union (PSet.of_list [1;2]) (PSet.of_list [2;3]));
     "inter" >::
       (fun () ->
	  ok [2] @@
	    PSet.inter (PSet.of_list [1;2]) (PSet.of_list [2;3]));
     "diff" >::
       (fun () ->
	  ok [1;2] @@ PSet.diff (PSet.of_list [1;2;3;4]) (PSet.of_list [3;4;5]));
     "to_list should sorted" >::
       (fun () ->
	  ok [1;2;3] @@ PSet.of_list [3;2;1]);
     "mem" >::
       (fun () ->
	  assert_equal true @@ PSet.mem 1 @@ PSet.of_list [3;2;1];
	  assert_equal false @@ PSet.mem 100 @@ PSet.of_list [3;2;1])
   ]) +> run_test_tt_main
