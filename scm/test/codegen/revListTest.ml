open Base
open RevList
open OUnit

let _ =
  ("ISet" >::: [
     "index is immutable" >::
       (fun () ->
	  let set1 =
	    RevList.add 0 empty in
	  let set2 =
	    RevList.add 1 set1 in
	      assert_equal (index 0 set1) (RevList.index 0 set2));
     "mem" >::
       (fun () ->
	  assert_equal false (RevList.mem 0 empty);
	  assert_equal true  (RevList.mem 0 (RevList.add 0 empty)));
     "index" >::
       (fun () ->
	  let set =
	    RevList.add 42 empty in
	    assert_equal 0 (RevList.index 42 set));
     "to_list" >::
       (fun () ->
	  let set1 =
	    RevList.add 42 empty in
	  let set2 =
	    RevList.add 0 set1 in
	    assert_equal [42;0] (RevList.to_list set2));
     "add_list" >::
       (fun () ->
	  let set1 =
	    RevList.add_list [1;2;3] empty in
	  let set2 =
	    RevList.add 3 (RevList.add 2 (RevList.add 1 empty)) in
	    assert_equal set1 set2)
   ]) +> run_test_tt
