open Base
open ISet
open OUnit

let _ =
  ("ISet" >::: [
     "index is immutable" >::
       (fun () ->
	  let set1 =
	    ISet.add 0 empty in
	  let set2 =
	    ISet.add 1 set1 in
	      assert_equal (index 0 set1) (ISet.index 0 set2));
     "index is immutable for same number" >::
       (fun () ->
	  let set1 =
	    ISet.add 0 empty in
	  let set2 =
	    ISet.add 0 set1 in
	      assert_equal (ISet.index 0 set1) (ISet.index 0 set2));
     "index" >::
       (fun () ->
	  let set =
	    ISet.add 42 empty in
	    assert_equal 0 (ISet.index 42 set));
     "to_list" >::
       (fun () ->
	  let set1 =
	    ISet.add 42 empty in
	  let set2 =
	    ISet.add 0 set1 in
	    assert_equal [42;0] (ISet.to_list set2));
     "add_list" >::
       (fun () ->
	  let set1 =
	    ISet.add_list [1;2;3] empty in
	  let set2 =
	    ISet.add 3 (ISet.add 2 (ISet.add 1 empty)) in
	    assert_equal set1 set2)
   ]) +> run_test_tt

