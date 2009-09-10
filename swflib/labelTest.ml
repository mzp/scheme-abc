open Base
open OUnit
open Label

let _ =
  ("label.ml" >::: [
     "unique" >::
       (fun () ->
	  let a =
	    Label.make () in
	  let b =
	    Label.make() in
	    assert_equal false (a=b));
     "peek" >::
       (fun () ->
	  let a =
	    Label.peek 0 in
	  let b =
	    Label.make() in
	    assert_equal true (a=b));
   ]) +> run_test_tt_main
