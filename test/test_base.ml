open Base
open OUnit
open Util

let _ =
  ("base module test" >::: [
     "range" >::
       (fun () -> 
	  ok [1;2;3] @@ range 1 4;
	  ok [] @@ range 1 1;
	  ok [1] @@ range 1 2;
	  ok [] @@ range 1 0);
     "unfold" >::
       (fun () ->
	  let f x = 
	    if x <= 0 then
	      None
	    else
	      Some (x,x-1)
	  in
	    ok [3;2;1] @@ unfold f 3;
	    ok [] @@ unfold f 0;
	    ok [1] @@ unfold f 1);
     "map_accum_left" >::
       (fun () ->
	  ok (0, []) @@ 
	    map_accum_left (fun i x->(i+1,x+i)) 0 [];
	  ok (4, [1; 2; 3; 4]) @@ 
	    map_accum_left (fun i x->(i+1,x+i)) 0 [1;1;1;1]);
     "group_by" >::
       (fun () ->
	  ok [[1];[2;2];[3]] @@ group_by (==) [1;2;2;3])
   ]) +> run_test_tt
