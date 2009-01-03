open Base
open OUnit
open Util

let inc x = 
  x + 1

let _ =
  ("Tuple" >:::
     ["T2" >::: [
	"map1" >:: 
	  (fun () ->
	     ok (2,1) @@ Tuple.T2.map1 inc (1,1));
	"map2" >::
	  (fun () ->
	     ok (1,2) @@ Tuple.T2.map2 inc (1,1));
      ];
      "T3" >::: [
	"map1" >::
	  (fun () ->
	     ok (2,1,1) @@ Tuple.T3.map1 inc (1,1,1));
	"map2" >::
	  (fun () ->
	     ok (1,2,1) @@ Tuple.T3.map2 inc (1,1,1));
	"map3" >::
	  (fun () ->
	     ok (1,1,2) @@ Tuple.T3.map3 inc (1,1,1));
      ]
     ]) +> run_test_tt
    
