open Base
open OUnit
open Node

let string _ =
  of_string "a
b
c
"

let file _ =
  of_file "test_node.txt"
    
let rec ok xs stream =
  match xs with
      [] ->
	begin try
	  Stream.empty stream;
	  assert_bool "" true
	with Stream.Failure ->
	  assert_failure "not empty"
	end
    | y::ys ->
	OUnit.assert_equal ~printer:Std.dump y @@ Stream.next stream;
	ok ys stream

let empty = {
  value    = '-';
  filename = "<string>";
  lineno   = -1
}

let _ =
  ("node module" >:::
     ["string" >::
	(fun () ->
	   ok [{empty with value= 'a'; lineno=0};
	       {empty with value='\n'; lineno=0};
	       {empty with value= 'b'; lineno=1};
	       {empty with value='\n'; lineno=1};
	       {empty with value= 'c'; lineno=2};
	       {empty with value='\n'; lineno=2}; ] @@
	     string ());
      "file" >::
	(fun () ->
	   ok [{value= 'a'; filename="test_node.txt"; lineno=0};
	       {value='\n'; filename="test_node.txt"; lineno=0};
	       {value= 'b'; filename="test_node.txt"; lineno=1};
	       {value='\n'; filename="test_node.txt"; lineno=1};
	       {value= 'c'; filename="test_node.txt"; lineno=2};
	       {value='\n'; filename="test_node.txt"; lineno=2}; ] @@
	     file ())
     ]) +> run_test_tt
