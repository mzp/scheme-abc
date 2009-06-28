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

let empty =
  {(Node.ghost '-') with filename = "<string>"}

let _ =
  ("node module" >:::
     ["string" >::
	(fun () ->
	   ok [{empty with value= 'a'; lineno=0; start_pos=0; end_pos=1};
	       {empty with value='\n'; lineno=0; start_pos=1; end_pos=2};
	       {empty with value= 'b'; lineno=1; start_pos=0; end_pos=1};
	       {empty with value='\n'; lineno=1; start_pos=1; end_pos=2};
	       {empty with value= 'c'; lineno=2; start_pos=0; end_pos=1};
	       {empty with value='\n'; lineno=2; start_pos=1; end_pos=2}; ] @@
	     string ());
      "file" >::
	(fun () ->
	   ok [{value= 'a'; filename="test_node.txt"; lineno=0; start_pos=0; end_pos=1};
	       {value='\n'; filename="test_node.txt"; lineno=0; start_pos=1; end_pos=2};
	       {value= 'b'; filename="test_node.txt"; lineno=1; start_pos=0; end_pos=1};
	       {value='\n'; filename="test_node.txt"; lineno=1; start_pos=1; end_pos=2};
	       {value= 'c'; filename="test_node.txt"; lineno=2; start_pos=0; end_pos=1};
	       {value='\n'; filename="test_node.txt"; lineno=2; start_pos=1; end_pos=2};] @@
	     file ())
     ]) +> run_test_tt
