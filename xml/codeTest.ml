open Base
open OUnit
open EasyXml

let ok x y =
  OUnit.assert_equal ~printer:Xml.to_string_fmt (normalize x) (normalize y)

let _ = 
  ("code module test" >::: [
     "add" >::
       (fun _ ->
	  ok (elem "OpAdd" []) @@
	    Code.to_xml `Add);
     "constructprop" >::
       (fun _ ->
	  ok (attr "OpConstructProp" ["name","1";"argc","42"]) @@
	    Code.to_xml (`ConstructProp (1l,42l)));
     "getlex" >::
       (fun _ ->
	  ok (attr "OpGetLex" ["name","42"]) @@
	    Code.to_xml (`GetLex 42l))
   ]) +> run_test_tt_main
