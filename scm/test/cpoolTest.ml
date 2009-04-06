open Base
open Cpool
open Util
open Bytes
open OUnit

let test_pair get make value =
  ok (u30 1) (get value @@ (make value))

let ok_cp cpool entry =
  let cpool1 =
    entry in
  let cpool2 =
    append entry entry in
    ok cpool (to_abc cpool1);
    ok cpool (to_abc cpool2)

let _ =
  ("CPool module test" >::: [
     "empty + empty" >::
       (fun () ->
	  ok empty (append empty empty));
     "int" >::
       (fun () -> 
	  test_pair int_get int 42;
	  test_pair int_get int ~-42);
     "uint" >::
       (fun () ->
	  test_pair uint_get uint 42);
     "string" >::
       (fun () ->
	  test_pair string_get string "foobar");
     "multiname" >::
       (fun () ->
	  test_pair multiname_get multiname (QName ((Namespace "std"),"print"));
	  test_pair multiname_get multiname (Multiname ("print",[]));
	  test_pair multiname_get multiname (Multiname ("print",[Namespace "std"])));
     "append" >::
       (fun () ->
	  let cpool =
	    append (string "foobar") (int 42) in
	    ok (u30 1) (int_get 42 cpool);
	    ok (u30 1) (string_get "foobar" cpool));
     "cpool" >::
       (fun () ->
	  ok_cp {Abc.empty_cpool with Abc.string=["foobar"]} (string "foobar");
	  ok_cp {Abc.empty_cpool with Abc.int=[30]} (int 30);
	  ok_cp {Abc.empty_cpool with Abc.int=[~-30]} (int ~-30);
	  ok_cp {Abc.empty_cpool with Abc.uint=[42]} (uint 42));
     "no dup" >::
       (fun () ->
	  let cpool = 
	    List.fold_left append empty [string "foo";string "bar";string "foo"] in
	    ok 2 (string_nget "foo" cpool);
	    ok {Abc.empty_cpool with Abc.string=["bar";"foo"]} (to_abc cpool));
     "multiname get" >::
       (fun () ->
	  ok_cp
	    {Abc.empty_cpool with 
	       Abc.string = ["foobar";"std"];
	       namespace  = [{Abc.kind=0x08;Abc.ns_name=2}];
	       multiname  = [Abc.QName (1,1)]} 
	    (multiname (QName (Namespace "std","foobar")));
	  ok_cp
	    {Abc.empty_cpool with 
	       Abc.string   = ["foobar";"std"];
	       namespace    = [{Abc.kind=0x08;Abc.ns_name=2}];
	       namespace_set= [[1]];
	       multiname=[Abc.Multiname (1,1)]} 
	    (multiname (Multiname ("foobar",[Namespace "std"]))))
   ]) +> run_test_tt
