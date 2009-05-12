open Base
open Cpool
open Bytes
open OUnit

let test_index value =
  let cpool =
    Cpool.add Cpool.empty value in
    assert_equal 1 (Cpool.index cpool value)

let ok cpool value =
  assert_equal cpool (to_abc @@ Cpool.add Cpool.empty value)

let _ =
  ("CPool module test" >::: [
     "int" >::
       (fun () ->
	  test_index (`Int 42);
	  test_index (`Int ~-42));
     "uint" >::
       (fun () ->
	  test_index (`UInt 42));
     "string" >::
       (fun () ->
	  test_index (`String "foobar"));
     "multiname" >::
       (fun () ->
	  test_index (`QName ((`Namespace "std"),"print"));
	  test_index (`Multiname ("print",[]));
	  test_index (`Multiname ("print",[`Namespace "std"])));
     "literal cpool" >::
       (fun () ->
	  ok {Abc.empty_cpool with Abc.string=["foobar"]} (`String "foobar");
	  ok {Abc.empty_cpool with Abc.int=[30]} (`Int 30);
	  ok {Abc.empty_cpool with Abc.int=[~-30]} (`Int ~-30);
	  ok {Abc.empty_cpool with Abc.uint=[42]} (`UInt 42));
     "qname cpool" >::
       (fun () ->
	  ok
	    {Abc.empty_cpool with
	       Abc.string = ["foobar"; "std"];
	       namespace  = [{Abc.kind=0x08; namespace_name=2}];
	       multiname  = [Abc.QName (1,1)]}
	    (`QName (`Namespace "std","foobar")));
     "multiname cpool" >::
       (fun () ->
	  ok
	    {Abc.empty_cpool with
	       Abc.string   = ["std";"foobar"];
	       namespace    = [{Abc.kind=0x08; namespace_name=1}];
	       namespace_set= [[1]];
	       multiname=[Abc.Multiname (2,1)]}
	    (`Multiname ("foobar",[`Namespace "std"])));
     "cpool entry should be unique" >::
       (fun () ->
	  let cpool =
	    List.fold_left Cpool.add empty [`String "foo"; `String "bar"; `String "foo"] in
	    assert_equal 1 (Cpool.index cpool (`String "foo"));
	    assert_equal {Abc.empty_cpool with Abc.string=["foo";"bar"]} (to_abc cpool));
     "index is not change" >::
       (fun () ->
	  let cpool1 =
	    Cpool.add empty (`Int 42) in
	  let cpool2 =
	    Cpool.add cpool1 (`Int 42) in
	    assert_equal (Cpool.index cpool1 (`Int 42))  (Cpool.index cpool2 (`Int 42)))
   ]) +> run_test_tt

