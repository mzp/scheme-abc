open Base
open Cpool
open Util
open Bytes

test empty_append =
    assert_equal empty (append empty empty)

let test_pair get make value =
  assert_equal (U30 1) (get value @@ cmap_of_clist (make value))

test int =
  test_pair int_get int 42;
  test_pair int_get int ~-42;

test uint =
  test_pair uint_get uint 42

test string =
  test_pair string_get string "foobar"

test multiname =
  test_pair multiname_get multiname (QName ((Namespace "std"),"print"));
  test_pair multiname_get multiname (Multiname ("print",[]));
  test_pair multiname_get multiname (Multiname ("print",[Namespace "std"]));

test append =
  let cmap =
    cmap_of_clist @@ append (string "foobar") (int 42) in
    assert_equal (U30 1) (int_get 42 cmap);
    assert_equal (U30 1) (string_get "foobar" cmap)

let cpool_test cpool entry =
  let cpool1 =
    cpool_of_cmap @@ cmap_of_clist entry in
  let cpool2 =
    cpool_of_cmap @@ cmap_of_clist (append entry entry) in
    assert_equal cpool cpool1;
    assert_equal cpool cpool2;

test dummy_pack =
  let cmap = 
    cmap_of_clist (List.fold_left append empty [string "foo";string "bar";string "foo"]) in
  let cpool = 
    cpool_of_cmap @@ cmap in
    assert_equal (U30 2) (string_get "foo" cmap);
    assert_equal {Abc.empty_cpool with Abc.string=["bar";"foo"]} cpool

test cpool =
   cpool_test {Abc.empty_cpool with Abc.string=["foobar"]} (string "foobar");
   cpool_test {Abc.empty_cpool with Abc.int=[30]} (int 30);
   cpool_test {Abc.empty_cpool with Abc.int=[~-30]} (int ~-30);
   cpool_test {Abc.empty_cpool with Abc.uint=[42]} (uint 42)

test mname_cpool =
   cpool_test 
     {Abc.empty_cpool with 
	Abc.string=["foobar";"std"];
	Abc.namespace=[{Abc.kind=0x08;Abc.ns_name=2}];
	Abc.multiname=[Abc.QName (1,1)]} 
     (multiname (QName (Namespace "std","foobar")));
   cpool_test 
     {Abc.empty_cpool with 
	Abc.string=["foobar";"std"];
	Abc.namespace=[{Abc.kind=0x08;Abc.ns_name=2}];
	Abc.namespace_set=[[1]];
	Abc.multiname=[Abc.Multiname (1,1)]} 
     (multiname (Multiname ("foobar",[Namespace "std"])));
