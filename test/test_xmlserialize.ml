open Base
open Xml
open XmlSerialize

let assert_equal lhs rhs =
  OUnit.assert_equal ~printer:Xml.to_string_fmt lhs rhs;;

let u30 n =
  Element ("U30",["value",n],[])

let u30i =
  u30 $ string_of_int

let string2 n =
  Element ("String2",["value",n],[])

test empty_cpool =
    assert_equal
      (Element ("Constants",[],[
		  Element ("ints", [],[]);
		  Element ("uints",[],[]);
		  Element ("doubles",[],[]);
		  Element ("strings",[],[]);
		  Element ("namespaces",[],[]);
		  Element ("namespaceSets",[],[]);
		  Element ("multinames",[],[])])) @@
      of_cpool Abc.empty_cpool

test int =
  assert_equal
      (Element ("Constants",[],[
		  Element ("ints", [],[u30i 1;u30i 2;u30i 3]);
		  Element ("uints",[],[]);
		  Element ("doubles",[],[]);
		  Element ("strings",[],[]);
		  Element ("namespaces",[],[]);
		  Element ("namespaceSets",[],[]);
		  Element ("multinames",[],[])])) @@
    of_cpool {Abc.empty_cpool  with Abc.int = [1;2;3] }

test uint =
    assert_equal
      (Element ("Constants",[],[
		  Element ("ints", [],[]);
		  Element ("uints",[],[u30i 4;u30i 5;u30i 6]);
		  Element ("doubles",[],[]);
		  Element ("strings",[],[]);
		  Element ("namespaces",[],[]);
		  Element ("namespaceSets",[],[]);
		  Element ("multinames",[],[])]))
      (of_cpool { Abc.empty_cpool with Abc.uint = [4;5;6] })

test doubles =
    assert_equal
      (Element ("Constants",[],[
		  Element ("ints", [],[]);
		  Element ("uints",[],[]);
		  Element ("doubles",[],List.map (u30 $ string_of_float) [7.;8.;9.]);
		  Element ("strings",[],[]);
		  Element ("namespaces",[],[]);
		  Element ("namespaceSets",[],[]);
		  Element ("multinames",[],[]);])) @@
      of_cpool {
	Abc.empty_cpool with
	  Abc.double = [7.;8.;9.];
      }

test strings = 
    assert_equal
      (Element ("Constants",[],[
		  Element ("ints", [],[]);
		  Element ("uints",[],[]);
		  Element ("doubles",[],[]);
		  Element ("strings",[],List.map string2 ["foo";"bar";"baz"]);
		  Element ("namespaces",[],[]);
		  Element ("namespaceSets",[],[]);
		  Element ("multinames",[],[]);])) @@
      of_cpool {
	Abc.empty_cpool with Abc.string = ["foo";"bar";"baz"];
      }


test namespace_cpool =
    assert_equal
      (Element ("Constants",[],[
		  Element ("ints", [],[]);
		  Element ("uints",[],[]);
		  Element ("doubles",[],[]);
		  Element ("strings",[],[]);
		  Element ("namespaces",[],List.map 
			     (fun i->Element ("Namespace",["index",string_of_int i],[])) [1;2]);
		  Element ("namespaceSets",[],[]);
		  Element ("multinames",[],[])])) @@
      of_cpool {Abc.empty_cpool with
		  Abc.namespace = [{Abc.kind=0;ns_name=1};{Abc.kind=0;ns_name=2}]}

test namespace_set_cpool =
    assert_equal
      (Element ("Constants",[],[
		  Element ("ints", [],[]);
		  Element ("uints",[],[]);
		  Element ("doubles",[],[]);
		  Element ("strings",[],[]);
		  Element ("namespaces",[],[]);
		  Element ("namespaceSets",[],
			   [Element ("NamespaceSet",[],[u30i 1;u30i 2;u30i 3])]);
		  Element ("multinames",[],[])])) @@
      of_cpool {Abc.empty_cpool with
		  Abc.namespace_set = [[1;2;3]]}

test multiname_cpool =
    assert_equal
      (Element ("Constants",[],[
		  Element ("ints", [],[]);
		  Element ("uints",[],[]);
		  Element ("doubles",[],[]);
		  Element ("strings",[],[]);
		  Element ("namespaces",[],[]);
		  Element ("namespaceSets",[],[]);
		  Element ("multinames",[],[
			     Element ("QName",["namespaceIndex","1";"nameIndex","2"],[]);
			     Element ("Multiname",["nameIndex","3";"namespaceSetIndex","4"],[]);
			   ]);])) @@
      of_cpool {Abc.empty_cpool with
		  Abc.multiname = [Abc.QName (1,2);Abc.Multiname (3,4)]}

(*
test int =
    assert_equal
      (Element ("Constants",[],[
		  Element ("ints", [],[u30i 1;u30i 2;u30i 3]);
		  Element ("uints",[],[u30i 4;u30i 5;u30i 6]);
		  Element ("doubles",[],List.map (u30 $ string_of_float) [7.;8.;9.]);
		  Element ("strings",[],List.map string2 ["foo";"bar";"baz"]);
		  Element ("namespaces",[],List.map 
			     (fun i->Element ("Namespace",["index",string_of_int i],[])) [1;2]);o
		  Element ("namespaceSets",[],
			   [Element ("namespaceSet",[],[u30i 1;u30i 2;u30i 3])]);
		  Element ("multinames",[],[
			     Element ("QName",["namespaceIndex","1";"nameIndex","2"],[]);
			     Element ("Multiname",["nameIndex","3";"namespaceSetIndex","4"],[]);
			   ]);])) @@
      of_cpool {
	Abc.int    = [1;2;3];
	Abc.uint   = [4;5;6];
	Abc.double = [7.;8.;9.];
	Abc.string = ["foo";"bar";"baz"];
	Abc.namespace = [{Abc.kind=0;ns_name=1};{Abc.kind=0;ns_name=2}];
	Abc.namespace_set = [[1;2;3]];
	Abc.multiname = [Abc.QName (1,2);Abc.Multiname (3,4)]
      }

*)
