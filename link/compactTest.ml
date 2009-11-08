open Base
open OUnit
open Swflib.AbcType

open EmptyAbc
open Compact

let ok x y =
  assert_equal x (compact y)

let _ = begin "uniq.ml" >::: [
  "id" >:: begin fun () ->
    ok abc abc
  end;
  "cpool" >:: begin fun () ->
    ok
      {abc with cpool =
	  {
	     int = [1; 2];
	     uint          = [1;2;1];
	     double        = [1.0;2.];
	     string        = ["foo";"bar"];
	     namespace     = [Namespace 1];
	     namespace_set = [[1]];
	     multiname     = [QName(1,1)];
	  } }
      {abc with cpool =
	  {
	     int =[1;2;1;];
	     uint          = [1;2;1];
	     double        = [1.0;2.0;1.0];
	     string        = ["foo";"bar";"foo"];
	     namespace     = [Namespace 1; Namespace 3];
	     namespace_set = [[1];[2]];
	     multiname     = [QName(1,1); QName(2,3)];
	  } }
  end;
  "multiname" >:: begin fun () ->
    ok
      {abc with
	 cpool = {cpool with
		    string        = ["foo";"bar"];
		    namespace     = [Namespace 1];
		    multiname     = [QName(1,1)];
		 };
	 method_info = [{info with method_name=0}] }
      {abc with
	 cpool = {cpool with
		    string        = ["foo";"bar"];
		    namespace     = [Namespace 1];
		    multiname     = [QName(1,1); QName(1,1)];
		 };
	 method_info = [{info with method_name=1}] }
  end;
] end +> run_test_tt_main

