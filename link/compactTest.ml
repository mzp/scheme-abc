open Base
open OUnit
open Swflib.AbcType

open EmptyAbc
open Compact

let ok ?msg x y =
  assert_equal ?msg ~printer:Std.dump x (compact y)

let cp xs = {abc with cpool=xs}

let _ = begin "uniq.ml" >::: [
  "id" >:: begin fun () ->
    ok abc abc
  end;
  "cpool" >:: begin fun () ->
    ok ~msg:"int"    (cp {cpool with int   =[2;1]}) (cp {cpool with int=[1;2;1]});
    ok ~msg:"uint"   (cp {cpool with uint  =[2;1]}) (cp {cpool with uint=[1;2;1]});
    ok ~msg:"double" (cp {cpool with double=[2.;1.]}) (cp {cpool with double=[1.;2.;1.]});
    ok ~msg:"str"    (cp {cpool with string=["bar";"foo"]}) (cp {cpool with string=["foo";"bar";"foo"]});
    ok ~msg:"ns"
      (cp {cpool with
	     string=["bar";"foo"];
	     namespace=[Namespace 2]})
      (cp {cpool with
	     string=["foo";"bar";"foo"];
	     namespace=[Namespace 1; Namespace 3]});
    ok ~msg:"nss"
      (cp {cpool with
	     string=["bar";"foo"];
	     namespace=[Namespace 2];
	     namespace_set=[[1]];})
      (cp {cpool with
	     string=["foo";"bar";"foo"];
	     namespace=[Namespace 1; Namespace 3];
	     namespace_set=[[1];[2]]});
    ok ~msg:"multiname"
      (cp {cpool with
	     string=["bar";"foo"];
	     namespace=[Namespace 2];
	     multiname= [QName(1,2)];})
      (cp {cpool with
	     string=["foo";"bar";"foo"];
	     namespace=[Namespace 1; Namespace 3];
	     multiname= [QName(1,1); QName(2,3)]})
  end;
  "multiname" >:: begin fun () ->
    ok
      {abc with
	 cpool = {cpool with
		    string        = ["foo";"bar"];
		    namespace     = [Namespace 1];
		    multiname     = [QName(1,1)];
		 };
	 method_info = [{info with method_name=1}] }
      {abc with
	 cpool = {cpool with
		    string        = ["foo";"bar"];
		    namespace     = [Namespace 1];
		    multiname     = [QName(1,1); QName(1,1)];
		 };
	 method_info = [{info with method_name=2}] }
  end;
  "zero" >:: begin fun () ->
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
	 method_info = [{info with method_name=0}] }
  end;
] end +> run_test_tt_main

