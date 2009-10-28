open Base
open OUnit
open Link
open Swflib.AbcType

let ok ?msg x y z =
  assert_equal ?msg ~printer:Std.dump x (link y z)

let lok ?msg x y =
  ok ?msg x y y

let cpool = {
  int           = [];
  uint          = [];
  double        = [];
  string        = [];
  namespace     = [];
  namespace_set = [];
  multiname     = []
}

let abc = {
  cpool         = cpool;
  method_info   = [];
  metadata      = [];
  classes       = [];
  instances     = [];
  scripts       = [];
  method_bodies = []
}

let info = {
  params       = [];
  return       = 0;
  method_name  = 0;
  method_flags = []
}

let body = {
  method_sig       = 0;
  max_stack        = 0;
  local_count      = 0;
  init_scope_depth = 0;
  max_scope_depth  = 0;
  code             = [];
  exceptions       = [];
  method_traits    = []
}

let class_ = {
  cinit=0;
  class_traits=[];
}

let instance={
  instance_name=0;
  super_name=0;
  instance_flags=[];
  interfaces=[];
  iinit=0;
  instance_traits=[]
}

let _ = begin "link.ml" >::: [
  "ident" >:: begin fun () ->
    ok abc abc abc
  end;
  "cpool" >:: begin fun () ->
    let ok_c msg x y z =
      ok ~msg {abc with cpool=x} {abc with cpool=y} {abc with cpool=z} in
      ok_c "string"
	{ cpool with string=["foo"; "bar"; "foo"; "baz"] }
	{ cpool with string=["foo";"bar"]}
	{ cpool with string=["foo";"baz"]};
      ok_c "number"
	{ cpool with int    = [0 ; 1]; uint   = [0 ; 1]; double = [0.; 1.]}
	{ cpool with int    = [0 ];    uint   = [0 ];    double = [0.]}
	{ cpool with int    = [1 ];    uint   = [1 ];    double = [1.]};
      ok_c "reloc"
	{ cpool with
	    string=["foo"; "bar"];
	    namespace=[Namespace 1; PrivateNamespace 1; Namespace 2];
	    namespace_set=[[1];[3]];
	    multiname=[QName(1,1); QName(3,2)]}
	{ cpool with
	    string=["foo"];
	    namespace=[Namespace 1; PrivateNamespace 1];
	    namespace_set=[[1]];
	    multiname=[QName(1,1)]}
	{ cpool with
	    string=["bar"];
	    namespace=[Namespace 1];
	    namespace_set=[[1]];
	    multiname=[QName(1,1)]}
  end;
  "info" >:: begin fun () ->
    lok ~msg:"link"
      {abc with method_info = [info; info]}
      {abc with method_info = [info]};
    lok ~msg:"reloc"
      {abc with
	 cpool={cpool with
		   string=["foo"; "foo"];
		   namespace=[Namespace 1;Namespace 2];
		  multiname=[QName(1,1); QName(2,2)]};
	 method_info = [{info with method_name=1};
			{info with method_name=2}]}
      {abc with
	 cpool={ cpool with
		   string=["foo"];
		   namespace=[Namespace 1];
		   multiname=[QName(1,1)]};
	 method_info = [{info with method_name=1}]}
  end;
  "body">:: begin fun () ->
    ok ~msg:"link"
      {abc with
	 cpool={cpool with string=["foo"; "bar"; "foo"; "baz"]};
	 method_info = [info; info];
	 method_bodies=[body; { body with method_sig=1 }]}
      {abc with
	 cpool={ cpool with string=["foo";"bar"]};
	 method_info = [info];
	 method_bodies=[body]}
      {abc with
	 cpool={ cpool with string=["foo";"baz"]};
	 method_info = [info];
	 method_bodies=[body]};
    ok ~msg:"reloc"
      {abc with
	 cpool={cpool with string=["foo"; "bar"; "foo"; "baz"]};
	 method_info = [info; info];
	 method_bodies=[{ body with code=[`PushString 1; `PushString 2]};
			{ body with method_sig=1; code=[`PushString 3; `PushString 4]}]}
      {abc with
	 cpool={ cpool with string=["foo";"bar"]};
	 method_info = [info];
	 method_bodies=[{ body with code=[`PushString 1; `PushString 2]}]}
      {abc with
	 cpool={ cpool with string=["foo";"baz"]};
	 method_info = [info];
	 method_bodies=[{ body with code=[`PushString 1; `PushString 2]}]}
  end;
  "class" >:: begin fun () ->
    lok
      {abc with
	 method_info=[info;info];
	 method_bodies=[body;{body with method_sig=1}];
	 classes=[{class_ with  cinit=0}; {class_ with  cinit=1}]}
      {abc with
	 method_info=[info];
	 method_bodies=[body];
	 classes=[{class_ with  cinit=0}]}
  end;
  "instance" >:: begin fun () ->
    lok
      {abc with
	 cpool={cpool with
		   string    = ["foo"; "foo"];
		   namespace = [Namespace 1;Namespace 2];
		   multiname = [QName(1,1); QName(1,1); QName(2,2); QName(2,2)]};
	 method_info   = [info;info];
	 method_bodies = [body;
			  {body with method_sig=1}];
	 instances     = [{ instance with instance_name=1; super_name=2; };
			  { instance with instance_name=3; super_name=4; iinit=1; }]}
      {abc with
	 cpool={ cpool with
		   string    = ["foo"];
		   namespace = [Namespace 1];
		   multiname = [QName(1,1); QName(1,1)]};
	 method_info   = [info];
	 method_bodies = [body];
	 instances     = [{ instance with instance_name=1; super_name=2; }]}
  end;
  "slot/const trait" >:: begin fun () ->
    let cp1 =
      { cpool with
	  string=["foo"];
	  namespace=[Namespace 1];
	  multiname=[QName(1,1)]} in
    let cp2 =
      { cpool with
	  string=["foo"; "foo"];
	  namespace=[Namespace 1; Namespace 2];
	  multiname=[QName(1,1); QName(2,2)]} in
    let tok msg t1 t2 t3 =
      lok ~msg
	{abc with
	   cpool=cp2;
	   classes=[{class_ with class_traits= t1};
		    {class_ with class_traits= t2}]}
	{abc with
	   cpool=cp1;
	   classes=[{class_ with class_traits= t3}]} in
      (* TODO: add vindex/vkind *)
      tok "slot"
	[{ trait_name=1; data=SlotTrait(0,1,0,0); trait_metadata=[] }]
	[{ trait_name=2; data=SlotTrait(0,2,0,0); trait_metadata=[] }]
	[{ trait_name=1; data=SlotTrait(0,1,0,0); trait_metadata=[] }];
      tok "const"
	[{ trait_name=1; data=ConstTrait(0,1,0,0); trait_metadata=[] }]
	[{ trait_name=2; data=ConstTrait(0,2,0,0); trait_metadata=[] }]
	[{ trait_name=1; data=ConstTrait(0,1,0,0); trait_metadata=[] }]
  end;
  "class trait" >:: begin fun () ->
    let cp1 =
      { cpool with
	  string=["foo"];
	  namespace=[Namespace 1];
	  multiname=[QName(1,1)]} in
    let cp2 =
      { cpool with
	  string=["foo"; "foo"];
	  namespace=[Namespace 1; Namespace 2];
	  multiname=[QName(1,1); QName(2,2)]} in
    let tok msg t1 t2 t3 =
      lok ~msg
	{abc with
	   cpool=cp2;
	   classes=[{class_ with class_traits= t1};
		    {class_ with class_traits= t2}]}
	{abc with
	   cpool=cp1;
	   classes=[{class_ with class_traits= t3}]} in
      tok "class"
	[{ trait_name=1; data=ClassTrait(0,1); trait_metadata=[] }]
	[{ trait_name=2; data=ClassTrait(0,2); trait_metadata=[] }]
	[{ trait_name=1; data=ClassTrait(0,1); trait_metadata=[] }]
  end;
  "fun/method/getter/setter trait" >:: begin fun () ->
    let cp1 =
      { cpool with
	  string=["foo"];
	  namespace=[Namespace 1];
	  multiname=[QName(1,1)]} in
    let cp2 =
      { cpool with
	  string=["foo"; "foo"];
	  namespace=[Namespace 1; Namespace 2];
	  multiname=[QName(1,1); QName(2,2)]} in
    let tok msg t1 t2 t3 =
      lok ~msg
	{abc with
	   cpool=cp2;
	   method_info = [info;info];
	   classes=[{class_ with class_traits= t1};
		    {class_traits= t2; cinit=1}]}
	{abc with
	   cpool=cp1;
	   method_info = [info];
	   classes=[{class_ with class_traits= t3}]} in
      tok "fun"
	[{ trait_name=1; data=FunctionTrait(0,0); trait_metadata=[]}]
	[{ trait_name=2; data=FunctionTrait(0,1); trait_metadata=[]}]
	[{ trait_name=1; data=FunctionTrait(0,0); trait_metadata=[]}];
      tok "meth"
	[{ trait_name=1; data=MethodTrait(0,0,[]); trait_metadata=[]}]
	[{ trait_name=2; data=MethodTrait(0,1,[]); trait_metadata=[]}]
	[{ trait_name=1; data=MethodTrait(0,0,[]); trait_metadata=[]}];
      tok "getter"
	[{ trait_name=1; data=GetterTrait(0,0,[]); trait_metadata=[]}]
	[{ trait_name=2; data=GetterTrait(0,1,[]); trait_metadata=[]}]
	[{ trait_name=1; data=GetterTrait(0,0,[]); trait_metadata=[]}];
      tok "setter"
	[{ trait_name=1; data=SetterTrait(0,0,[]); trait_metadata=[]}]
	[{ trait_name=2; data=SetterTrait(0,1,[]); trait_metadata=[]}]
	[{ trait_name=1; data=SetterTrait(0,0,[]); trait_metadata=[]}]
  end;
  "script" >:: begin fun () ->
    lok
      {abc with
	 method_info=[info;info];
	 scripts=[{init=0;script_traits=[]};
		  {init=1;script_traits=[]}]}
      {abc with
	 method_info=[info];
	 scripts=[{init=0;script_traits=[]}]}
  end
] end +> run_test_tt_main

