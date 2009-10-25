open Base
open OUnit
open Link
open Swflib.AbcType

let ok ?msg x y z =
  assert_equal ?msg x (link y z)


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
  end
] end +> run_test_tt_main

