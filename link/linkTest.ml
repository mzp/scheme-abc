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

let method_info = {
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
    ok ~msg:"merge"
      {abc with cpool={ cpool with string=["foo"; "bar"; "foo"; "baz"] }}
      {abc with cpool={ cpool with string=["foo";"bar"]}}
      {abc with cpool={ cpool with string=["foo";"baz"]}};
    ok ~msg:"reloc"
      {abc with cpool={ cpool with
			  string=["foo"; "bar"];
			  namespace=[Namespace 1; Namespace 2] }}
      {abc with cpool={ cpool with
			  string=["foo"];
			  namespace=[Namespace 1]}}
      {abc with cpool={ cpool with
			  string=["bar"];
			  namespace=[Namespace 1]}};
  end;
  "body">:: begin fun () ->
    ok
      {abc with
	 cpool={cpool with string=["foo"; "bar"; "foo"; "baz"]};
	 method_bodies=[{ body with code=[`PushString 1; `PushString 2]};
			{ body with code=[`PushString 3; `PushString 4]}]}
      {abc with
	 cpool={ cpool with string=["foo";"bar"]};
	 method_bodies=[{ body with code=[`PushString 1; `PushString 2]}]}
      {abc with
	 cpool={ cpool with string=["foo";"baz"]};
	 method_bodies=[{ body with code=[`PushString 1; `PushString 2]}]}
  end
] end +> run_test_tt_main

