open Base
open Abc
open Util
open Bytes

let cpool =
  { empty_cpool with 
      int = [~-1;42];
      uint = [42];
      string = ["abc"];
      namespace = [{kind=0x08; ns_name=1}];
      namespace_set = [[1;2]];
      multiname=[QName (0,1);Multiname (2,3)] }

let info =
  { params=[]; return=1; name=2; flags=3 }

let body =
  { method_sig=1;
    max_stack=2;
    local_count=3;
    init_scope_depth=4;
    max_scope_depth=5;
    code=[U8 1;U8 2;U8 3];
    exceptions=[];
    trait_m=[] }

let script =
  {init=0x7F; trait_s=[]}

test script =
  assert_equal [U30 0x7F; U30 0] @@ bytes_of_script script

test method_info =
  assert_equal [U30 0; U30 1; U30 2; U8 3] @@ 
    bytes_of_method_info info

test method_body =
  let expect = [U30 1;
		U30 2;
		U30 3;
		U30 4;
		U30 5;
		U30 3; U8 1; U8 2; U8 3;
		U30 0;
		U30 0] in
  assert_equal expect @@ bytes_of_method_body body

test cpool =
  assert_equal [U30 1;(* int *)
		U30 1;(* uint *)
		U30 1;(* double*)
		U30 1;(* string *)
		U30 1;(* ns *)
		U30 1;(* ns_set *)
		U30 1 (* mname *)] @@ bytes_of_cpool empty_cpool;
  assert_equal [U30 3; S32 ~-1; S32 42; (* int *)
		U30 2; U32 42; (* uint *)
		U30 1;(* double*)
		U30 2; U30 3; U8 0x61; U8 0x62; U8 0x63; (* string *)
		U30 2; U8 0x08; U30 1; (* ns *)
		U30 2; U30 2; U30 1; U30 2; (* ns_set *)
		U30 3; U8 0x07; U30 0; U30 1; U8 0x09; U30 2; U30 3; (* mname *)] @@ 
    bytes_of_cpool cpool

test bytes_of_abc =
  let abc =
    {cpool=empty_cpool; method_info=[]; metadata=[]; classes=[]; instances=[]; 
     script=[]; method_body=[]} in
  let cpool =
    {empty_cpool with string=["foo"]; } in
    assert_equal [
      (* version *)
      U16 16; U16 46;
      (* cpool *)
      U30 1; U30 1; U30 1; U30 1; U30 1; U30 1;	U30 1;
      U30 0; (* info *)
      U30 0; (* meta *)
      U30 0; (* class *)
      U30 0; (* script *)
      U30 0; (* body *)
    ] @@ bytes_of_abc abc;
    assert_equal (List.concat [
      (* version *)
      [ U16 16; U16 46];
      (* cpool *)
      bytes_of_cpool cpool;
      [U30 1]; bytes_of_method_info info; (* info *)
      [U30 0;  (* meta *) U30 0; (* class *)];
      [U30 1]; bytes_of_script script; (* script *)
      [U30 1]; bytes_of_method_body body; (* body *)
    ]) @@ bytes_of_abc {abc with 
			  cpool=cpool;
			  method_info=[info];
			  method_body=[body];
			  script=[script]}
