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
    code=[u8 1;u8 2;u8 3;s24 1];
    exceptions=[];
    trait_m=[] }

let script =
  {init=0x7F; trait_s=[]}

test script =
  assert_equal [u30 0x7F; u30 0] @@ bytes_of_script script

test trait =
  assert_equal ([u30 1;u8 0; u30 1; u30 2; u30 3; u8 4; u30 0]) (bytes_of_trait {t_name=1; data=SlotTrait (1,2,3,4)});
  assert_equal ([u30 1;u8 0; u30 1; u30 2; u30 0; u30 0]) (bytes_of_trait {t_name=1; data=SlotTrait (1,2,0,4)});
  assert_equal ([u30 1;u8 1; u30 1; u30 2; u30 0]) (bytes_of_trait {t_name=1; data=MethodTrait (1,2)});
  assert_equal ([u30 1;u8 2; u30 1; u30 2; u30 0]) (bytes_of_trait {t_name=1; data=GetterTrait (1,2)});
  assert_equal ([u30 1;u8 3; u30 1; u30 2; u30 0]) (bytes_of_trait {t_name=1; data=SetterTrait (1,2)});
  assert_equal ([u30 1;u8 4; u30 1; u30 2; u30 0]) (bytes_of_trait {t_name=1; data=ClassTrait (1,2)});
  assert_equal ([u30 1;u8 5; u30 1; u30 2; u30 0]) (bytes_of_trait {t_name=1; data=FunctionTrait (1,2)});
  assert_equal ([u30 1;u8 6; u30 1; u30 2; u30 3; u8 4;u30 0]) (bytes_of_trait {t_name=1; data=ConstTrait (1,2,3,4)});
  assert_equal ([u30 1;u8 6; u30 1; u30 2; u30 0;u30 0]) (bytes_of_trait {t_name=1; data=ConstTrait (1,2,0,4)});

test method_info =
  assert_equal [u30 0; u30 1; u30 2; u8 3] @@ 
    bytes_of_method_info info

test method_body =
  let expect = [u30 1;
		u30 2;
		u30 3;
		u30 4;
		u30 5;
		block [u8 1; u8 2; u8 3;s24 1];
		u30 0;
		u30 0] in
  assert_equal expect @@ bytes_of_method_body body

test cpool =
  assert_equal [u30 1;(* int *)
		u30 1;(* uint *)
		u30 1;(* double*)
		u30 1;(* string *)
		u30 1;(* ns *)
		u30 1;(* ns_set *)
		u30 1 (* mname *)] @@ bytes_of_cpool empty_cpool;
  assert_equal [u30 3; s32 ~-1; s32 42; (* int *)
		u30 2; u32 42; (* uint *)
		u30 1;(* double*)
		u30 2; u30 3; u8 0x61; u8 0x62; u8 0x63; (* string *)
		u30 2; u8 0x08; u30 1; (* ns *)
		u30 2; u30 2; u30 1; u30 2; (* ns_set *)
		u30 3; u8 0x07; u30 0; u30 1; u8 0x09; u30 2; u30 3; (* mname *)] @@ 
    bytes_of_cpool cpool

test bytes_of_class =
  assert_equal [u30 10; u30 0;] @@ bytes_of_class {cinit=10; trait_c=[]}

test bytes_of_instance =
  let abc = [
    u30 1; (* name *)
    u30 2; (* super name *)
    u8  3; (* flags *)
    u30 4; (* interface count *)
    u30 1; u30 2; u30 3; u30 4; (* interface *)
    u30 5; (* iinit *)
    u30 0; (* traits count *) ] in
  let instance = {
    name_i=1;
    super_name=2;
    flags_i=[Sealed;Final];
    interface=[1;2;3;4];
    iinit=5;
    trait_i=[]} in
    assert_equal abc (bytes_of_instance instance)

test bytes_of_instance_protected =
  let abc = [
    u30 1; (* name *)
    u30 2; (* super name *)
    u8  8; (* flags *)
    u30 1; (* protected ns *)
    u30 4; (* interface count *)
    u30 1; u30 2; u30 3; u30 4; (* interface *)
    u30 5; (* iinit *)
    u30 0; (* traits count *) ] in
  let instance = {
    name_i=1;
    super_name=2;
    flags_i=[ProtectedNs 1];
    interface=[1;2;3;4];
    iinit=5;
    trait_i=[]} in
    assert_equal abc (bytes_of_instance instance)

test bytes_of_abc =
  let abc =
    {cpool=empty_cpool; method_info=[]; metadata=[]; classes=[]; instances=[]; 
     script=[]; method_body=[]} in
    assert_equal [
      (* version *)
      u16 16; u16 46;
      (* cpool *)
      u30 1; u30 1; u30 1; u30 1; u30 1; u30 1;	u30 1;
      u30 0; (* info *)
      u30 0; (* meta *)
      u30 0; (* class *)
      u30 0; (* script *)
      u30 0; (* body *)
    ] @@ bytes_of_abc abc

test complex_abc = 
  let abc =
    {cpool=empty_cpool; method_info=[]; metadata=[]; classes=[]; instances=[]; 
     script=[]; method_body=[]} in
  let cpool =
    {empty_cpool with string=["foo"]; } in
  let expect = [u30 1;
		u30 2;
		u30 3;
		u30 4;
		u30 5;
		block [u8 1; u8 2; u8 3;s24 1];
		u30 0;
		u30 0] in
    assert_equal (List.concat [
      (* version *)
      [ u16 16; u16 46];
      (* cpool *)
      bytes_of_cpool cpool;
      [u30 1]; bytes_of_method_info info; (* info *)
      [u30 0;  (* meta *) u30 0; (* class *)];
      [u30 1]; bytes_of_script script; (* script *)
      [u30 1]; expect; (* body *)
    ]) @@ bytes_of_abc {abc with 
			  cpool=cpool;
			  method_info=[info];
			  method_body=[body];
			  script=[script]}
