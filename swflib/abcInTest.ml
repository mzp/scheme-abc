open Base
open OUnit
open BytesOut
open AbcType

let ok ?msg x y =
  OUnit.assert_equal ?msg ~printer:Std.dump x y

module A = AbcIn.Make(struct
			 type t = int
			 let of_bytes _ =
			   raise Stream.Failure
		       end)

let example name =
  let ch =
    open_in_bin @@ Printf.sprintf "%s.abc" name in
    A.of_bytes @@ BytesIn.of_channel ch

let bytes xs =
  Stream.of_list @@ BytesOut.to_int_list xs

let abc =
  example "hello"

let cpool =
  abc.cpool

let _ =
  ("asm module test" >::: [
     "cpool" >:: begin fun () ->
       let char c =
	 u8 @@ Char.code c in
       let cpool =
	 A.to_cpool @@ bytes [
	   (* int *)
	   u30 3; s32 1;s32 2;
	   (* uint *)
	   u30 2; u32 3;
	   (* double *)
	   u30 2; d64 4.2;
	   (* str *)
	   u30 2; u30 2; char 'h'; char 'i';
	   (* ns *)
	   u30 3; u8 0x08; u30 1; u8 0x05; u30 1;
	   (* ns_set *)
	   u30 2; u30 1; u30 1;
	   (* mn *)
	   u30 2; u8 0x07; u30 1; u30 1
	 ] in
	 ok ~msg:"int"    [1;2] cpool.int;
	 ok ~msg:"uint"   [3] cpool.uint;
	 ok ~msg:"double" [4.2] cpool.double;
	 ok ~msg:"str"    ["hi"] cpool.string;
	 ok ~msg:"ns"     [Namespace 1; PrivateNamespace 1] cpool.namespace;
	 ok ~msg:"ns_set" [[1]] cpool.namespace_set;
	 ok ~msg:"mn"     [QName(1,1)] cpool.multiname;
     end;
     "method signature" >::  begin fun () ->
       let m =
	 A.to_method_info @@ bytes [
	   (* param count *) u30 3;
	   (* return_type *) u30 1;
	   (* param types *) u30 1; u30 2; u30 3;
	   (* name *) u30 4;
	   (* flags *) u8 0x03;
	 ] in
	 ok ~msg:"param" [1;2;3] m.params;
	 ok ~msg:"return" 1 m.return;
	 ok ~msg:"name"   4 m.method_name;
	 ok ~msg:"flags"  [NeedArguments; NeedActivation] m.method_flags;
     end;
   "metadata test" >:: begin fun () ->
     let metadata =
       A.to_metadata @@ bytes [
	 (* name *) u30 0;
	 (* item_count *) u30 2;
	 (* items *) u30 1; u30 2; u30 3; u30 4
       ] in
       ok ~msg:"name" 0 metadata.metadata_name;
       ok ~msg:"items" [(1,2);(3,4)] metadata.items
   end;
   "class and instance size has same size" >::
     (fun () ->
        ok (List.length abc.instances) (List.length abc.classes));
   "instance" >::
     (fun () ->
	ok [] abc.instances);
   "class" >::
     (fun () ->
	ok [] abc.classes);
   "script" >::
     (fun () ->
	match abc.scripts with
	    [s] ->
	      ok 0 s.init;
	      ok [] s.script_traits
	  | _   ->
	      assert_failure "error");
   "method body" >::
     (fun () ->
        match abc.method_bodies with
	    [m] ->
	      ok 0 m.method_sig;
	      ok [] m.exceptions;
	      ok 1 m.local_count;
	      ok 0 m.init_scope_depth;
	      ok 1 m.max_scope_depth;
	      ok 2 m.max_stack;
	      ok [] m.method_traits;
	      flip ok m.code [
	      ]
	  | _   ->
	      assert_failure "error");
   ]) +> run_test_tt_main
