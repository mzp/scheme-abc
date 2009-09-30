open Base
open AbcType
open OUnit
open BytesOut

module A = AbcOut.Make(struct
			 type t = int
			 let to_bytes _ = []
		       end)
open A

let cpool =
  { empty_cpool with
      int = [~-1;42];
      uint = [42];
      string = ["abc"];
      namespace = [Namespace 1];
      namespace_set = [[1;2]];
      multiname=[QName (0,1);Multiname (2,3)] }

let info =
  { params=[]; return=1; method_name=2; method_flags=[ NeedArguments; NeedActivation] }

let body =
  { method_sig=1;
    max_stack=2;
    local_count=3;
    init_scope_depth=4;
    max_scope_depth=5;
    code=[];
    exceptions=[];
    method_traits=[] }

let script =
  {init=0x7F; script_traits=[]}

let ok x y =
  OUnit.assert_equal ~printer:(Std.dump)  (to_int_list x) (to_int_list y)

let _ =
  ("abcOut.ml" >:::
     ["of_script test" >::
	(fun () ->
	   ok [u30 0x7F; u30 0] @@ of_script script);
      "of_trait test" >::
	(fun () ->
	   ok [u30 1;u8 0; u30 1; u30 2; u30 3; u8 4; ] @@
	     of_trait {trait_name=1; data=SlotTrait (1,2,3,4); trait_metadata=[]};
	   ok [u30 1;u8 0; u30 1; u30 2; u30 0] @@
	     of_trait {trait_name=1; data=SlotTrait (1,2,0,4); trait_metadata=[]};
	   ok [u30 1;u8 1; u30 1; u30 2] @@
	     of_trait {trait_name=1; data=MethodTrait (1,2,[]); trait_metadata=[]};
	   ok [u30 1;u8 2; u30 1; u30 2] @@
	     of_trait {trait_name=1; data=GetterTrait (1,2,[]); trait_metadata=[]};
	   ok [u30 1;u8 3; u30 1; u30 2] @@
	     of_trait {trait_name=1; data=SetterTrait (1,2,[]); trait_metadata=[]};
	   ok [u30 1;u8 4; u30 1; u30 2] @@
	     of_trait {trait_name=1; data=ClassTrait (1,2); trait_metadata=[]};
	   ok [u30 1;u8 5; u30 1; u30 2] @@
	     of_trait {trait_name=1; data=FunctionTrait (1,2); trait_metadata=[]};
	   ok [u30 1;u8 6; u30 1; u30 2; u30 3; u8 4] @@
	     of_trait {trait_name=1; data=ConstTrait (1,2,3,4); trait_metadata=[]};
	   ok [u30 1;u8 6; u30 1; u30 2; u30 0] @@
	     of_trait {trait_name=1; data=ConstTrait (1,2,0,4);  trait_metadata=[]};);
      "of_method_info test" >::
	(fun () ->
	   ok
	     [u30 0; u30 1; u30 2; u8 3] @@
	     of_method_info info);
      "of_method_body test" >::
	(fun () ->
	   ok [u30 1;
	       u30 2;
	       u30 3;
	       u30 4;
	       u30 5;
	       u30 0;
	       u30 0;
	       u30 0] @@
	     of_method_body body);
      "of_cpool test" >::
	(fun () ->
	   ok [u30 1;(* int    *)
	       u30 1;(* uint   *)
	       u30 1;(* double *)
	       u30 1;(* string *)
	       u30 1;(* ns     *)
	       u30 1;(* ns_set *)
	       u30 1 (* mname  *)] @@
	     of_cpool empty_cpool;
	   ok [u30 3; s32 ~-1; s32 42;                  (* int    *)
	       u30 2; u32 42;                           (* uint   *)
	       u30 1;                                   (* double *)
	       u30 2; u30 3; u8 0x61; u8 0x62; u8 0x63; (* string *)
	       u30 2; u8 0x08; u30 1;                   (* ns     *)
	       u30 2; u30 2; u30 1; u30 2;              (* ns_set *)
	       u30 3; u8 0x07; u30 0; u30 1;
	              u8 0x09; u30 2; u30 3;            (* mname *)] @@
	     of_cpool cpool);
      "of_class test" >::
	(fun () ->
	   ok [u30 10; u30 0] @@
	     of_class {cinit=10; class_traits=[]});
      "of_instance test" >::
	(fun () ->
	   ok [u30 1; (* name *)
	       u30 2; (* super name *)
	       u8  3; (* flags *)
	       u30 4; (* interface count *)
	       u30 1; u30 2; u30 3; u30 4; (* interface *)
	       u30 5; (* iinit *)
	       u30 0; (* traits count *) ] @@
	     of_instance {
	       instance_name=1;
	       super_name=2;
	       instance_flags=[Sealed;Final];
	       interfaces=[1;2;3;4];
	       iinit=5;
	       instance_traits=[]});
      "of_instance protected ns" >::
	(fun () ->
	   ok [u30 1; (* name *)
	       u30 2; (* super name *)
	       u8  8; (* flags *)
	       u30 1; (* protected ns *)
	       u30 4; (* interface count *)
	       u30 1; u30 2; u30 3; u30 4; (* interface *)
	       u30 5; (* iinit *)
	       u30 0; (* traits count *) ] @@
	     of_instance {
	       instance_name=1;
	       super_name=2;
	       instance_flags=[ProtectedNs 1];
	       interfaces=[1;2;3;4];
	       iinit=5;
	       instance_traits=[]});
      "spimle abc" >::
	(fun () ->
	   ok [u16 16; u16 46;(* version *)
	       u30 1; u30 1; u30 1; u30 1; u30 1; u30 1; u30 1;
	       (* cpool *)
	       u30 0; (* info *)
	       u30 0; (* meta *)
	       u30 0; (* class *)
	       u30 0; (* script *)
	       u30 0; (* body *) ] @@
	     to_bytes {
	       cpool       = empty_cpool;
	       method_info = [];
	       metadata    = [];
	       classes     = [];
	       instances   = [];
	       scripts       = [];
	       method_bodies = []});
      "full abc" >::
	(fun () ->
	   ok (List.concat [
	     (* version *) [ u16 16; u16 46];
	     (* cpool   *) of_cpool {empty_cpool with string=["foo"] };
	     (* info    *) [ u30 1]; of_method_info info;
	     (* meta    *) [u30 0];
	     (* class   *) [u30 0];
	     (* script  *) [u30 1]; of_script script;
	     (* body    *) [u30 1]; of_method_body body; ]) @@
	     to_bytes {
	       cpool       = {empty_cpool with string=["foo"] } ;
	       method_info = [info];
	       metadata    = [];
	       classes     = [];
	       instances   = [];
	       scripts     = [script];
	       method_bodies = [body]})
     ] ) +> run_test_tt_main
