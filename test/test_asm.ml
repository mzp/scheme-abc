open Base
open Asm
open Util
open Cpool
open Bytes
open OUnit
module Set = Core.Std.Set

let m = 
  { Asm.empty_method with
      Asm.name     = make_qname "main";
      instructions = [GetLocal_0;
		      PushScope;
		      FindPropStrict (QName ((Namespace ""),"print"));
		      PushInt (~-42);
		      PushUInt 42;
		      CallPropLex (QName ((Namespace ""),"print"),1);
		      Pop;
		      ReturnVoid]}

let {abc_cpool=cpool;method_info=info;method_body=body} = 
    assemble m

let _ = 
  ("asm module test" >::: [
     "cpool" >::
       (fun () ->
	  ok {Abc.empty_cpool with
		Abc.int   = [~-42];
		uint      = [42];
		string    = ["";"main";"print"];
		namespace = [{Abc.kind=0x08; ns_name=1}];
		multiname = [Abc.QName (1,2);Abc.QName (1,3)]}
	    cpool);
     "method info" >::
       (fun () ->
	  ok [{Abc.params=[]; return=0; name=1; flags=0}]
	    info);
     "method body" >::
       (fun () ->
	  ok
	    [{Abc.method_sig=0;
	      max_stack=3; 
	      local_count=1; 
	      init_scope_depth=0;
	      max_scope_depth=1;
	      code=[u8 208;
		    u8 48;
		    u8 93; u30 2;
		    u8 45; u30 1; (* pushint *)
		    u8 46; u30 1; (* pushuint *)
		    u8 76; u30 2; u30 1;
		    u8 41;
		    u8 71];
	      exceptions=[];
	      trait_m=[] }]
	    body);
     "collect constant" >::
       (fun () ->
	  ok (Cpool.to_abc @@ List.fold_left 
		Cpool.append Cpool.empty 
		[string "hoge";
		 int 42;
		 multiname @@ make_qname "f";
		 multiname @@ make_qname "g"]) @@
	    Cpool.to_abc @@ collect_const {
	      Asm.empty_method with
		name = make_qname "f";
		instructions = [PushInt 42; 
				NewFunction ({Asm.empty_method with
						name = make_qname "g";
						instructions =
						 [PushString "hoge"]})]});
     "collect method" >::
       (fun () ->
	  let m1 =
	    {Asm.empty_method with
	       name = make_qname "M1";
	       instructions = [] } in
	  let m2 =
	    {Asm.empty_method with
	       name = make_qname "M2";
	       instructions = [NewFunction m1] } in
	  let m3 =
	    {Asm.empty_method with
	       name = make_qname "M2";
	       instructions = [NewFunction m2] } in
	  let m4 =
	    {Asm.empty_method with
	       name = make_qname "M2";
	       instructions = [NewFunction m3] } in
	    ok (Set.to_list @@ Set.of_list [m1;m2;m3;m4]) @@
	      collect_method m4)
   ]) +> run_test_tt
	 
