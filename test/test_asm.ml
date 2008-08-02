open Base
open Asm
open Util
open Cpool
open Bytes

test asm =
    let m = 
      { name  ="main";
	params=[];
	return=0;
	flags =0;
	exceptions=[];
	traits=[];
	instructions=[GetLocal_0;
		      PushScope;
		      FindPropStrict (QName ((Namespace ""),"print"));
		      PushInt (~-42);
		      PushUInt 42;
		      CallPropLex (QName ((Namespace ""),"print"),1);
		      Pop;
		      ReturnVoid]} in
  let cpool,info,body = 
    assemble m in
    assert_equal {Abc.empty_cpool with
		    Abc.int = [~-42];
		    Abc.uint = [42];
		    Abc.string=["";"print"];
		    Abc.namespace = [{Abc.kind=0x08; Abc.ns_name=1}];
		    Abc.multiname = [Abc.QName (1,2)]} cpool;
    assert_equal [{Abc.params=[]; Abc.return=0; Abc.name=0; Abc.flags=0}] info;
    assert_equal [{Abc.method_sig=0; 
		   Abc.max_stack=3; 
		   Abc.local_count=1; 
		   Abc.init_scope_depth=0;
		   Abc.max_scope_depth=1;
		   Abc.code=[u8 208;
			     u8 48;
			     u8 93; u30 1;
			     u8 45; u30 1; (* pushint *)
			     u8 46; u30 1; (* pushuint *)
			     u8 76; u30 1; u30 1;
			     u8 41;
			     u8 71];
		   Abc.exceptions=[];
		   Abc.trait_m=[] }] body;


