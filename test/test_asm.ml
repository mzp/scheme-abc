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
	instructions=[GetLocal;
		      PushScope;
		      FindPropStrict (QName ((Namespace ""),"print"));
		      PushInt (~-42);
		      PushUInt 42;
		      CallPropLex (QName ((Namespace ""),"print"),1);
		      Pop;
		      ReturnVoid]} in
  let cpool,info,body = 
    assemble [m] in
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
		   Abc.code=[U8 208;
			     U8 48;
			     U8 93; U30 1;
			     U8 45; U30 1; (* pushint *)
			     U8 46; U30 1; (* pushuint *)
			     U8 76; U30 1; U30 1;
			     U8 41;
			     U8 71];
		   Abc.exceptions=[];
		   Abc.trait_m=[] }] body;
