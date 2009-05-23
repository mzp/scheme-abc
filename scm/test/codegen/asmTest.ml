open Base
open OUnit
open Asm
open Cpool
open Bytes

let empty = {
  method_name = `QName (`Namespace "","");
  params = [];
  return = 0;
  method_flags = 0;
  instructions = [];
  traits= [];
  exceptions= [];
  fun_scope= Global
}

let empty_class = {
  class_name = `QName (`Namespace "","Foo");
  super      = `QName (`Namespace "","Object");
  class_flags= [];
  cinit = empty;
  iinit = empty;
  interface = [];
  instance_methods = [];
  static_methods = [];
  attributes = [];
}

let _ =
  ("asm.ml" >::: [
     ("top level" >:::
       let { cpool = cpool; method_info = info; method_body = body } =
	 assemble {empty with
		     method_name = `QName (`Namespace "","main");
		     instructions = [`PushString "a"; `PushString "b"; `PushString "c"]} in
       let cpool =
	 Cpool.to_abc cpool in
	 [
	   "cpool" >::
	     (fun () ->
		assert_equal ~printer:Std.dump
		  {Abc.empty_cpool with
		     Abc.string    = ["a";"b";"c";"main"; ""];
		     namespace = [{Abc.kind = 8; Abc.namespace_name = 5}];
		     multiname = [Abc.QName (1, 4)]}
		  cpool);
	   "instruction" >::
	     (fun () ->
		assert_equal ~printer:Std.dump
		  [u8 0x2c;u30 1;
		   u8 0x2c;u30 2;
		   u8 0x2c;u30 3]
		  (List.hd body).Abc.code);
	   "usage" >::
	     (fun () ->
		assert_equal 3 (List.hd body).Abc.max_stack;
		assert_equal 0 (List.hd body).Abc.max_scope_depth);
	   "signature" >::
	     (fun () ->
		assert_equal 0 (List.hd body).Abc.method_sig);
	 ]);
     ("method nested" >:::
      let {cpool =cpool; method_info = info; method_body = body } =
	assemble {empty with
		    instructions = [
		      `PushString "foo";
		      `NewFunction {empty with instructions=[`PushString "bar"]};
		      `NewFunction  {empty with instructions=[`PushString "baz"]}
		    ]} in
       let cpool =
	 Cpool.to_abc cpool in
	[
	  "cpool" >::
	    (fun () ->
	       assert_equal
		 {Abc.empty_cpool with
		    Abc.string = ["foo"; "bar"; ""; "baz"];
		    namespace = [{Abc.kind = 8; Abc.namespace_name = 3}];
		    multiname = [Abc.QName (1, 3)]}
		 cpool);
	  "method count" >::
	    (fun () ->
	       assert_equal 3 @@ List.length info;
	       assert_equal 3 @@ List.length body);
	  "1st method is [PushString bar]" >::
	    (fun () ->
	       assert_equal [u8 0x2c;u30 2] (List.nth body 0).Abc.code);
	  "2nd method is [PushString baz]" >::
	    (fun () ->
	       assert_equal ~printer:(Std.dump $ Bytes.to_int_list)
		 [u8 0x2c;u30 4]
		 (List.nth body 1).Abc.code);
	  "3rd method is [NewFunction]" >::
	    (fun () ->
	       assert_equal ~printer:(Std.dump $ Bytes.to_int_list)
		 [u8 0x2c;u30 1;
		  u8 0x40; u30 0;
		  u8 0x40; u30 1]
		 (List.nth body 2).Abc.code);
	]);
     "class test" >:::
       let { cpool = cpool; method_body=body; class_info=class_} =
	 assemble {empty with
		    instructions = [
		      `PushString "foo";
		      `NewClass {empty_class with
				   instance_methods = [{empty with instructions=[`PushString "bar"]}]
				}]} in
       let cpool =
	 Cpool.to_abc cpool in
	 [
	   "cpool" >::
	     (fun () ->
		assert_equal ~printer:Std.dump
		  {Abc.empty_cpool with
		     Abc.string = ["foo"; ""; "bar"; "Foo"; "Object"];
		     namespace = [{Abc.kind = 8; Abc.namespace_name = 2}];
		     Abc.multiname = [Abc.QName (1, 2); Abc.QName (1, 4); Abc.QName (1, 5)]}
		  cpool);
	   "method body" >::
	     (fun () ->
		(* iinit, cinit, member,script *)
		assert_equal 4 (List.length body);
		assert_equal [] (List.nth body 0).Abc.code;
		assert_equal [] (List.nth body 1).Abc.code;
		assert_equal [u8 0x2c;u30 3] (List.nth body 2).Abc.code;
		assert_equal [u8 0x2c;u30 1;
			      u8 0x58;u30 0] (List.nth body 3).Abc.code;
	     );
	   "class info" >::
	     (fun () ->
		assert_equal 1 (List.length class_))
	 ]
   ]) +> run_test_tt

