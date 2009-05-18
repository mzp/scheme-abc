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

let _ =
  ("asm.ml" >::: [
     ("top level" >:::
       let { abc_cpool = cpool; method_info = info; method_body = body } =
	 assemble {empty with
		     method_name = `QName (`Namespace "","main");
		     instructions = [`PushString "a"; `PushString "b"; `PushString "c"]} in
	 [
	   "cpool" >::
	     (fun () ->
		assert_equal ~printer:Std.dump {Abc.empty_cpool with
				Abc.string    = ["a";"b";"c";"main"; ""];
				namespace = [{Abc.kind = 8; Abc.namespace_name = 5}];
				multiname = [Abc.QName (1, 4)]} cpool);
	   "instruction" >::
	     (fun () ->
		assert_equal ~printer:Std.dump  (Bytes.to_int_list [u8 0x2c;u30 1;
			      u8 0x2c;u30 2;
			      u8 0x2c;u30 3])
		  (Bytes.to_int_list (List.hd body).Abc.code));
	   "usage" >::
	     (fun () ->
		assert_equal 3 (List.hd body).Abc.max_stack;
		assert_equal 0 (List.hd body).Abc.max_scope_depth)
	 ]);
     ("method nested" >:::
      let {abc_cpool =cpool; method_info = info; method_body = body } =
	assemble {empty with
		    instructions = [
		      `PushString "foo";
		      `NewFunction {empty with instructions=[`PushString "bar"]};
		      `NewFunction  {empty with instructions=[`PushString "baz"]}
		    ]}
      in
	[
	  "cpool" >::
	    (fun () ->
	       assert_equal
		 (List.sort compare ["foo";"bar";"baz";""]) @@
		 (List.sort compare cpool.Abc.string));
	  "method count" >::
	    (fun () ->
	       assert_equal 3 @@ List.length info;
	       assert_equal 3 @@ List.length body)
	])
   ]) +> run_test_tt

