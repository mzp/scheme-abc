open Base
open OUnit

let ok x y =
  OUnit.assert_equal ~printer:Std.dump x y

let example name =
  let ch =
    open_in_bin @@ Printf.sprintf "../example/%s.abc" name in
    Abc.of_stream @@ Byte.of_channel ch

let abc =
  example "hello"

let cpool =
  abc#constant_pool

let _ =
  ("asm module test" >::: [
     "major/minor version" >::
       (fun () ->
	  ok 16 abc#minor_version;
	  ok 46 abc#major_version);
     "cpool" >::: [
       "integer" >::
	 (fun () -> ok [] cpool#integer);
       "uinteger" >::
	 (fun () -> ok [] cpool#uinteger);
       "double" >::
	 (fun () -> ok [] cpool#double);
       "string" >::
	 (fun () -> ok [""; "Hello,world!!";"print"] cpool#string);
       "namespace" >::
	 (fun () ->
	    match cpool#namespace with
		[ns] ->
		  ok (`Namespace 1l) ns
	      | _ ->
		  assert_failure "list size is over");
       "namespace set" >::
	 (fun () ->
	    ok [] cpool#ns_set);
       "multiname" >::
	 (fun () ->
	    match cpool#multiname with
		[`QName x;`QName y] ->
		  ok 1 (Int32.to_int x#ns);
		  ok 1 (Int32.to_int x#name);
		  ok 1 (Int32.to_int y#ns);
		  ok 3 (Int32.to_int y#name);
	      | _  ->
		  assert_failure "not qname")
     ];
   "method signature" >::
     (fun () ->
	match abc#methods with
	    [m] ->
	      ok [] m#param_types;
	      ok 0l m#return_type;
	      ok 1l m#name;
	      ok false m#need_activation;
	      ok false m#need_arguments;
	      ok false m#need_rest;
	      ok false m#set_dxns;
	      ok None  m#options;
	      ok None  m#param_names
	  | _   ->
	      assert_failure "over size");
   "metadata test" >::
     (fun () ->
	ok [] abc#metadata);
   "class and instance size has same size" >::
     (fun () ->
        ok (List.length abc#instances) (List.length abc#classes));
   "instance" >::
     (fun () ->
	ok [] abc#instances);
   "class" >::
     (fun () ->
	ok [] abc#classes);
   "script" >::
     (fun () ->
	match abc#script with
	    [s] ->
	      ok 0l s#init;
	      ok [] s#traits
	  | _   ->
	      assert_failure "error");
   "method body" >::
     (fun () ->
        match abc#method_body with
	    [m] ->
	      ok 0l m#methodi;
	      ok [] m#exceptions;
	      ok 1l m#local_count;
	      ok 0l m#init_scope_depth;
	      ok 1l m#max_scope_depth;
	      ok 2l m#max_stack;
	      ok [] m#traits;
	      flip ok m#code [
		`GetLocal_0;
		`PushScope;
		`FindPropStrict 2l;
		`PushString 2l;
		`CallPropLex (2l,1l);
		`Pop;
		`ReturnVoid ]
	  | _   ->
	      assert_failure "error");
   ]) +> run_test_tt
