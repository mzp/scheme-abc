open Base
open OUnit
open AbcType

let ok x y =
  OUnit.assert_equal ~printer:Std.dump x y

module A = AbcIn.Make(struct
			 type t = int
			 let of_bytes _ =
			   raise Stream.Failure
		       end)

let example name =
  let ch =
    open_in_bin @@ Printf.sprintf "%s.abc" name in
    A.of_bytes @@ BytesIn.of_channel ch

let abc =
  example "hello"

let cpool =
  abc.cpool

let _ =
  ("asm module test" >::: [
     "cpool" >::: [
       "integer" >::
	 (fun () -> ok [] cpool.int);
       "uinteger" >::
	 (fun () -> ok [] cpool.uint);
       "double" >::
	 (fun () -> ok [] cpool.double);
       "string" >::
	 (fun () -> ok [""; "Hello,world!!";"print"] cpool.string);
(* TODO *)
(*       "namespace" >::
	 (fun () ->
	    match cpool.namespace with
		[ns] ->
		  ok (`Namespace 1l) ns
	      | _ ->
		  assert_failure "list size is over");*)

       "namespace set" >::
	 (fun () ->
	    ok [] cpool.namespace_set);
       "multiname" >::
	 (fun () ->
	    assert_equal [QName (1,1);QName (1,3)]
	      cpool.multiname)
     ];
   "method signature" >::
     (fun () ->
	match abc.method_info with
	    [m] ->
	      ok [] m.params;
	      ok 0 m.return;
	      ok 1 m.method_name;
	      ok 0 m.method_flags;
	  | _   ->
	      assert_failure "over size");
   "metadata test" >::
     (fun () ->
	ok [] abc.metadata);
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
