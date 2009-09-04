open Base
open OUnit
open ISpec
open Asm
open Bytes

(* start prefix *)
let test_cases = ref []
let test name body =
  test_cases := (name >:: body)::!test_cases;;

let ok =
  OUnit.assert_equal

(* test util *)
let empty_method =
{ ISpec.method_attrs = [];
  method_name        = `QName (`Namespace "","");
  params             = [];
  return             = 0;
  method_flags       = 0;
  instructions       = [];
  traits             = [];
  exceptions         = [];
  fun_scope          = `Global
}

let insts insts =
  {empty_method with instructions=insts}

(* stub module *)
module Inst = struct
  type t =
      [ `OpOnly1  | `OpOnly2
      | `OpOnly3  | `OpOnly4
      | `WithArgs | `WithPrefix
      | `String   | `Int
      |	`StackAdd | `StackDel
      | `ScopeAdd | `ScopeDel
      | `Meth
      | `Class ]

  let default : t ISpec.t = {
    op=0;
    args=const [];
    prefix=const [];
    const=[];
    method_  = None;
    class_ = None;
    stack=0;
    scope=0;
    count=0;
  }

  let spec =
    function
	`OpOnly1 ->
	  {default with op=101}
      | `OpOnly2 ->
	  {default with op=102}
      | `OpOnly3 ->
	  {default with op=103}
      | `OpOnly4 ->
	  {default with op=104}
      | `WithArgs ->
	  {default with args=const [u8 1]}
      | `WithPrefix ->
	  {default with prefix=const [u8 2]}
      | `String ->
	  {default with const=[`String "foo"]}
      | `Int ->
	  {default with const=[`Int 42]}
      |	`StackAdd ->
	  {default with stack=1}
      | `StackDel ->
	  {default with stack= -1}
      | `ScopeAdd ->
	  {default with scope=1}
      | `ScopeDel ->
	  {default with scope= -1}
      | `Meth ->
	  {default with method_ =
	      Some {(insts [`OpOnly1]) with
		      method_name = `QName (`Namespace "","f")}}
      | `Class ->
	  {default with class_ = Some {
	     class_name = `QName (`Namespace "","Foo");
	     super      = `QName (`Namespace "","Object");
	     class_flags= [`Sealed];
	     cinit      = insts [`OpOnly1];
	     iinit      = insts [`OpOnly2];
	     interface  = [];
	     instance_methods = [insts [`OpOnly3]];
	     static_methods   = [insts [`OpOnly4]];
	     attributes = [];
	   }
	  }
end

module A = Asm.Make(Inst)

let _ = test "Instruction" begin
  fun () ->
    let {method_info=mi;
	 method_body=mb} =
      A.assemble_method @@ insts [`OpOnly1; `OpOnly2] in
      ok 1 @@ List.length mi;
      ok 1 @@ List.length mb;
      ok 0 @@ (List.hd mb).Abc.method_sig;
      ok [u8 101; u8 102] @@ (List.hd mb).Abc.code
end

let _ = test "args/prefix" begin
  fun () ->
    let {method_info=mi;
	 method_body=mb} =
      A.assemble_method @@ insts [`WithArgs; `WithPrefix] in
      ok 1 @@ List.length mi;
      ok 1 @@ List.length mb;
      ok 0 @@ (List.hd mb).Abc.method_sig;
      ok [u8 0; u8 1;
		    u8 2; u8 0] @@ (List.hd mb).Abc.code
end

let _ = test "constant" begin
  fun () ->
    let {cpool=cpool} =
      A.assemble_method @@ insts [`String; `Int; `Meth] in
    let cpool' =
      List.fold_left (flip Cpool.add) Cpool.empty [
	`String "foo";
	`Int    42;
	`QName (`Namespace "","f");
	`QName (`Namespace "","");
      ] in
      ok cpool' cpool
end

let _ = test "stack" begin
  fun () ->
    let {method_info=mi;
	 method_body=mb} =
      A.assemble_method @@ insts [`StackAdd; `StackAdd; `StackDel] in
      ok 1 @@ List.length mi;
      ok 1 @@ List.length mb;
      ok 2 @@ (List.hd mb).Abc.max_stack;
end

let _ = test "scope" begin
  fun () ->
    let {method_info=mi;
	 method_body=mb} =
      A.assemble_method @@ insts [`ScopeAdd; `ScopeAdd; `ScopeDel] in
      ok 1 @@ List.length mi;
      ok 1 @@ List.length mb;
      ok 2 @@ (List.hd mb).Abc.max_scope_depth;
      ok 0 @@ (List.hd mb).Abc.init_scope_depth
end

let _ = test "method" begin
  fun () ->
    let {method_info=mi;
	 method_body=mb} =
      A.assemble_method @@ insts [`Meth] in
      ok 2 @@ List.length mi;
      ok 2 @@ List.length mb;
      ok 0 @@ (List.nth mb 0).Abc.method_sig;
      ok 1 @@ (List.nth mb 1).Abc.method_sig;
      ok [u8 101] @@ (List.nth mb 0).Abc.code;
      ok [u8 0]   @@ (List.nth mb 1).Abc.code;
end

let _ = test "method dup" begin
  (* same method should NOT be unified for AVM2 restriction *)
  fun () ->
    let {method_info=mi;
	 method_body=mb} =
      A.assemble_method @@ insts [`Meth; `Meth] in
      ok 3 @@ List.length mi;
      ok 3 @@ List.length mb
end

let _ = test "class" begin
  fun () ->
    let {method_info   = mi;
	 method_body   = mb;
	 instance_info = ii;
	 class_info    = ci;
	 cpool         = cp } =
      A.assemble_method @@ insts [`Class] in
    let nth_method i =
      (List.nth mb i).Abc.code in
      ok 1 @@ List.length ci;
      ok 1 @@ List.length ii;
      ok 5 @@ List.length mi;
      ok 5 @@ List.length mb;
      let assert_cpool expect acutal =
	ok (Cpool.index expect cp) @@ acutal in
      let c =
	List.hd ci in
      let i =
	List.hd ii in
	(* class info *)
	ok [u8 101] @@ nth_method c.Abc.cinit;
	begin match c.Abc.class_traits with
	    [{ Abc.trait_name = name;
	       data           = Abc.MethodTrait (0,method_i,[])}] ->
	      ok [u8 104] @@ nth_method method_i;
	      assert_cpool (`QName (`Namespace "","")) @@ name
	  | _ ->
	      assert_failure "must not happen" end;
	(* instance info *)
	assert_cpool (`QName (`Namespace "","Foo")) @@ i.Abc.instance_name;
	assert_cpool (`QName (`Namespace "","Object")) @@ i.Abc.super_name;
	ok [Abc.Sealed] @@ i.Abc.instance_flags;
	ok [u8 102] @@ (List.nth mb i.Abc.iinit).Abc.code;
	begin match i.Abc.instance_traits with
	    [{ Abc.trait_name = name;
	       data           = Abc.MethodTrait (0,method_i,[])}] ->
	      ok [u8 103] @@ nth_method method_i;
	      assert_cpool (`QName (`Namespace "","")) @@ name
	  | _ ->
	      assert_failure "must not happen" end;
end

(* end prefix *)
let _ =
  run_test_tt_main ("asm.ml" >::: !test_cases)
