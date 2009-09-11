open Base
open OUnit
open AbcType
open Asm
open Bytes
open Compile

(* start prefix *)
let test_cases = ref []
let test name body =
  test_cases := (name >:: body)::!test_cases;;

let ok x y =
  OUnit.assert_equal ~printer:Std.dump x y

(* test util*)
let empty_method =
{ Compile.method_attrs = [];
  method_name        = `QName (`Namespace "","");
  params             = [];
  return             = 0;
  method_flags       = 0;
  code = [];
  traits             = [];
  exceptions         = [];
  fun_scope          = `Global
}

let insts insts =
   {empty_method with code=insts}

module Inst = struct
  type s =
      [ `OpOnly1  | `OpOnly2
      | `OpOnly3  | `OpOnly4
      | `String   | `Int
      |	`StackAdd | `StackDel
      | `ScopeAdd | `ScopeDel
      | `Meth
      | `Class
      | `Script of s method_ ]
  type t = int

  let class_ =
    function
	`Class ->
	  Some {
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
      | _ ->
	  None

  let method_ =
      function
	  `Meth ->
	    Some {(insts [`OpOnly1]) with
		    method_name = `QName (`Namespace "","f")}
	| _ ->
	    None

  let scope =
    function
	`ScopeAdd ->
	  1
      | `ScopeDel ->
	  -1
      | _ ->
	  0

  let stack =
    function
      	`StackAdd ->
	  1
      | `StackDel ->
	  -1
      | _ ->
	  0

  let const =
    function
	`String ->
	  ([`String "foo"] :> Cpool.entry list)
      | `Int ->
	  ([`Int 42] :> Cpool.entry list)
      | _ ->
	  []

  let inst _ =
    function
	`OpOnly1 ->
	  101
      | `OpOnly2 ->
	  102
      | `OpOnly3 ->
	  103
      | `OpOnly4 ->
	  104
      | _ ->
	  0
end

module C = Compile.Make(Inst)

let to_abc xs =
  C.to_abc @@ (insts xs)

let _ = test "Instruction" begin
  fun () ->
    let {method_info=mi;
	 method_body=mb} =
      to_abc [`OpOnly1; `OpOnly2] in
      ok 1 @@ List.length mi;
      ok 1 @@ List.length mb;
      ok 0 @@ (List.hd mb).method_sig;
      ok [101; 102] @@ (List.hd mb).AbcType.code
end

let _ = test "constant" begin
  fun () ->
    let {cpool=cpool} =
      to_abc [`String; `Int; `Meth] in
      List.iter
	(ignore $ Cpool.index cpool ) [
	  `String "foo";
	  `Int    42;
	  `QName (`Namespace "","f");
	  `QName (`Namespace "","");
	]
end

let _ = test "stack" begin
  fun () ->
    let {method_info=mi;
	 method_body=mb} =
      to_abc [`StackAdd; `StackAdd; `StackDel] in
      ok 1 @@ List.length mi;
      ok 1 @@ List.length mb;
      ok 2 @@ (List.hd mb).max_stack;
end

let _ = test "scope" begin
  fun () ->
    let {method_info=mi;
	 method_body=mb} =
      to_abc [`ScopeAdd; `ScopeAdd; `ScopeDel] in
      ok 1 @@ List.length mi;
      ok 1 @@ List.length mb;
      ok 2 @@ (List.hd mb).max_scope_depth;
      ok 0 @@ (List.hd mb).init_scope_depth
end

let _ = test "method" begin
  fun () ->
    let {method_info=mi;
	 method_body=mb} =
      to_abc [`Meth] in
      ok 2 @@ List.length mi;
      ok 2 @@ List.length mb;
      ok 0 @@ (List.nth mb 0).method_sig;
      ok 1 @@ (List.nth mb 1).method_sig;
      ok [101] @@ (List.nth mb 0).AbcType.code;
      ok [0]   @@ (List.nth mb 1).AbcType.code;
end

let _ = test "method dup" begin
  (* same method should NOT be unified for AVM2 restriction *)
  fun () ->
    let {method_info=mi;
	 method_body=mb} =
      to_abc [`Meth; `Meth] in
      ok 3 @@ List.length mi;
      ok 3 @@ List.length mb
end

let method_trait { trait_name = name; data = data} =
  match data with
    | MethodTrait (0,i,[]) ->
	(name,i)
    | MethodTrait _  | SlotTrait _
    | GetterTrait _  | SetterTrait _
    | ClassTrait _   | FunctionTrait _
    | ConstTrait _ ->
	failwith "munt not happen"

let _ = test "class" begin
  fun () ->
    let {method_info   = mi;
	 method_body   = mb;
	 instance_info = ii;
	 class_info    = ci;
	 cpool         = cp } =
      to_abc [`Class] in
    let nth_method i =
      (List.nth mb i).AbcType.code in
      ok 1 @@ List.length ci;
      ok 1 @@ List.length ii;
      ok 5 @@ List.length mi;
      ok 5 @@ List.length mb;
      let assert_cpool expect acutal =
	ok (Cpool.index cp expect) @@ acutal in
      let c =
	List.hd ci in
      let i =
	List.hd ii in
	(* class info *)
	ok [101] @@ nth_method c.AbcType.cinit;
	begin match c.class_traits with
	    [t] ->
	      let (name,method_i) =
		method_trait t in
		ok [104] @@ nth_method method_i;
		assert_cpool (`QName (`Namespace "","")) @@ name
	  | _::_ | [] ->
	      assert_failure "must not happen" end;
	(* instance info *)
	assert_cpool (`QName (`Namespace "","Foo")) @@ i.instance_name;
	assert_cpool (`QName (`Namespace "","Object")) @@ i.super_name;
	ok [Sealed] @@ i.instance_flags;
	ok [102] @@ (List.nth mb i.AbcType.iinit).AbcType.code;
	begin match i.instance_traits with
	    [t] ->
	      let (name,method_i) =
		method_trait t in
		ok [103] @@ nth_method method_i;
		assert_cpool (`QName (`Namespace "","")) @@ name
	  | _::_ | [] ->
	      assert_failure "must not happen" end;
end

(* end prefix *)
let _ =
  run_test_tt_main ("compile.ml" >::: !test_cases)
