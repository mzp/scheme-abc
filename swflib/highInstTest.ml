open Base
open OUnit
open HighInst

let ctx = {|
    cpool   = Cpool.add_list Cpool.empty [
      `Int 42;
    ];
    methods = [];
    classes = []
|};;

let m = {
  MethodType.method_attrs = [];
  method_name        = `QName (`Namespace "","");
  params             = [];
  return             = 0;
  method_flags       = [];
  code = [];
  traits             = [];
  exceptions         = [];
  fun_scope          = `Global
}

let c = {
  MethodType.class_name = `QName (`Namespace "","Foo");
  super      = `QName (`Namespace "","Object");
  class_flags= [`Sealed];
  cinit      = m;
  iinit      = m;
  interface  = [];
  instance_methods = [];
  static_methods   = [];
  attrs = [];
}
let _ =
  ("highInst.ml" >::: [
     "inst" >::
       (fun () ->
	  assert_equal (`PushInt 1) @@ inst ctx (`PushInt 42));
     "const" >::
       (fun () ->
	  assert_equal [`Int 42]       @@ const (`PushInt 42);
	  assert_equal [`String "foo"] @@ const (`PushString "foo"));
     "stack" >::
       (fun () ->
	  assert_equal 1 @@ stack (`PushInt 42);
	  assert_equal 0 @@ stack `Nop);
     "scope" >::
       (fun () ->
	  assert_equal 1 @@ scope `PushScope;
	  assert_equal 0 @@ scope `Nop);
     "method" >::
       (fun () ->
	  assert_equal (Some m) @@ method_ (`NewFunction m);
	  assert_equal None     @@ method_ (`Nop));
     "class" >::
       (fun () ->
	  assert_equal (Some c) @@ class_ (`NewClass c);
	  assert_equal None     @@ class_ `Nop);
   ]) +> run_test_tt_main
