open Base
open Asm
open Ast
open Cpool
open Codegen
open Util
open OUnit
open AstUtil

(** util function *)
let compile_string str =
  snd @@ VarResolve.trans @@ BindCheck.uncheck @@ ModuleTrans.trans @@ ClosTrans.trans @@ Lisp.compile_string str

let string_of_insts xs =
  let ys =
    String.concat "; \n\t" @@ List.map string_of_instruction xs in
    Printf.sprintf "[\n\t%s ]\n" ys

let ok lhs rhs =
  OUnit.assert_equal ~printer:Std.dump ~msg:"name"
    lhs.name         rhs.name;
  OUnit.assert_equal ~printer:Std.dump ~msg:"params"
    lhs.params       rhs.params;
  OUnit.assert_equal ~printer:Std.dump ~msg:"return"
    lhs.return       rhs.return;
  OUnit.assert_equal ~printer:Std.dump ~msg:"flags"
    lhs.flags        rhs.flags;
  OUnit.assert_equal ~printer:string_of_insts ~msg:"instructions"
    lhs.instructions rhs.instructions;
  OUnit.assert_equal ~printer:Std.dump ~msg:"traits"
    lhs.traits       rhs.traits;
  OUnit.assert_equal ~printer:Std.dump ~msg:"exceptions"
    lhs.exceptions   rhs.exceptions

let expr inst =
  {Asm.empty_method with
     name =
      make_qname "";
     instructions=
      [GetLocal_0;PushScope]@inst@[Pop;ReturnVoid]}

let toplevel inst =
  {Asm.empty_method with
     name =
      make_qname "";
     instructions=
      [GetLocal_0;PushScope]@inst@[ReturnVoid]}

let count =
  ref 0

let uniq () =
  incr count;
  !count

let inner args inst =
  {Asm.empty_method with
     name =
      make_qname @@ string_of_int @@ uniq ();
     params =
      args;
     instructions=
      inst@[ReturnValue] }

let qname name =
  QName ((Namespace ""),name)

let compile x =
  generate_script @@ snd @@ VarResolve.trans [`Expr x]

let stmt x =
  generate_script @@ snd @@ VarResolve.trans x

let new_class klass =
  (toplevel [
     GetLex klass.Asm.sname;
     PushScope;
     GetLex klass.Asm.sname;
     NewClass klass;
     PopScope;
     GetGlobalScope;
     Swap;
     InitProperty klass.Asm.cname])

let prefix= [GetLocal_0;
	     ConstructSuper 0]

let init =
  {Asm.empty_method with
     name =
      make_qname "init";
     fun_scope =
      Asm.Class (make_qname "Foo");
     instructions =
      prefix @ [ReturnVoid] }

let cinit =
  {Asm.empty_method with
     name =
      make_qname "cinit";
     fun_scope =
      Asm.Class (make_qname "Foo");
     instructions =
      [ReturnVoid] }

let _ =
  ("codegen.ml(class)" >::: [
     "expr" >::: [
       "new" >::
	 (fun () ->
	    ok (expr [FindPropStrict (make_qname "Foo");
		      ConstructProp (make_qname "Foo",0)]) @@
	      generate_script @@ compile_string "(new Foo)");
       "new arguments" >::
	 (fun () ->
	    ok (expr [FindPropStrict (make_qname "Foo");
		      PushByte 42;ConstructProp (make_qname "Foo",1)]) @@
	      generate_script @@ compile_string "(new Foo 42)");
       "invoke" >::
	 (fun () ->
	    ok (expr [GetLex (make_qname "x");
		      PushByte 10;
		      CallProperty (make_qname "foo",1)]) @@
	      generate_script @@ compile_string "(. x (foo 10))");
       "slot-ref" >::
	 (fun () ->
	    ok (expr [GetLex (make_qname "obj");
		      GetProperty (make_qname "x")]) @@
	      generate_script @@ compile_string "(slot-ref obj x)");
       "slot-set!" >::
	 (fun () ->
	    ok (expr [PushByte 42;
		      GetLex (make_qname "obj");
		      Swap;
		      SetProperty (make_qname "x");
		      PushUndefined]) @@
	      generate_script @@ compile_string "(slot-set! obj x 42)");
     ];
     "stmt" >::: [
       "normal" >::
	 (fun () ->
	    ok
	      (new_class
		 {Asm.cname = make_qname "Foo";
		  sname     = make_qname "Object";
		  flags_k   = [Asm.Sealed];
		  attributes = [];
		  cinit     = cinit;
		  iinit     = {init with
				 instructions =
		      prefix@[PushByte 10;Pop]@[ReturnVoid] };
		  interface = [];
		  methods   = []}) @@
	      generate_script @@ compile_string "(define-class Foo (Object) ())
          (define-method init ((self Foo)) 10)");
       "empty" >::
	 (fun () ->
	    ok (new_class
		  {Asm.cname = make_qname "Foo";
		   sname     = make_qname "Object";
		   flags_k   = [Asm.Sealed];
		   attributes= [];
		   cinit     = cinit;
		   iinit     = init;
		   interface = [];
		   methods   = []}) @@
	      generate_script @@ compile_string
	      "(define-class Foo (Object) ())");
       "method" >::
	 (fun ()->
	    ok (new_class
		  {Asm.cname = make_qname "Foo";
		   sname     = make_qname "Object";
		   flags_k   = [Asm.Sealed];
		   attributes= [];
		   cinit     = cinit;
		   iinit     = init;
		   interface = [];
		   methods   = [{ Asm.empty_method with
				    name = make_qname "f";
				    fun_scope = Asm.Class (make_qname "Foo");
				    instructions = [PushByte 42;ReturnValue] }]}) @@
	      generate_script @@ compile_string
		 "(define-class Foo (Object) ())
          (define-method f ((self Foo)) 42)");
       "namespace" >::
	 (fun () ->
	    let make ns x =
	      QName ((Namespace ns),x) in
	      ok (new_class
		    {Asm.cname =
			make_qname "Foo";
		     sname =
			make "flash.text" "Object";
		     flags_k =
			[Asm.Sealed];
		     attributes =
			[];
		     cinit =
			cinit;
		     iinit =
			{init with
			   instructions = prefix @ [PushByte 42; Pop; ReturnVoid]};
		     interface = [];
		     methods   = []}) @@
		generate_script @@ compile_string
		"(define-class Foo (flash.text.Object) ())
              (define-method init ((self Foo))  42)");
       "method arguments" >::
	 (fun () ->
	    ok (new_class
		  {Asm.cname = make_qname "Foo";
		   sname     = make_qname "Object";
		   flags_k   = [Asm.Sealed];
		   attributes = [];
		   cinit     = cinit;
		   iinit     = {init with
				  params = [0];
				  instructions = List.concat [
				    prefix;
				    [GetLocal 1; Pop;ReturnVoid] ] };
		   interface = [];
		   methods   = []}) @@
	      generate_script @@ compile_string
	      "(define-class Foo (Object) ())
          (define-method init ((self Foo) x) x)");
       "self" >::
	 (fun () ->
	    ok (new_class
		  {Asm.cname = make_qname "Foo";
		   sname     = make_qname "Object";
		   flags_k   = [Asm.Sealed];
		   attributes = [];
		   cinit     = cinit;
		   iinit     = {init with
				  instructions = prefix @ [GetLocal 0;Pop;ReturnVoid] };
		   interface = [];
		   methods   = []}) @@
	      generate_script @@ compile_string
	      "(define-class Foo (Object) ())
          (define-method init ((self Foo)) self)");
       "attributes" >::
	 (fun () ->
	    ok (new_class
		  {Asm.cname = make_qname "Foo";
		   sname     = make_qname "Object";
		   flags_k   = [Asm.Sealed];
		   cinit     = cinit;
		   iinit     = init;
		   interface = [];
		   attributes = [Cpool.make_qname "x";Cpool.make_qname "y"];
		   methods   = []}) @@
	      generate_script @@ compile_string "(define-class Foo (Object) (x y))");
     ]
   ]) +> run_test_tt


