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

let ok_s expect actual =
  assert_equal (toplevel expect) @@ generate_script [actual]

let count =
  ref 0

let uniq () =
  incr count;
  !count

let inner args inst =
  {Asm.empty_method with
     name =
      make_qname "";
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

let _ =
  ("codegen.ml(stmt)" >::: [
     "redefine should use setproperty" >::
       (fun () ->
	  ok_s [PushByte 42;
		GetScopeObject 0;
		Swap;
		SetProperty (qname "f")] @@
	    redefine (`Public (global "f")) 0 (int 42));
     "define should use PushWith" >::
       (fun () ->
	  ok_s [NewObject 0;Dup;PushWith;
		PushByte 42;
		SetProperty (qname "f")] @@
	    define (`Public (global "f")) (int 42));
   ]) +> run_test_tt


