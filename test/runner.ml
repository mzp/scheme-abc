(* CAUTION: this is a generated file.  If you edit it, all changes will be lost! *)
# 1 "runner.mlp"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "./runner.h" 1















# 1 "<command-line>" 2
# 1 "runner.mlp"
# 1 "runner.h" 1
open Test_abc
open Test_asm
open Test_ast
open Test_base
open Test_bytes
open Test_clostrans
open Test_closuretrans
open Test_codegen
open Test_cpool
open Test_hList
open Test_lexer
open Test_lisp
open Test_sexp
open Testtbl
open Util
# 2 "runner.mlp" 2

let _ =
  Testtbl.run_test ()
