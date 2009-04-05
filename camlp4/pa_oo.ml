(*
  Copyright (c) 2008, Jacques Garrigue
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1 Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.

  2 Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

  3 The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(* $Id: pa_oo.ml,v 1.6 2008/05/02 06:03:23 garrigue Exp $ *)

(*
   To compile:
     ocamlc -I +camlp4 -c -pp camlp4orf pa_oo.ml
   To use:
     ocaml camlp4o.cma pa_oo.cmo
   or
     ocamlc -pp 'camlp4o -I . pa_oo.cmo'
*)

open Camlp4.PreCast

module Caml = Syntax

let expand_access _loc mut id e kind =
  let id' = id^"'" in
  let reader = <:class_str_item< method $id$ = $lid:id$ >>
  and writer =
    <:class_str_item< method $"set_"^id$ $lid:id'$ = $lid:id$ := $lid:id'$ >>
  and updater =
    <:class_str_item< method $"set_"^id$ $lid:id'$ = {< $lid:id$ = $lid:id'$ >} >>
  in
  let accessors =
    match kind with None -> <:class_str_item<>>
    | Some k -> match k with
      | `R -> <:class_str_item< $reader$; $updater$ >>
      | `W -> writer
      | `RW -> <:class_str_item< $reader$; $writer$ >>
  in
  <:class_str_item< value $mutable:mut$ $lid:id$ = $e$; $accessors$ >>

(* Copied from camlp4/Camlp4Parsers/Camlp4OCamlRevisedParser.ml *)
let bigarray_set _loc var newval =
  match var with
  | <:expr< Bigarray.Array1.get $arr$ $c1$ >> ->
      Some <:expr< Bigarray.Array1.set $arr$ $c1$ $newval$ >>
  | <:expr< Bigarray.Array2.get $arr$ $c1$ $c2$ >> ->
      Some <:expr< Bigarray.Array2.set $arr$ $c1$ $c2$ $newval$ >>
  | <:expr< Bigarray.Array3.get $arr$ $c1$ $c2$ $c3$ >> ->
      Some <:expr< Bigarray.Array3.set $arr$ $c1$ $c2$ $c3$ $newval$ >>
  | <:expr< Bigarray.Genarray.get $arr$ [| $coords$ |] >> ->
      Some <:expr< Bigarray.Genarray.set $arr$ [| $coords$ |] $newval$ >>
  | _ -> None

let expand_set _loc e1 e2 =
  match bigarray_set _loc e1 e2 with
  | Some e -> e
  | None -> match e1 with
    | <:expr< $o$ # $x$ >> -> <:expr< $o$ # $"set_"^x$ $e2$ >>
    | _ -> <:expr< $e1$ := $e2$ >>

DELETE_RULE Caml.Gram Caml.expr: SELF; "<-"; Caml.expr LEVEL "top" END;;

EXTEND Caml.Gram
  GLOBAL: Caml.class_str_item Caml.expr Caml.opt_mutable Caml.ctyp;
  Caml.class_str_item: [
    [ "val"; "mutable"; `LIDENT lab; e = cvalue_binding; kind = cvalue_kind ->
      expand_access _loc Ast.BTrue lab e kind
    | "val"; `LIDENT lab; e = cvalue_binding; kind = cvalue_kind ->
      expand_access _loc Ast.BFalse lab e kind ]
  ];
  cvalue_kind: [
    [ kind = OPT [ "with"; k =
       [ "reader" -> `R | "writer" -> `W | "accessor" -> `RW ] -> k] ->
      kind ]
  ];
  cvalue_binding: [
    [ "="; e = Caml.expr -> e
    | ":"; t = Caml.ctyp; "="; e = Caml.expr -> <:expr< ($e$ : $t$) >> ]
  ];
  Caml.expr: LEVEL ":=" [
    [ e1 = SELF; "<-"; e2 = Caml.expr LEVEL "top" -> expand_set _loc e1 e2 ]
  ];
  Caml.expr: LEVEL "simple" [
    [ "{|"; cf = LIST1 obj_record SEP ";"; "|}" ->
      <:expr< object $Ast.crSem_of_list cf$ end >> ]
  ];
  obj_record: [
    [ "inherit"; ce = Caml.class_expr -> <:class_str_item< inherit $ce$ >>
    | mf = Caml.opt_mutable; `LIDENT lab; ty = OPT [ ":"; t = Caml.ctyp -> t];
      "="; e = Caml.expr LEVEL "top" ->
        expand_access _loc mf lab e
          (Some(if mf = Ast.BFalse then `R else `RW)) ]
  ];
END;;
