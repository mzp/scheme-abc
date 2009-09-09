open Base
open Str
open ExtList
open Printf

let write name ~ocaml ~byte =
  printf "type %s = %s\n" name ocaml;
  printf "let write_%s= %s\n" name byte

let u30 name =
  write name ~ocaml:"int" ~byte:"u30"

let _ =
  match Sys.argv.(1) with
      "-writer" ->
	u30 "method_";
	u30 "class_";
	u30 "c_int";
	u30 "c_uint";
	u30 "c_string";
	u30 "c_float";
	u30 "namespace";
	u30 "multiname";
	u30 "u30";
	write "u8" ~ocaml:"int" ~byte:"u8";
	write "label"
	  ~ocaml:"(Label.t,int) either"
	  ~byte:"function
                   Left  label   -> label_ref label
                 | Right address -> s24 address"
    | _ ->
	exit 1
