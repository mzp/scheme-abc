open Base
open Str
open ExtList
open Printf

let write name ~ocaml ~byte =
  printf "type %s = %s\n" name ocaml;
  printf "let write_%s= %s\n" name byte

let u30 name =
  write name ~ocaml:"int" ~byte:"u30"

let base name ~cpool ~arg ~clas ~meth =
  printf "let const_%s _x = (%s :> Cpool.entry option)\n" name cpool;
  printf "let arg_%s _ctx _x = %s\n" name arg;
  printf "let class_%s _x = %s\n" name clas;
  printf "let method_%s _x = %s\n" name meth

let high name ~ocaml ~cpool ~arg =
  printf "type %s = %s\n" name ocaml;
  base name ~cpool ~arg ~clas:"None" ~meth:"None"

let lit name ~ocaml =
  high name ~ocaml ~cpool:"None" ~arg:"_x"

let cpool name ~ocaml ~entry =
  high name ~ocaml
    ~cpool:(sprintf "Some (%s _x)" entry)
    ~arg:(sprintf "Cpool.index _ctx#cpool (%s _x)" entry)

let _ =
  match Sys.argv.(1) with
      "-low" ->
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
    | "-high" ->
	cpool "c_int" ~ocaml:"int" ~entry:"`Int";
	cpool "c_uint" ~ocaml:"int" ~entry:"`UInt";
	cpool "c_string" ~ocaml:"string" ~entry:"`String";
	cpool "c_float" ~ocaml:"float" ~entry:"`Double";
	cpool "namespace" ~ocaml:"Cpool.namespace" ~entry:"";
	cpool "multiname" ~ocaml:"Cpool.multiname" ~entry:"";
	lit "u30" ~ocaml:"int";
	lit "u8" ~ocaml:"int";
	high "label" ~ocaml:"Label.t" ~cpool:"None" ~arg:"Left _x";
	base "method_" ~cpool:"None" ~arg:"index _x _ctx#methods"
	  ~clas:"None" ~meth:"Some _x";
	base "class_" ~cpool:"None"  ~arg:"index _x _ctx#classes"
	  ~clas:"Some _x" ~meth:"None";
    | _ ->
	exit 1
