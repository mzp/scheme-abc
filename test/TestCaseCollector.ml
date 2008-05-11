module Id = struct
  let name = "TestCaseCollector"
  let version = "1.0"
end

open Camlp4
module Make(Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

  let (@@) f g = f g
  let start_with pre str =
    let n =
      String.length pre in
    let m = 
      String.length str in
      if n > m then
	false
      else
	pre = (String.sub str 0 n)

  EXTEND Gram
    GLOBAL: str_item;
    str_item: LEVEL "top"
      [[ "test"; name = LIDENT; "=" ; e = expr ->
	   let s = 
	     Printf.sprintf "%s(line %d)" (Loc.file_name _loc) (Loc.start_line _loc) in
	     <:str_item< let _ = Testtbl.add $`str:s$ $str:name$ (fun () -> $exp:e$) >> 
       | "depend"; prefix = a_STRING; dir = a_STRING -> 
	   let mk_open fname = 
	     let mname =
	       String.capitalize (Filename.chop_extension fname) in
	       <:str_item< open $uid:mname$ >> in
	   let xs = 
	     List.map mk_open @@ 
	       List.filter
	       (fun fname -> start_with prefix fname && Filename.check_suffix fname ".ml")
	       @@ Array.to_list @@ Sys.readdir dir in
	   <:str_item< $list:xs$ >> ]];
  END
end

let module M = Register.OCamlSyntaxExtension(Id)(Make) in
  ()
