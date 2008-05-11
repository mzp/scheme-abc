module Id = struct
  let name = "TestCaseCollector"
  let version = "1.0"
end

open Camlp4
module Make(Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

  EXTEND Gram
    GLOBAL: str_item;
    str_item: LEVEL "top"
      [[ "test"; name = LIDENT; "=" ; e = expr ->
	   let s = 
	     Printf.sprintf "%s(%d)" (Loc.file_name _loc) (Loc.start_line _loc) in
	     <:str_item< let _ = Testtbl.add $`str:s$ $str:name$ (fun () -> $exp:e$) >> ]];
  END
end

let module M = Register.OCamlSyntaxExtension(Id)(Make) in
  ()
