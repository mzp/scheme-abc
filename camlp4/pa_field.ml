(*
  pa_field.ml

  To compile:
    ocamlc -pp camlp4oof -g -I . -I +camlp4 -c pa_field.ml

  To use:
    ocamlc -pp 'camlp4o pa_field.cmo' example.ml
  or
    ocaml dynlink.cma camlp4o.cma pa_field.cmo


  {x; y} == {x=x; y=y}

  Example:
  # type pt = {x:int};;
  type pt = { x : int; }
  # let x = 42 in {x};;
  - : pt = {x = 42}
  # let f {x} = x;;
  val f : pt -> int = <fun>
*)

open Camlp4.PreCast

module Id = struct
  let name = "pa_field"
  let version = "$Id:$"
end

module Field ( Syntax : Camlp4.Sig.Camlp4Syntax) = struct
  include Syntax

  let stream_peek_nth n strm =
    let toks = Stream.npeek n strm in
      try
	Some (fst (List.nth toks (pred n)))
      with Failure _ ->
	None

  let test_no_with =
    let rec test lev strm =
      match stream_peek_nth lev strm with
	| Some (KEYWORD "(" | KEYWORD "with" | KEYWORD "=") ->
	    raise Stream.Failure
	| Some (UIDENT _ | LIDENT _ | KEYWORD ".")  ->
            test (succ lev) strm
	| _ -> () in
      Gram.Entry.of_parser "test_no_with" (test 1)

  EXTEND Gram
    expr: LEVEL "simple" [
      [ "{"; test_no_with; lel = label_expr_list; "}" ->
	  Ast.ExRec (_loc, lel, Ast.ExNil _loc) ]
    ];

    label_expr: [
      [ id = label_longident ->
          Ast.RbEq (_loc, id,Ast.ExId (_loc, id)) ]
    ];

    label_patt: [
      [ id = label_longident ->
          Ast.PaEq (_loc, id, Ast.PaId (_loc, id)) ]
    ];
  END
end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Field)
