open Base
open Str
open Printf
open ExtList

(* util *)
let rec filter_map f =
  function
      x::xs ->
	begin match f x with
	    Some y -> y::filter_map f xs
	  | None   -> filter_map f xs
	end
    | [] ->
	[]

(* type *)
type decl = {
  name   : string;
  opcode : int;
  args   : string list;
  extra  : string
}

(*
   Parsing

  Example:
  PushBytes of int(1F) : stack=1; scope=1
*)
let of_hex s =
  Scanf.sscanf s "%x" id

let parse_entry entry =
  let no_args =
    regexp "\\([A-Z][_a-zA-Z0-9]*\\) *(0x\\([0-9A-Fa-f][0-9A-Fa-f]\\))" in
  let args =
    regexp "\\([A-Z][_a-zA-Z0-9]*\\) *of *\\([^(]*\\) *(0x\\([0-9A-Fa-f][0-9A-Fa-f]\\))" in
    if string_match no_args entry 0 then
      {
	name   = matched_group 1 entry;
	args   = [];
	opcode = of_hex @@ matched_group 2 entry;
	extra  = ""
      }
    else if string_match args entry 0 then
      let name,args,opcode =
	(matched_group 1 entry,
	 matched_group 2 entry,
	 matched_group 3 entry) in
      {
	name   = name;
	args   = split (regexp " *\\* *") args;
	opcode = of_hex @@ opcode;
	extra  = ""
      }
  else
    failwith ("Invalid entry: " ^ entry)

let parse_line s =
  if string_match (regexp "^#\\|^$") s 0  then
    None
  else
    match bounded_split (regexp " *-> *") s 2 with
	[entry; extra] ->
	  Some {parse_entry entry with
		  extra = extra}
      | [entry] ->
	  Some (parse_entry entry)
      | [] | _::_ ->
	  failwith ("Invalid format: " ^ s)

let parse ch =
  let decls =
    ref [] in
    try
      while true do
	match parse_line @@ input_line ch with
	    Some x ->
	      decls := x :: !decls
	  | None ->
	      ()
      done;
      failwith "must not happen"
    with End_of_file ->
      List.rev !decls

let concat_mapi sep f xs =
  String.concat sep @@ List.mapi (fun i x -> f x i) xs

let make_pat name args =
  sprintf "`%s %s"
    name
    (match args with
	 [] -> ""
       | _::_ ->
	   sprintf "(%s)" @@
	     concat_mapi "," (fun _ i -> sprintf "arg%d" i) args)

let call_args prefix args =
    concat_mapi ";" (sprintf "%s_%s arg%d" prefix) args

let cmds = [
  begin "-type",fun {name=name; args=args}->
     if args = [] then
       sprintf "| `%s" name
     else
       sprintf "| `%s of %s" name @@ String.concat "*" args
  end;

  begin "-asm",fun {name=name; opcode=opcode; args=args} ->
     let pat =
       make_pat name args in
     let record =
       sprintf "[u8 0x%x; %s]"
	 opcode
	 (call_args "write" args) in
       sprintf "| %s -> %s" pat record
  end;

  begin "-const",fun {name=name; args=args} ->
    let pat =
      make_pat name args in
      sprintf "| %s -> some_only [%s]" pat @@
	call_args "c" args
  end
]


let f _ =
  let decls =
    parse stdin in
  let f =
    List.assoc Sys.argv.(1) cmds in
    decls
    +> List.map f
    +> List.iter print_endline

let _ = if not !Sys.interactive then
  f ()

