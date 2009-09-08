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
    regexp "\\([A-Z][_a-zA-Z0-9]*\\) *(0x\\([0-9A-F][0-9A-F]\\))" in
  let args =
    regexp "\\([A-Z][_a-zA-Z0-9]*\\) *of *\\([^(]*\\) *(0x\\([0-9A-F][0-9A-F]\\))" in
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
      !decls


let cmds = [
  (* types *)
  ("-type",fun {name=name; args=args}->
     if args = [] then
       sprintf "| `%s" name
     else
       sprintf "| `%s of %s" name @@ String.concat "*" args);
  (* writer *)
  ("-writer",fun {name=name; opcode=opcode; args=args; extra=extra} ->
     if args = [] then
       sprintf "| `%s -> {default with op=0x%x; %s}" name opcode extra
     else
       sprintf "| `%s of (%s) -> {default with op=0x%x; args=[%s]; consts=[%s]; %s}"
	 name
	 (String.concat "," @@ List.mapi (fun i _ -> sprintf "arg%d" i) args)
	 opcode
	 (String.concat ";" @@ List.map ((^) "p_") args)
	 (String.concat ";" @@ List.map ((^) "c_") args)
	 extra);
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

