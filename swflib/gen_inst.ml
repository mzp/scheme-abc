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
  extra  : (string*string) list
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
	extra  = []
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
	extra  = []
      }
  else
    failwith ("Invalid entry: " ^ entry)

let split2 sep s =
  match bounded_split (regexp sep) s 2 with
      [a;b]->
	(a,b)
    | [a] ->
	(a,"")
    | [] | _::_ ->
	failwith ("Invalid format: " ^ s)

let parse_line s =
  if string_match (regexp "^#\\|^$") s 0  then
    None
  else
    let (entry,extra) =
      split2 " *-> *" s in
      Some {parse_entry entry with
	      extra = List.map (split2 " *= *") @@ split (regexp " *; *") extra}

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
	     concat_mapi "," (fun _ i -> sprintf "_%d" i) args)

let call_args  prefix args =
    concat_mapi ";" (sprintf "%s_%s _%d" prefix) args

let assoc x xs =
  try
    Some (List.assoc x xs)
  with Not_found ->
    None

let cmds = [
  begin "-type",fun {name; args}->
     if args = [] then
       sprintf "| `%s" name
     else
       sprintf "| `%s of %s" name @@ String.concat "*" args
  end;

  begin "-asm",fun {name; opcode; args;extra} ->
     let pat =
       make_pat name args in
     let record =
       match assoc "prefix" extra with
	   Some "true" ->
	     sprintf "[%s; u8 0x%x]"
	       (call_args "byte" args)
	       opcode
	 | Some _ | None ->
	     sprintf "[u8 0x%x; %s]"
	       opcode
	       (call_args "byte" args) in
       sprintf "| %s -> %s" pat record
  end;

  begin "-disasm",fun {name; opcode; args} ->
     let pat =
       sprintf "[< _ = op %d %s %s >]"
	 opcode
	 (if args = [] then "" else ";") @@
	 concat_mapi ";" (fun x i -> sprintf "arg%d = read_%s" i x ) args in
     let body =
       if args <> [] then
	 sprintf "`%s (%s)" name @@
	   concat_mapi "," (fun _ i -> sprintf "arg%d" i) args
       else
	 sprintf "`%s" name in
       sprintf "| %s -> %s" pat body
  end;

  begin "-tag", fun {opcode; name; args} ->
     let pat =
       make_pat name args in
     let body =
       sprintf "(0x%x,List.concat [%s])"
	 opcode
	 (call_args "byte" args) in
       sprintf "| %s -> %s" pat body
  end;

  begin "-compile",fun {name;args}->
    let pat =
      make_pat name args in
    let args' =
      if args = [] then
	""
      else
	sprintf "(%s)" @@ concat_mapi "," (sprintf "arg_%s ctx _%d") args in
      sprintf "| %s -> `%s %s" pat name args'
  end;

  begin "-pat",fun {name; args} ->
    let pat =
      make_pat name args in
      sprintf "| %s -> [%s]" pat @@
	call_args Sys.argv.(2) args
  end;

  begin "-extra",fun {name; args;extra} ->
    let pat =
      make_pat name args in
      try
	sprintf "| %s -> %s" pat @@ List.assoc Sys.argv.(2) extra
      with Not_found ->
	sprintf "| %s -> default" pat
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

