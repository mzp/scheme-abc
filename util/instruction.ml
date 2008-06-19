open Str

type decl = {
  name:string;
  args:string list;
  body:string
}

let mapi f xs =
  let rec sub f n =
    function
	[] -> []
      | x::xs -> (f n x)::sub f (n+1) xs in
    sub f 0 xs

(* parsing *)
let parse s =
  if string_match (regexp "^#\\|^$") s 0  then
    None
  else
    match bounded_split (regexp " *: *") s 2 with
	[decl;body] ->
	  begin match bounded_split (regexp " *of *") decl 2 with
	      [name] -> Some {name=name;args=[]; body=body}
	    | [name;args] -> Some {name=name;args=split (regexp " *\\* *") args; body=body}
	    | _ -> failwith ("invalid decl format:"^decl)
	  end
      | _ ->
	  failwith ("invalid file format: "^s)

(* 
   output type decl

   Example:
   | PushInt  of int
   | Pop
   ...
*)
let type_of_decl {name=name;args=args} =
  if args = [] then
    Printf.sprintf "| %s" name
  else
    Printf.sprintf "| %s of %s" name (String.concat "*" args)
  
let output_types decls =
  print_endline (String.concat "\n" (List.map type_of_decl decls))

(*
  output match clause

  Example:
  let get_config = function
  | Dup  -> {default with op=0x2a; stack= 2}
  | NewActivation  -> {default with op=0x57; stack=1}
  | NewArray (arg0) -> {default with op=0x56; args=const [Bytes.u30 arg0]}
  ...
*)
let clause_of_decl {name=name;args=args;body=body} =
  let args' =
    if args = [] then
      ""
    else
      Printf.sprintf "(%s)" (String.concat "," (mapi (fun n _ -> Printf.sprintf "arg%d" n) args)) in
    Printf.sprintf "| %s %s -> {default with %s}" name args' body

let output_match decls =
  let func = 
    (String.concat "\n" (List.map clause_of_decl decls)) in
    Printf.printf "let get_config = function%s\n" func

(* output string function
let string_of_instruction = function
  | Dup  -> "Dup(" ^ ")"
  | NewActivation  -> "NewActivation(" ^ ")"
  | NewArray (arg0) -> "NewArray(" ^ (Std.dump arg0) ^ ")"
*)
let clause_of_output {name=name;args=args} =
  let args' =
    if args = [] then
      ""
    else
      Printf.sprintf "(%s)" (String.concat "," (mapi (fun n _ -> Printf.sprintf "arg%d" n) args)) in
  let prefix =
    Printf.sprintf "| %s %s -> \"%s(\"" name args' name in
  let mid =
    mapi (fun i _ -> Printf.sprintf "(Std.dump arg%d)" i) args in
  let postfix =
    "\")\"" in
    String.concat " ^ " ([prefix]@mid@[postfix])

let output_string decls =
  let func = 
    (String.concat "\n" (List.map clause_of_output decls)) in
    Printf.printf "let string_of_instruction = function%s\n" func


let f _ =
  let decls = 
    ref [] in
    try
      while true do
	match parse (read_line ()) with
	    Some x ->
	      decls := x::!decls
	  | _ ->
	      ()
      done
    with End_of_file ->
      let decls' =
	!decls in
	match Sys.argv.(1) with
	    "-t" ->
	      output_types decls'
	  | "-m" ->
	      output_match decls'
	  | "-s" ->
	      output_string decls'
	  | _ ->
	      failwith "invalid option"


let _ = if not !Sys.interactive then
  f ()
