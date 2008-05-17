open Str

type decl = {
  name:string;
  args:string list;
  body:string
}

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

let type_of_decl {name=name;args=args} =
  if args = [] then
    Printf.sprintf "| %s" name
  else
    Printf.sprintf "| %s of %s" name (String.concat "*" args)
  
let mapi f xs =
  let rec sub f n =
    function
	[] -> []
      | x::xs -> (f n x)::sub f (n+1) xs in
    sub f 0 xs

let clause_of_decl {name=name;args=args;body=body} =
  let args' =
    if args = [] then
      ""
    else
      Printf.sprintf "(%s)" (String.concat "," (mapi (fun n _ -> Printf.sprintf "arg%d" n) args)) in
    Printf.sprintf "| %s %s -> {default with %s}" name args' body

let output_types decls =
  let inst = 
    Printf.sprintf "type instruction =\n%s" (String.concat "\n" (List.map type_of_decl decls)) in
    Printf.printf "module type S = sig\n %s \nend\n\n module B = struct\n %s \nend\n" inst inst

let output_inf decls =
  let inst = 
    Printf.sprintf "type instruction =\n%s" (String.concat "\n" (List.map type_of_decl decls)) in
    Printf.printf "module type S = sig\n %s \nend\n\nmodule B : S\n" inst

let output_match decls =
  let func = 
    (String.concat "\n" (List.map clause_of_decl decls)) in
    Printf.printf "type config = {
  op:int;
  args: Cpool.cmap -> Bytes.t list;
  const:  Cpool.clist;
  stack: int;
  scope: int;
  count: int;
}

let const x _ = x
let default = {
  op=0;
  args=const [];
  const= Cpool.empty;
  stack=0;
  scope=0;
  count=0;
}
open Opcode.B
open Cpool
let get_config = function
%s\n" func


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
	List.sort (fun {name=a} {name=b} -> compare a b) !decls in
	if Sys.argv.(1) = "-t" then
	  output_types decls'
	else if Sys.argv.(1) = "-i" then
	  output_inf decls'
	else 
	  output_match decls'

let _ = if not !Sys.interactive then
  f ()
