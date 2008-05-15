open Str

type decl = {
  name:string;
  args:string list;
  body:string
}

let parse s =
  let [decl;body] =
    bounded_split (regexp " *-> *") s 2 in
    match bounded_split (regexp " *of *") decl 2 with
	[name] -> {name=name;args=[]; body=body}
      | [name;args] -> {name=name;args=split (regexp " *\* *") args; body=body}

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
    String.concat " " (mapi (fun n _ -> Printf.sprintf "args%d" n) args) in
    Printf.sprintf "| %s %s -> {default with %s}" name args' body

let output f name first xs =
  let ch = 
    open_out name in
    output_string ch first;
    output_string ch "\n";
    List.iter (fun d-> 
		 output_string ch (f d);
		 output_string ch "\n") xs;
    close_out ch

let f _ =
  let decls = 
    ref [] in
    try
      while true do
	decls := (parse (read_line ()))::!decls
      done
    with End_of_file ->
      let decls' =
	List.sort (fun {name=a} {name=b} -> compare a b) !decls in
	output type_of_decl "types.ml" "type instruction = " decls';
	output clause_of_decl "match.ml" "let get_config = function" decls'

let _ = if not !Sys.interactive then
  f ()
