open Base
open Node

(* ------------------------------
   exceptions
   ------------------------------ *)
exception Unbound_var    of (string list * string) Node.t
exception Forbidden_var  of (string list * string) Node.t
exception Unbound_method of string Node.t

(* ------------------------------
   environments
   ------------------------------ *)
module MSet = Set.Make(
  struct
    type t =
	string Node.t
    let compare {value=a} {value=b} =
      Pervasives.compare a b
  end)

type access = Public | Internal | Local
type qname  = string list * string

class type table = object
  method mem_symbol : qname -> bool
  method mem_method : string -> bool
end

type env = {
  meths   : MSet.t;
  current : string list;
  vars    : (qname*access) list;
  table  : table
}

(* ------------------------------
   Binding check
   ------------------------------ *)
let name_node {Node.value = name}=
  ([],name)

let add_local xs env =
  let vars =
    List.map (fun x -> (name_node x,Local)) xs in
    { env with
	vars = vars @ env.vars }

let check_access {vars=vars; current=current; table=table} var =
  match (maybe @@ List.assoc var.value) vars with
      Some Public | Some Local ->
	()
    | Some Internal when fst var.value = current ->
	()
    | Some Internal ->
	raise (Forbidden_var var)
    | None ->
	let qname =
	  match var.value with
	      ([],sname) ->
		(["std"],sname)
	    | qname ->
		qname in
	  if table#mem_symbol qname then
	    ()
	  else
	    raise (Unbound_var var)

let check_expr env expr  =
  ignore @@ Ast.fix_fold Ast.fold
    (fun env expr ->
       match expr with
	 | `Var var ->
	     check_access env var;
	     env
	 | `New (klass,_) ->
	     check_access env klass;
	     env
	 | `Let (decls,_) | `LetRec (decls,_) ->
	     add_local (List.map fst decls) env
	 | `Lambda (args,_) ->
	     add_local args env
	 | `Invoke (_,meth,_) ->
	     if not(MSet.mem meth env.meths) &&
	       not(env.table#mem_method meth.value) then
		 raise (Unbound_method meth);
	     env
	 | #Ast.expr ->
	     env)
    const env expr

let add_var var exports env =
  let access =
    match exports with
	`All ->
	  Public
      | `Only vars when List.exists
	  (fun {Node.value=name} -> var = name) vars ->
	  Public
      | _ ->
	  Internal in
    {env with
       vars = ((env.current,var),access)::env.vars}

let add_methods methods env =
  {env with
     meths = List.fold_left (flip MSet.add) env.meths methods}

let rec check_stmt exports env  =
  function
      `Module {Ast.module_name = {Node.value=name};
	       exports = exports;
	       stmts   = body} ->
	let env' =
	  {env with current =
	      env.current @ [name] } in
	let env'' =
	  List.fold_left (check_stmt exports) env' body in
	  {env'' with current = env.current}
    | `Expr expr ->
	check_expr env expr;
	env
    | `Define ({Node.value=name},expr) ->
	let env' =
	  add_var name exports env in
	  check_expr env' expr;
	  env'
    | `Class {Ast.class_name={Node.value=klass};
	      super=super;
	      methods=methods} ->
	check_access env super;
	let env' =
	  add_methods (List.map
			 (function {Ast.method_name=`Public m} |  {Ast.method_name=`Static m} ->
			    m) methods) @@
	    add_var klass exports env in
	  List.iter (fun {Ast.args=args; body=body} ->
		       check_expr (add_local args env') body)
	    methods;
	  env'

let check table program =
  let env = {
    meths = MSet.empty;
    vars  = [];
    current = [];
    table = table } in
    ignore @@
      List.fold_left (check_stmt `All) env program;
    program

