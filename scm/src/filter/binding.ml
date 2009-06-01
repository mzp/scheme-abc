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

let is_access {vars=vars; current=current; table=table} var =
  match (maybe @@ List.assoc var.value) vars with
      Some Public | Some Local ->
	true
    | Some Internal when fst var.value = current ->
	true
    | Some Internal ->
	raise (Forbidden_var var)
    | None ->
	table#mem_symbol var.value

let bind_qname env ({ value = (ns,name)} as var) =
  env.current
  +> HList.scanl (fun x y -> x @ [y] ) []
  +> List.map (fun ns' -> {var with value=(ns' @ ns, name)})
  +> maybe (List.find (is_access env))
  +> function Some c -> c | None -> raise (Unbound_var var)

let bind_expr env expr  =
  Ast.fix_fold Ast.fold
    begin fun env -> function
       | `Let (decls,_) | `LetRec (decls,_) ->
	   add_local (List.map fst decls) env
       | `Lambda (args,_) ->
	   add_local args env
       | #Ast.expr ->
	   env end
    begin fun env -> function
       | `Var var ->
	   `Var (bind_qname env var)
       | `New (c,args) ->
	   `New (bind_qname env c,args)
       | `Invoke (_,meth,_) as expr ->
	   if not( MSet.mem meth env.meths ) &&
  	     not( env.table#mem_method meth.value ) then
	       raise (Unbound_method meth)
	   else
	     expr
       | #Ast.expr as e ->
	   e end
    env expr

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

let rec bind_stmt exports env  stmt =
  open Ast in
  match stmt with
      `Module ({module_name = {Node.value=name};
		exports = exports;
		stmts   = stmts} as m) ->
	let env' =
	  {env with current =
	      env.current @ [name] } in
	let env'', stmts' =
	  map_accum_left (bind_stmt exports) env' stmts in
	  ({env'' with current = env.current},
	   `Module {m with stmts = stmts'})
    | `Expr expr ->
	env, `Expr (bind_expr env expr)
    | `Define ({Node.value=name} as node, expr) ->
	let env' =
	  add_var name exports env in
	  env', `Define(node,bind_expr env' expr)
    | `Class ({class_name={Node.value=klass};
	       super=super;
	       methods=methods} as c) ->
	let env' =
	  add_methods (List.map
			 (function {method_name=`Public m} |  {method_name=`Static m} ->
			    m) methods) @@
	    add_var klass exports env in
	let methods' =
	  List.map (fun m ->
		      {m with body = bind_expr (add_local m.args env') m.body})
	    methods in
	  env',`Class {c with
			 super = bind_qname env super;
			 methods   = methods'}
    | `Open _ as s ->
	(* fixme *)
	env,s

let bind table program =
  let env = {
    meths   = MSet.empty;
    vars    = [];
    current = [];
    table   = (table :> table) } in
    snd @@ map_accum_left (bind_stmt `All) env program


