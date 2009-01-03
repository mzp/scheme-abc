open Base
open Node

(** exceptions *)
exception Unbound_var of (string*string) Node.t
exception Forbidden_var of (string*string) Node.t
exception Unbound_method of string Node.t

(** ast types *)
type 'stmt stmt_type =
    [ `ExternalClass of Ast.sname * Ast.sname list
    | `External of Ast.sname
    | 'stmt ModuleTrans.stmt_type]

type stmt =
    stmt stmt_type

type program = stmt list

(** environments *)
(* method set *)
module MSet = Set.Make(
  struct
    type t =
	string Node.t
    let compare {value=a} {value=b} =
      Pervasives.compare a b
  end)

type access = Public | Internal | Local
type qname  = string * string
type env = {
  meths   : MSet.t;
  current : string;
  vars    : (qname*access) list
}

let empty = {
  meths  = MSet.empty;
  vars   = [];
  current = "";
}

let name_node {Node.value = name}=
  ("",name)

let add_local xs env =
  let vars =
    List.map (fun x -> (name_node x,Local)) xs in
    { env with
	vars = vars @ env.vars }

let check_access {vars=vars; current=current} var =
  match (maybe @@ List.assoc var.value) vars with
      Some Public | Some Local ->
	()
    | Some Internal when fst var.value = current ->
	()
    | Some Internal ->
	raise (Forbidden_var var)
    | None ->
	raise (Unbound_var var)

let rec check_expr env : Ast.expr -> unit =
  function
      `Bool _ | `Float _ | `Int _ | `String _ ->
	()
    | `Var var ->
	check_access env var
    | `New (klass,args) ->
	check_access env klass;
	List.iter (check_expr env) args
    | `Let (decls,body) ->
	let env' =
	  add_local (List.map fst decls) env in
	  List.iter (fun (_,init)-> check_expr env init) decls; (* use env, not env' *)
	  check_expr env' body
    | `LetRec (decls,body) ->
	let env' =
	  add_local (List.map fst decls) env in
	  List.iter (fun (_,init)-> check_expr env' init) decls; (* use env', not env *)
	  check_expr env' body
    | `Lambda (args,body) ->
	let env' =
	  add_local args env in
	  check_expr env' body
    | `Block xs | `Call xs ->
	List.iter (check_expr env) xs
    | `If (a,b,c) ->
	check_expr env a;
	check_expr env b;
	check_expr env c
    | `Invoke (name,meth,args) ->
	check_expr env name;
	if not(MSet.mem meth env.meths) then
	  raise (Unbound_method meth);
	List.iter (check_expr env) args
    | `SlotRef (obj,_) ->
	check_expr env obj
    | `SlotSet (obj,_,value) ->
	check_expr env obj;
	check_expr env value

let add_var var exports env =
  let access =
    match exports with
	ModuleTrans.All ->
	  Public
      | ModuleTrans.Restrict vars when List.exists
	  (fun {Node.value=name} -> var = name) vars ->
	  Public
      | _ ->
	  Internal in
    {env with
       vars = ((env.current,var),access)::env.vars}

let add_methods methods env =
  {env with
     meths = List.fold_left (flip MSet.add) env.meths methods}

let rec check_stmt exports env : stmt -> env =
  function
      `Module ({Node.value=name},exports,body) ->
	let env' =
	  {env with current =
	      if env.current = "" then
		name
	      else
		env.current ^ "." ^ name} in
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
    | `External {Node.value=name} ->
	add_var name exports env
    | `ExternalClass ({Node.value=klass},methods) ->
	add_methods methods @@ add_var klass exports env
    | `Class ({Node.value=klass},super,_,methods) ->
	check_access env super;
	let env' =
	  add_methods (List.map (fun (m,_,_) -> m) methods) @@
	    add_var klass exports env in
	  List.iter (fun (_,args,body)->
		       check_expr (add_local args env') body)
	    methods;
	  env'

let rec remove_external : stmt -> ModuleTrans.stmt list =
  function
      `External _ | `ExternalClass _ ->
	[]
    | `Module (name,exports,stmts) ->
	[`Module (name,exports,HList.concat_map remove_external stmts)]
    | `Class _ | `Define _ | `Expr _ as s ->
	[s]

let uncheck =
  HList.concat_map remove_external

let check program =
  ignore @@
    List.fold_left (check_stmt ModuleTrans.All) empty program;
  HList.concat_map remove_external program

let rec lift f : stmt -> stmt =
  function
      `Class (klass,super,attrs,methods) ->
	let methods' =
	  List.map (Tuple.T3.map3 f) methods in
	  `Class (klass,super,attrs,methods')
    | `Define (name,body) ->
	`Define (name,f body)
    | `Expr expr ->
	`Expr (f expr)
    | `Module (name,exports,stmts) ->
	`Module (name,exports,List.map (lift f) stmts)
    | `External _ | `ExternalClass _ as s ->
	s
