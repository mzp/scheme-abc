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
module MSet = Set.Make(struct
			 type t =  string Node.t
			 let compare {value=a} {value=b} = Pervasives.compare a b
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

let (++)  {var=v1; klass=k1; meth=m1} {var=v2; klass=k2; meth=m2} = {
  var   = VSet.union v1 v2;
  klass = CSet.union k1 k2;
  meth  = MSet.union m1 m2;
}

let (--) env xs = {
  env with
    var = List.fold_left (fun set x -> VSet.remove x set) env.var xs}

let union =
  List.fold_left (++) empty

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
	raise (Forbidden_var var)
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

let qname_of_stmt_name node =
  {node with value=("",node.value)}


let rec unzip_with f =
  function
      [] ->
	([],[])
    | x::xs ->
	let (x,y) =
	  f x in
	let (xs,ys) =
	  unzip_with f xs in
	  (x::xs,y::ys)

let unbound_stmt (stmt : stmt) env =
  match stmt with
      `Module _ ->
	failwith "not yet"
    | `Expr expr ->
	unbound_expr expr ++ env
    | `Define (name,expr) ->
	(env ++ unbound_expr expr) -- [qname_of_stmt_name name]
    | `External name ->
	failwith "not yet"
    | `Class (klass,super,_,methods) ->
	let (ms,envs) =
	  unzip_with
	    (fun (name,args,expr) ->
	       (name,
		unbound_expr expr -- (List.map (Node.lift (fun a->("",a))) args)))
	    methods in
	let {meth=meths; klass=klasses; var=vars} =
	  union envs ++ env in
	  {
	     meth  =
	      List.fold_left (flip MSet.remove) meths ms;
	     klass =
	      CSet.add super @@
		CSet.remove (qname_of_stmt_name klass) klasses;
	     var   =
	      VSet.remove (qname_of_stmt_name klass) vars (* class name is first class*)
	  }
    | `ExternalClass (name,methods) ->
(*	{
	  meth  = List.fold_left (flip MSet.remove) env.meth methods;
	  klass = CSet.remove name env.klass;
	  var   =
	    if fst name.value = "" then
	      VSet.remove name env.var
	    else
	      env.var
	}*)
	failwith "not yet"

let rec check_stmt exports env : stmt -> env =
  function
      `Module (name,exports,body) ->
	List.iter (ignore $ check_stmt exports env) body;
	env
    | _ ->
	failwith ""


let trans_stmt (stmt : stmt) : ModuleTrans.stmt list=
  match stmt with
     `External _ | `ExternalClass _ ->
       []
    | _ ->
	failwith "not yet"

let trans program =
    List.fold_right (fun s (stmt,env) ->
		       let env' =
			 unbound_stmt s env in
			 ((trans_stmt s)@stmt,env'))
    program
    ([],empty)

let format f min set =
  try
    let {Node.value = value} as elt =
      min set in
      [{elt with
	  Node.value=f value}]
  with _ ->
    []

let uncheck =
  HList.concat_map trans_stmt

let check (program : stmt list)=
  let program',env =
    trans program in
    if env = empty then
      program'
    else if env.var <> VSet.empty then
      raise (Unbound_var (VSet.min_elt env.var))
    else if env.klass <> CSet.empty then
      raise (Unbound_class (CSet.min_elt env.klass))
    else if env.meth <> MSet.empty then
      raise (Unbound_method (MSet.min_elt env.meth))
    else
      failwith "must not happen"

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
