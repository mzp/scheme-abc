open Base
open Node

exception Unbound_var of string Node.t
exception Unbound_class of (string*string) Node.t
exception Unbound_method of string Node.t

type method_ = Ast.ident

type stmt =
    [ `ExternalClass of Ast.name * method_ list
    | `External of Ast.ident
    | Ast.stmt]

type 'a info = 'a * 'a Node.t
module VSet = Set.Make(struct
			 type t =  string Node.t
			 let compare {value=a} {value=b} = Pervasives.compare a b
		       end)

module CSet = Set.Make(struct
			 type t =  (string*string) Node.t
			 let compare {value=a} {value=b} = Pervasives.compare a b
		       end)
module MSet = VSet
type env = {
  var:   VSet.t;
  klass: CSet.t;
  meth:  MSet.t;
}

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

let empty = {
  var  = VSet.empty;
  klass= CSet.empty;
  meth = MSet.empty;
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

let rec unbound_expr : Ast.expr -> env =
  function
      `Bool _ | `Float _ | `Int _ | `String _ ->
	empty
    | `Var node ->
	{empty with
	   var = VSet.singleton (node)}
    | `Block xs | `Call xs ->
	union @@ List.map unbound_expr xs
    | `Let (decls,expr) ->
	let xs = 
	  union @@ List.map (unbound_expr$snd) decls in
	let vars =
	  List.map fst decls in
	let ys =
	  unbound_expr expr in
	  xs ++ (ys -- vars)
    | `LetRec (decls,expr) ->
	let xs =
	  union @@ List.map (unbound_expr$snd) decls in
	let vars =
	  List.map fst decls in
	let ys =
	  unbound_expr expr in
	  (xs ++ ys) -- vars
    | `If (a,b,c) ->
	union @@ List.map unbound_expr [a;b;c]
    | `Lambda (args,body) ->
	unbound_expr body -- args
    | `Invoke (name,meth,args) ->
	let { meth = meths } as env' = 
	  unbound_expr name ++ union (List.map unbound_expr args) in
	  {env' with
	     meth = MSet.add meth meths}
    | `New (klass,args) ->
	let {klass=klasses} as env' =
	  union @@ List.map unbound_expr args in
	  {env' with
	     klass = CSet.add klass klasses}
    | `SlotRef (obj,_) ->
	unbound_expr obj
    | `SlotSet (obj,_,value) ->
	unbound_expr obj ++ unbound_expr value

let unbound_stmt (stmt : stmt) env = 
  match stmt with
      `Expr expr ->
	unbound_expr expr ++ env
    | `Define (name,expr) ->
	unbound_expr expr -- [name]
    | `External name ->
	env -- [name]
    | `Class (name,super,_,methods) ->
	let (ms,envs) =
	  unzip_with 
	    (fun (name,args,expr) -> (name,unbound_expr expr -- args)) 
	    methods in
	let {meth=meths; klass=klasses} as env' =
	  union envs ++ env in
	  {env' with
	     meth  = List.fold_left (flip MSet.remove) meths ms;
	     klass = CSet.add super @@ 
	      CSet.remove {name with value=("",name.value)} klasses}
    | `ExternalClass (name,methods) ->
	  {env with
	     meth  = List.fold_left (flip MSet.remove) env.meth methods;
	     klass = CSet.remove name env.klass}


let trans_stmt (stmt : stmt) : Ast.stmt option =
  match stmt with
     `External _ | `ExternalClass _ ->
	None
    | #Ast.stmt as s ->
	Some s

let trans program =
    List.fold_right (fun s (stmt,env) ->
		       let env' =
			 unbound_stmt s env in
		       match trans_stmt s with
			   Some s' ->
			     ((s'::stmt),env')
			 | None ->
			     stmt,env')
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
