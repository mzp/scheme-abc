open Base
open Node

exception Unbound_var of (string*string) Node.t
exception Unbound_class of (string*string) Node.t
exception Unbound_method of string Node.t

type 'stmt stmt_type =
    [ `ExternalClass of Ast.sname * Ast.sname list
    | `External of Ast.sname
    | 'stmt ModuleTrans.stmt_type]

type stmt =
    stmt stmt_type


type program = stmt list

type 'a info = 'a * 'a Node.t
module VSet = Set.Make(struct
			 type t =  (string*string) Node.t
			 let compare {value=a} {value=b} = Pervasives.compare a b
		       end)

module CSet = Set.Make(struct
			 type t =  (string*string) Node.t
			 let compare {value=a} {value=b} = Pervasives.compare a b
		       end)
module MSet = Set.Make(struct
			 type t =  string Node.t
			 let compare {value=a} {value=b} = Pervasives.compare a b
		       end)
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
	  List.map (fun (x,_)->Node.lift (fun y->("",y)) x) decls in
	let ys =
	  unbound_expr expr in
	  xs ++ (ys -- vars)
    | `LetRec (decls,expr) ->
	let xs =
	  union @@ List.map (unbound_expr$snd) decls in
	let vars =
	  List.map (fun (x,_)->Node.lift (fun y->("",y)) x) decls in
	let ys =
	  unbound_expr expr in
	  (xs ++ ys) -- vars
    | `If (a,b,c) ->
	union @@ List.map unbound_expr [a;b;c]
    | `Lambda (args,body) ->
	unbound_expr body -- (List.map (Node.lift (fun x->("",x))) args)
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

let qname_of_stmt_name node =
  {node with value=("",node.value)}

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
