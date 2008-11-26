open Base
open Node

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
  

type method_ = Ast.ident * Ast.ident list 

type stmt =
    [ `ExternalClass of Ast.ident * Ast.name * Ast.attr list * method_ list
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
	     klass = CSet.remove {name with value=("",name.value)} klasses}
    | `ExternalClass (name,super,_,methods) ->
	let ms =
	  List.map fst methods in
	  {env with
	     meth  = List.fold_left (flip MSet.remove) env.meth ms;
	     klass = CSet.remove {name with value=("",name.value)} env.klass}

let unbound program =
  if List.fold_right unbound_stmt program empty = empty then
    Val true
  else
    Error false
