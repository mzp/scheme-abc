open Base
open Node

type method_ = Ast.ident * Ast.ident list 

type stmt =
    [ `ExternalClass of Ast.ident * Ast.name * Ast.attr list * method_ list
    | `External of Ast.ident
    | Ast.stmt]

module SSet = Set.Make(struct
			 type t =  string * string Node.t
			 let compare (a,_) (b,_) = Pervasives.compare a b
		       end)
type env = {
  var:   SSet.t;
  klass: SSet.t;
  meth:  SSet.t;
}

let empty = {
  var  = SSet.empty;
  klass= SSet.empty;
  meth = SSet.empty;
}
let lift f {var=v1; klass=k1; meth=m1} {var=v2; klass=k2; meth=m2} = {
    var   = f v1 v2;
    klass = f k1 k2;
    meth  = f m1 m2;
}

let (++) =
  lift SSet.union

let (--) env xs = {
  env with
    var = List.fold_left 
    (fun set x -> SSet.remove (x.value,x) set) 
    env.var xs}

let union = 
  List.fold_left (++) empty

let rec unbound : Ast.expr -> env =
  function
      `Bool _ | `Float _ | `Int _ | `String _ ->
	empty
    | `Var node ->
	{empty with
	   var = SSet.singleton (node.value,node)}
    | `Block xs | `Call xs ->
	union @@ List.map unbound xs
    | `Let (decls,expr) ->
	let xs = 
	  union @@ List.map (unbound$snd) decls in
	let vars =
	  List.map fst decls in
	let ys =
	  unbound expr in
	  xs ++ (ys -- vars)
    | `LetRec (decls,expr) ->
	let xs =
	  union @@ List.map (unbound$snd) decls in
	let vars =
	  List.map fst decls in
	let ys =
	  unbound expr in
	  (xs ++ ys) -- vars
    | `If (a,b,c) ->
	union @@ List.map unbound [a;b;c]
    | `Lambda (args,body) ->
	unbound body -- args
    | `Invoke (name,meth,args) ->
	let { meth = m } as env' = 
	  unbound name ++ union (List.map unbound args) in
	  {env' with
	     meth = SSet.add (meth.value,meth) m}
    | `New (klass,args) ->
	let {klass=k} as env' =
	  union @@ List.map unbound args in
	let join (x,y) = 
	  x ^ ":"  ^ y in
	let klass' =
	  {klass with value = join klass.value} in
	  {env' with
	     klass = SSet.add (klass'.value, klass') k}
    | `SlotRef (obj,_) ->
	unbound obj
    | `SlotSet (obj,_,value) ->
	unbound obj ++ unbound value

(*
let unbound_stmt env : ClosTrans.stmt -> env = 
  `Define (name,expr) ->
    
of ident * expr
    | `Expr of expr
    | `Class of ident * name * attr list * method_ list ]



    | `DefineClass  of ident * Ast.name * ident list
    | `DefineMethod of ident * (ident * ident) * ident list * Ast.expr]
*)
