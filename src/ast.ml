open Base
open Asm

(** expression has no side-effect. *)
type expr = 
    Int of int
  | String of string
  | Bool   of bool
  | Float  of float
  | Var    of string
  | Lambda of string list * expr
  | Call   of expr list
  | If     of expr * expr * expr
  | Let    of (string*expr) list * expr
  | LetRec of (string*expr) list * expr
  | Block  of expr list

(** statement has side-effect *)
type stmt = 
  | Define of string * expr
  | Expr of expr

type program = stmt list

(**{6 Ast}*)
module StringOrder = struct
  type t = string
  let compare x y = compare x y
end

module StringSet = Set.Make(StringOrder)

let set_of_list xs =
  List.fold_left (flip StringSet.add) StringSet.empty xs
  
let union xs =
  List.fold_left StringSet.union StringSet.empty xs

let rec free_variable =
  function
      Lambda (args,expr) ->
	StringSet.diff (free_variable expr) (set_of_list args)
    | Let (decl,expr) ->
	let xs = 
	  union @@ List.map (free_variable$snd) decl in
	let vars =
	  set_of_list @@ List.map fst decl in
	let ys =
	  StringSet.diff (free_variable expr) vars in
	  StringSet.union xs ys
    | LetRec (decl,expr) ->
	let xs =
	  union @@ List.map (free_variable$snd) decl in
	let vars =
	  set_of_list @@ List.map fst decl in
	let ys =
	  free_variable expr in
	  StringSet.diff (StringSet.union xs ys) vars
    | Var x ->
	StringSet.singleton x
    | Call args ->
	union @@ List.map free_variable args
    | If (cond,seq,alt) ->
	union [
	  free_variable cond;
	  free_variable seq;
	  free_variable alt;
	]
    | Block xs ->
	union @@ List.map free_variable xs
    | _ ->
	StringSet.empty

let rec closure_fv =
  function
      Lambda (_,body) as exp ->
	free_variable exp
    | Call args ->
	union @@ List.map closure_fv args
    | If (a,b,c) ->
	union [
	  closure_fv a;
	  closure_fv b;
	  closure_fv c]
    | Let (decls,body) | LetRec (decls,body) ->
	let vars =
	  set_of_list @@ List.map fst decls in
	  StringSet.diff (closure_fv body) vars
    | Block exprs ->
	union @@ List.map closure_fv exprs
    | _ ->
	StringSet.empty

(** {6 Environment function} *)
type bind = Scope of int  | Register of int
type env  = {depth:int; binding: (string * bind) list }

let empty_env =
  {depth=0; binding=[]}

let add_scope names {depth=n;binding=xs} =
  let names' =
    List.map (fun name-> name,Scope n) names in
    {depth=n+1; binding=names' @ xs}

let add_current_scope name {depth=n;binding=xs} =
    {depth=n; binding=(name,Scope (n-1))::xs}

let add_register names env =
  let names' = 
    ExtList.List.mapi (fun i name-> name,Register (i+1)) names in
    {env with binding = names'@env.binding}

let get_bind name {binding=xs} =
  List.assoc name xs

let get_bind_sure name state =
  try
    Some (get_bind name state)
  with Not_found ->
    None

let is_bind name {binding=xs} =
  List.mem_assoc name xs

let ensure_scope name env =
  match get_bind name env with
      Scope x -> 
	x
    | _ ->
	failwith ("scope not found:"^name)

(** {6 Method generation} *)
let make_qname x = 
  Cpool.QName ((Cpool.Namespace ""),x)

let make_meth ?(args=[]) name body = 
  let inst =
      body @
      [ReturnValue] in
  { name  = name;
    params= args;
    return=0;
    flags =0;
    exceptions=[];
    traits=[];
    instructions=inst}

(** {6 Builtin operator } *)
let builtin = ["+",(Add_i,2);
	       "-",(Subtract_i,2);
	       "*",(Multiply_i,2);
	       "+.",(Add,2);
	       "-.",(Subtract,2);
	       "*.",(Multiply,2);
	       "/",(Divide,2);
	       "=",(Equals,2);
	       ">",(GreaterThan,2);
	       ">=",(GreaterEquals,2);
	       "<",(LessThan,2);
	       "<=",(LessEquals,2);]

let is_builtin name args =
  try
    let _,n =
      List.assoc name builtin in
      n = List.length args
  with Not_found ->
    false


(** {6 Asm code generation} *)
let rec generate_expr expr env = 
  let gen e =
    generate_expr e env in
  match expr with
    | Bool b ->
	if b then
	  [PushTrue]
	else
	  [PushFalse]
    | Float v->
	[PushDouble v]
    | String str -> [PushString str]
    | Int n when 0 <= n && n <= 0xFF -> [PushByte n]
    | Int n      -> [PushInt n]
    | Block xs   -> List.concat @@ interperse [Pop] @@ (List.map gen xs)
    | Lambda (args,body) ->
	let fv =
	  StringSet.elements @@ StringSet.inter (set_of_list args) (closure_fv body) in
	let wrap =
	  if fv = [] then
	    body
	  else
	    Let (List.map (fun x->x,Var x) fv,body) in
	let env' =
	  add_register args empty_env in
	let args' =
	  List.map (const 0) args in
	let m =
	  make_meth ~args:args' "" @@ generate_expr wrap env' in
	  [NewFunction m]
    | Var name ->
	let qname = 
	  make_qname name in
	  begin match get_bind_sure name env with
	      Some (Scope scope) ->
		[GetScopeObject scope;
		 GetProperty qname]
	    | Some (Register n) ->
		[GetLocal n]
	    | _ ->
		[GetLex qname]
	  end
    | Let (vars,body) ->
	let env' =
	  add_scope (List.map fst vars) env in
	let inits =
	  concat_map (fun (name,init)-> 
		       List.concat [[PushString name];gen init]) vars in
	  List.concat [inits;
		       [NewObject (List.length vars);
			PushWith];
		       generate_expr body env';
		       [PopScope]]
    | LetRec (vars,body) ->
	let env' =
	  add_scope (List.map fst vars) env in
	let init = 
	  concat_map (fun (name,init)->
			List.concat [[GetScopeObject (ensure_scope name env')];
				     gen init;
				     [SetProperty (make_qname name)]])
	    vars in
	  List.concat [[NewObject 0;PushWith];
		       init;
		       generate_expr body env';
		       [PopScope]]
    | Call (Var name::args) when is_builtin name args ->
	let inst,_ =
	  List.assoc name builtin in
	  List.concat [
	    concat_map gen args;
	    [inst]]
    | Call (Var name::args) ->
	let qname =
	  make_qname name in
	let nargs =
	  List.length args in
	let args' =
	  concat_map gen args; in
	  begin match get_bind_sure name env with
	      Some (Scope scope) ->
		List.concat [[GetScopeObject scope];
			     args';
			     [CallPropLex (make_qname name,nargs)]]
	    | Some (Register n) ->
		List.concat [[GetLocal n;
			      GetGlobalScope];
			     args';
			     [Asm.Call nargs]]
	    | _ ->
		List.concat [[FindPropStrict qname];
			     args';
			     [CallPropLex (qname,nargs)]] end
    | Call (name::args) ->
	let nargs =
	  List.length args in
	  List.concat [gen name;
		       [GetGlobalScope];
		       concat_map gen args;
		       [Asm.Call nargs]]
    | Call [] ->
	failwith "must not happen"
    | If (cond,cons,alt) ->
	let l_alt =
	  Label.make () in
	let l_if = 
	  Label.make () in
	let prefix = List.concat @@ match cond with
	    Call [Var "=";a;b] ->
	      [gen a;gen b;[IfNe l_alt]]
	  | Call [Var ">";a;b] ->
	      [gen a;gen b;[IfNgt l_alt]]
	  | Call [Var ">=";a;b] ->
	      [gen a;gen b;[IfNge l_alt]]
	  | Call [Var "<";a;b] ->
	      [gen a;gen b;[IfNlt l_alt]]
	  | Call [Var "<=";a;b] ->
	      [gen a;gen b;[IfNle l_alt]]
	  | _ ->
	      [gen cond;[IfFalse l_alt]] in
	  List.concat [prefix;
		       gen cons;
		       [Jump l_if;Label l_alt];
		       gen alt;
		       [Label l_if]]

let generate_stmt env stmt =
  match stmt with
      Expr expr -> 
	env,generate_expr expr env
    | Define (name,body) when not @@ is_bind name env ->
	let env' = 
	  add_current_scope name env in
	let scope = 
	  ensure_scope name env' in
	let body' =
	  List.concat [generate_expr body env';
		       [GetScopeObject scope;
			Swap;
			SetProperty (make_qname name)]] in
	  env',body'
    | Define (name,body) ->
	let env' = 
	  add_scope [name] env in
	let scope = 
	  ensure_scope name env' in
	let body' =
	  List.concat [[NewObject 0;PushWith];
		       generate_expr body env';
		       [GetScopeObject scope;
			Swap;
			SetProperty (make_qname name)]] in
	  env',body'

let generate_program xs env =
  List.concat @@ snd @@ map_accum_left generate_stmt env xs

let generate_method xs =
  let init_env =
    add_scope ["this"] empty_env in
  let program =
    generate_program xs init_env in
    make_meth "" ([GetLocal_0;PushScope] @ program)

let generate program =
  let m = 
    generate_method program in
  let cpool,info,body =
    assemble m in
    { Abc.cpool=cpool;
      Abc.method_info=info;
      Abc.method_body=body;
      Abc.metadata=[]; Abc.classes=[]; Abc.instances=[];
      Abc.script=[{Abc.init=0; trait_s=[] }] }
