open Base
open Cpool
open Asm
open Node

type bind = Scope of int | Register of int | Global
type name = Package of string * string | Local of string
type env  = {depth:int; binding: (name * bind) list }

let get_bind name {binding=xs} =
  List.assoc name xs

let get_bind_sure name state =
  try
    Some (get_bind name state)
  with Not_found ->
    None

let is_bind name {binding=xs} =
  List.mem_assoc name xs

let empty_env =
  {depth=0; binding=[Local "this",Register 0]}

let script_bootstrap _ =
  [ GetLocal_0; PushScope ],{depth=1; binding=[]}

let arguments_with base args f =
  let b =
    ExtList.List.mapi (fun i {value = arg}->
			 (Local arg,Register (i+base))) args in
  let args' =
    List.map (const 0) @@ HList.drop (1-base) args in
  let code =
    f ({empty_env with binding = b }) args' in
    code

let arguments args f =
  arguments_with 1 args f

let arguments_self args f =
  arguments_with 0 args f

let let_env {depth=n; binding=binding} vars =
  {depth  = n+1;
   binding=
      List.map (fun ({value = var},_) ->
		  (Local var,Scope n)) vars @ binding}

let let_scope env vars f =
  let env' =
    let_env env vars in
    List.concat [HList.concat_map
		   (fun ({value = var},init)->
		      List.concat [[PushString var]; init]) vars;
		 [NewObject (List.length vars);
		  PushWith];
		 f env';
		 [PopScope]]

let let_rec_scope ({depth=n} as env) vars f =
  let env' =
    let_env env vars in
  let init =
    HList.concat_map
      (fun ({value = var},g)->
	 List.concat [[Dup];
		      g env';
		      [SetProperty (make_qname var)]]) vars in
    List.concat [[NewObject 0;Dup;PushWith];
		 init;
		 [Pop];
		 f env';
		 [PopScope]]

let stmt_name : Ast.stmt_name -> name * Cpool.multiname=
  function
      `Public {Node.value=(ns,name)} ->
	Package (ns,name),QName (Namespace ns,name)
    | `Internal {Node.value=(ns,name)} ->
	Package (ns,name),QName (PackageInternalNamespace ns,name)

let qname_of_stmt_name =
  snd $ stmt_name

let define_scope name ({depth=n;binding=xs} as env) f =
  let bind,qname =
    stmt_name name in
  let env' =
    {depth=n; binding=(bind,Scope (n-1))::xs} in
  let body' =
    if is_bind bind env then
      List.concat [
	[NewObject 0;PushWith];
	f env';
	[GetScopeObject n;
	 Swap;
	 SetProperty qname]]
    else
      List.concat [
	f env';
	[GetScopeObject (n-1);
	 Swap;
	 SetProperty qname]] in
    env',body'

let define_class name ({sname=super} as klass) env =
  let bind,qname =
    stmt_name name in
  let env' =
    {env with binding=(bind,Global)::env.binding} in
  env',[
    (* init class *)
    GetLex super;
    PushScope;
    GetLex super;
    NewClass {klass with cname = qname};
    PopScope;

    (* add to scope *)
    GetGlobalScope;
    Swap;
    InitProperty qname]

let var_ref (ns,name) env =
  let bind =
    if ns = "" then
      Local name
    else
      Package (ns,name) in
  let qname =
    QName ((Namespace ns),name) in
    match get_bind_sure bind env with
	Some (Scope scope) ->
	  [GetScopeObject scope;
	   GetProperty qname]
      | Some (Register n) ->
	  [GetLocal n]
      | Some Global ->
	  [GetGlobalScope;
	   GetProperty qname]
      | None ->
	  [GetLex qname]

let var_call (ns,name) args env =
  let bind =
    if ns = "" then
      Local name
    else
      Package (ns,name) in
  let qname =
    QName ((Namespace ns),name) in
  let nargs =
    List.length args in
    match get_bind_sure bind env with
	Some (Scope scope) ->
	  List.concat [[GetScopeObject scope];
		       List.concat args;
		       [CallPropLex (qname,nargs)]]
      | Some (Register n) ->
	  List.concat [[GetLocal n;
			GetGlobalScope];
		       List.concat args;
		       [Asm.Call nargs]]
      | Some Global ->
	  List.concat [[GetGlobalScope];
		       List.concat args;
		       [CallPropLex (qname,nargs)]]
      | None ->
	  List.concat [[FindPropStrict qname];
		       List.concat args;
		       [CallPropLex (qname,nargs)]]
