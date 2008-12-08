open Base
open Cpool
open Asm
open Node

type bind = Scope of int | Register of int | Global
type env  = {depth:int; binding: (string * bind) list }

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
  {depth=0; binding=[("this",Register 0)]}

let script_bootstrap _ =
  [ GetLocal_0; PushScope ],{depth=1; binding=[]}

let arguments args f =
  let b =
    ExtList.List.mapi (fun i {value = arg}-> (arg,Register (i+1))) args in
  let args' =
    List.map (const 0) args in
  let code =
    f ({empty_env with binding = b }) args' in
    code

let arguments_self args f =
  let b =
    ExtList.List.mapi (fun i {value = arg}-> (arg,Register i)) args in
  let args' =
    List.map (const 0) (List.tl args) in
  let code =
    f ({empty_env with binding = b }) args' in
    code

let let_scope {depth=n; binding=binding} vars f =
  let env' =
    {depth  = n+1;
     binding= List.map (fun ({value = var},_) -> (var,Scope n)) vars @ binding} in
    List.concat [HList.concat_map 
		   (fun ({value = var},init)-> 
		      List.concat [[PushString var]; init]) vars;
		 [NewObject (List.length vars);
		  PushWith];
		 f env';
		 [PopScope]]

let let_rec_scope {depth=n; binding=binding} vars f =
  let env' =
    {depth  = n+1;
     binding= List.map (fun ({value = var},_) -> (var,Scope n)) vars @ binding } in
  let init = 
    HList.concat_map 
      (fun ({value = var},g)->
	 List.concat [[GetScopeObject n];
		      g env';
		      [SetProperty (make_qname var)]]) vars in
    List.concat [[NewObject 0;PushWith];
		 init;
		 f env';
		 [PopScope]]

let define_scope name ({depth=n;binding=xs} as env) f =
  let env' =
    {depth=n; binding=(name,Scope (n-1))::xs} in
  let body' =
    if is_bind name env then
      List.concat [
	[NewObject 0;PushWith];
	f env';
	[GetScopeObject n;
	 Swap;
	 SetProperty (make_qname name)]]
    else
      List.concat [
	f env';
	[GetScopeObject (n-1);
	 Swap;
	 SetProperty (make_qname name)]] in
    env',body'

let define_class name ({sname=super; cname=cname} as klass) env =
  let env' = 
    {env with binding=(name,Global)::env.binding} in
  env',[
    (* init class *)
    GetLex super;
    PushScope;
    GetLex super;
    NewClass klass;
    PopScope;

    (* add to scope *)
    GetGlobalScope;
    Swap;
    InitProperty cname]

let var_ref var env =
  let qname = 
    make_qname var in
    match get_bind_sure var env with
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

let var_call var args env =
  let qname =
    make_qname var in
  let nargs =
    List.length args in
    match get_bind_sure var env with
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
