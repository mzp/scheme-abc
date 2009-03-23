open Base
open Ast
open Asm
open Node
open Cpool

module V = VarResolve

let qname_of_stmt_name : Ast.stmt_name -> Cpool.multiname=
  function
      `Public {Node.value=(ns,name)} ->
	QName (Namespace ns,name)
    | `Internal {Node.value=(ns,name)} ->
	QName (PackageInternalNamespace ns,name)

let qname {Node.value=(ns,name)} =
  Cpool.make_qname ~ns:ns name

(** {6 Builtin operator } *)
let builtin = ["+",(Add_i,2);
	       "-",(Subtract_i,2);
	       "*",(Multiply_i,2);
	       "+.",(Add,2);
	       "-.",(Subtract,2);
	       "*.",(Multiply,2);
	       "/",(Divide,2);
	       "=",(Equals,2);
	       "remainder",(Modulo,2);
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

let rec generate_expr (expr  : V.expr) =
  let gen e =
    generate_expr e in
  match expr with
      `Bool {value = true} ->
	[PushTrue]
    | `Bool {value = false} ->
	[PushFalse]
    | `Float {value = v} ->
	[PushDouble v]
    | `String {value = str} ->
	[PushString str]
    | `Int {value = n} when 0 <= n && n <= 0xFF ->
	[PushByte n]
    | `Int {value = n} ->
	[PushInt n]
    | `Block []   ->
	[PushUndefined]
    | `Block xs   ->
	List.concat @@ interperse [Pop] @@ (List.map gen xs)
    | `New (name,args) ->
	let qname =
	  qname name in
	List.concat [
	  [FindPropStrict qname];
	  HList.concat_map gen args;
	  [ConstructProp (qname,List.length args)]]
    | `Lambda (args,body) ->
	let body' =
	  generate_expr body in
	let m = {
	  Asm.empty_method with
	    name         = Cpool.make_qname @@Label.to_string @@ Label.make ();
	    params       = List.map (const 0) args;
	    instructions = body' @ [ReturnValue] } in
	  [NewFunction m]
    | `Var name ->
	  [GetLex (qname name)]
    | `BindVar {value=V.Member (V.Scope scope,(ns,name)) } ->
	[GetScopeObject scope;
	 GetProperty (make_qname ~ns:ns name)]
    | `BindVar {value=V.Member (V.Global,(ns,name)) } ->
	[GetGlobalScope;
	 GetProperty (make_qname ~ns:ns name)]
    | `BindVar {value=V.Register n } ->
	[GetLocal n]
    | `BindVar {value=V.Slot (V.Global,id) } ->
	[GetGlobalSlot id]
    | `BindVar {value=V.Slot (V.Scope scope,id) } ->
	[GetScopeObject scope;
	 GetSlot id]
    | `Let (vars,body) ->
	let inits =
	  vars +> HList.concat_map
	    (fun ({value = var},init)->
	       [PushString var] @
		 generate_expr init) in
	List.concat [inits;
		     [NewObject (List.length vars);
		      PushWith];
		     generate_expr body;
		     [PopScope]]
    | `LetRec (vars,body) ->
	let inits =
	  vars +> HList.concat_map
	    (fun ({value = var},init)->
	       List.concat [
		 [Dup];
		 generate_expr init;
		 [SetProperty (make_qname var)] ]) in
	  List.concat [[NewObject 0;Dup;PushWith];
		        inits;
		       [Pop];
		        generate_expr body;
		       [PopScope]]
    | `Invoke (obj,{value = name},args)->
	List.concat [
	  gen obj;
	  HList.concat_map gen args;
	  [CallProperty (make_qname name,List.length args)]]
    | `SlotRef (obj,{value = name}) ->
	List.concat [
	  gen obj;
	  [GetProperty (Cpool.make_qname name)]]
    | `SlotSet (obj,{value = name},value) ->
	List.concat [
	  gen value;
	  gen obj;
	  [Swap;
	   SetProperty (Cpool.make_qname name);
	   PushUndefined]]
    | `Call (`Var {value = ("",name)}::args) when is_builtin name args ->
	let inst,_ =
	  List.assoc name builtin in
	  List.concat [
	    HList.concat_map gen args;
	    [inst]]
    | `Call (`Var {value = (ns,name)}::args) ->
	let qname =
	  QName ((Namespace ns),name) in
	  List.concat [[FindPropStrict qname];
		       HList.concat_map generate_expr args;
		       [CallPropLex (qname,List.length args)]]
    | `Call (`BindVar {value =
		 V.Member (V.Scope scope,(ns,name))}::args) ->
	List.concat [[GetScopeObject scope];
		     HList.concat_map generate_expr args;
		     [CallPropLex (make_qname ~ns:ns name,
				   List.length args)]]
    | `Call (`BindVar {value =
		 V.Member (V.Global,(ns,name))}::args) ->
	List.concat [[GetGlobalScope];
		     HList.concat_map generate_expr args;
		     [CallPropLex (make_qname ~ns:ns name,
				   List.length args)]]
    | `Call (`BindVar {value = V.Register n}::args) ->
	List.concat [[GetLocal n;
		      GetGlobalScope];
		     HList.concat_map generate_expr args;
		     [Asm.Call (List.length args)]]
    | `Call (name::args) ->
	let nargs =
	  List.length args in
	  List.concat [gen name;
		       [GetGlobalScope];
		       HList.concat_map gen args;
		       [Asm.Call nargs]]
    | `Call [] ->
	failwith "must not happen"
    | `If (cond,cons,alt) ->
	let l_alt =
	  Label.make () in
	let l_if =
	  Label.make () in
	let prefix = List.concat @@ match cond with
	    `Call [`Var {value = (_var,"=")};a;b] ->
	      [gen a;gen b;[IfNe l_alt]]
	  | `Call [`Var {value = (_,">")};a;b] ->
	      [gen a;gen b;[IfNgt l_alt]]
	  | `Call [`Var {value = (_,">=")};a;b] ->
	      [gen a;gen b;[IfNge l_alt]]
	  | `Call [`Var {value = (_,"<")};a;b] ->
	      [gen a;gen b;[IfNlt l_alt]]
	  | `Call [`Var {value = (_,"<=")};a;b] ->
	      [gen a;gen b;[IfNle l_alt]]
	  | _ ->
	      [gen cond;[IfFalse l_alt]] in
	  List.concat [prefix;
		       gen cons;
		       [Coerce_a];
		       [Jump l_if;Label l_alt];
		       gen alt;
		       [Coerce_a];
		       [Label l_if]]

(* class *)
let init_prefix =
  [ GetLocal_0;
    ConstructSuper 0 ]

let generate_method scope ctx {Ast.method_name = name;
			       args = args;
			       body = body} =
  let {instructions = inst} as m =
    {Asm.empty_method with
       fun_scope    = scope;
       params       = List.map (const 0) @@ List.tl args;
       instructions = generate_expr body} in
    match name with
	`Public {Node.value="init"} ->
	  {ctx with
	     Asm.iinit =
	      {m with
		 name         = make_qname "init";
		 instructions = init_prefix @ inst @ [Pop;ReturnVoid]}}
      | `Public {Node.value=name} ->
	  {ctx with
	     Asm.methods =
	      {m with
		 name         = make_qname name;
		 instructions = inst @ [ReturnValue] } :: ctx.methods}
      | `Static {Node.value="init"} ->
	  {ctx with
	     Asm.cinit =
	      {m with
		 name         = make_qname "init";
		 instructions = inst @ [Pop;ReturnVoid]}}
      | `Static {Node.value=name} ->
	  {ctx with
	     Asm.static_methods =
	      {m with
		 name         = make_qname name;
		 instructions = inst @ [ReturnValue] } :: ctx.methods}

let generate_class name {value = (ns,sname)} attrs methods =
  let qname =
    qname_of_stmt_name name in
  let super =
    make_qname ~ns:ns sname in
  let init =
    { Asm.empty_method with
	name = make_qname "init";
	Asm.fun_scope = Asm.Class qname;
	instructions = init_prefix @ [ReturnVoid] } in
  let cinit =
    {Asm.empty_method with
       Asm.fun_scope = Asm.Class qname;
       name = make_qname "cinit";
       instructions = [ReturnVoid] } in
  let empty = {
    Asm.cname  = qname;
    sname      = super;
    flags_k    = [Sealed];
    cinit      = cinit;
    iinit      = init;
    interface  = [];
    methods    = [];
    static_methods = [];
    attributes = attrs
  } in
  let klass =
    List.fold_left (generate_method @@ Asm.Class qname)
      empty methods in
    [
      (* init class *)
      GetLex super;
      PushScope;
      GetLex super;
      NewClass klass;
      PopScope;

      (* add to scope *)
      GetGlobalScope;
      Swap;
      InitProperty qname
    ]

let generate_stmt (stmt : V.stmt)  =
  match stmt with
      `Expr expr ->
	(generate_expr expr)@[Pop]
    | `Define (name,body) ->
	let qname =
	  qname_of_stmt_name name in
	  List.concat [
	    [
	      FindPropStrict (make_qname "$Scope");
	      ConstructProp (make_qname "$Scope",0);
	      Dup;PushWith];
	    generate_expr body;
	    [SetProperty qname]]
    | `ReDefine (name,n,body) ->
	let qname =
	  qname_of_stmt_name name in
	  List.concat [
	    generate_expr body;
	    [GetScopeObject n;
	     Swap;
	     SetProperty qname]]
    | `Class {Ast.klass_name=name;
	      super=super;
	      attrs=attrs;
	      methods=methods} ->
	generate_class
	  name super
	  (List.map (Cpool.make_qname $ Node.value) attrs)
	  methods

let generate_program xs =
  HList.concat_map generate_stmt xs

let generate_scope_class slots =
  let attrs =
    List.map (fun ((ns,name),_)-> Cpool.make_qname ~ns:ns name)
      slots in
    generate_class
      (`Public (Node.ghost ("","$Scope")))
      (Node.ghost ("","Object"))
      attrs
      []

let generate_script slots program =
  let scope_class =
    generate_scope_class slots in
  let program' =
    generate_program program in
    {Asm.empty_method with
       name =
	make_qname "";
       instructions =
	[ GetLocal_0; PushScope ] @
	  scope_class @ program' @
	  [ReturnVoid]}

let generate slots program =
  let script =
    generate_script slots program in
  let ctx =
    to_context script in
  let slot_traits =
    assemble_slot_traits ctx @@
      List.map (fun ((ns,name),i)->
		  (Cpool.make_qname ~ns:ns name,i))
      slots in
  let { Asm.abc_cpool = cpool;
	method_info   = info;
	method_body   = body;
	class_info    = class_info;
	instance_info = instance_info} =
    assemble ctx in
  let class_traits =
    let n =
      List.length slots in
      ExtList.List.mapi
	(fun i {Abc.name_i=name} ->
	   {Abc.t_name=name; data=Abc.ClassTrait (i+n+1,i)})
	instance_info in
    { Abc.cpool   = cpool;
      method_info = info;
      method_body = body;
      metadata    = [];
      classes     = class_info;
      instances   = instance_info;
      script      = [{
		       Abc.init = 0;
		       trait_s  = slot_traits @ class_traits
		     }]}

