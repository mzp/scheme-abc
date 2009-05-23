open Base
open Ast
open Asm
open Node

module QName = struct
  open Cpool

  let join xs =
    String.concat "." xs

  let of_stmt_name =
    function
	`Public {Node.value=(ns,name)} ->
	  `QName (`Namespace (join ns),name)
      | `Internal {Node.value=(ns,name)} ->
	  `QName (`PackageInternalNamespace (join ns),name)

  let make ns x =
    `QName ((`Namespace (join ns)),x)

  let of_node {Node.value=(ns,name)} =
    make ns name

  let make_global name =
    make [] name
end

(** {6 Builtin operator } *)
let builtin = [
  "+" , (`Add_i,2);
  "-" , (`Subtract_i,2);
  "*" , (`Multiply_i,2);
  "+.", (`Add,2);
  "-.", (`Subtract,2);
  "*.", (`Multiply,2);
  "/" , (`Divide,2);
  "remainder", (`Modulo,2);
  "="  , (`Equals,2);
  ">"  , (`GreaterThan,2);
  ">=" , (`GreaterEquals,2);
  "<"  , (`LessThan,2);
  "<=" , (`LessEquals,2)
]

let is_builtin name args =
  try
    let _,n =
      List.assoc name builtin in
      n = List.length args
  with Not_found ->
    false

let rec generate_expr expr =
  let gen e =
    generate_expr e in
  match expr with
      `Bool {value = true} ->
	[`PushTrue]
    | `Bool {value = false} ->
	[`PushFalse]
    | `Float {value = v} ->
	[`PushDouble v]
    | `String {value = str} ->
	[`PushString str]
    | `Int {value = n} when 0 <= n && n <= 0xFF ->
	[`PushByte n]
    | `Int {value = n} ->
	[`PushInt n]
    | `Block []   ->
	[`PushUndefined]
    | `Block xs   ->
	List.concat @@ interperse [`Pop] @@ (List.map gen xs)
    | `New (name,args) ->
	let qname =
	  QName.of_node name in
	List.concat [
	  [`FindPropStrict qname];
	  HList.concat_map gen args;
	  [`ConstructProp (qname, List.length args)]]
    | `Lambda (args,body) ->
	let body' =
	  generate_expr body in
	let m = {
	  empty_method with
	    method_name = QName.make_global @@ Label.to_string @@ Label.make ();
	    params          = List.map (const 0) args;
	    instructions    = body' @ [`ReturnValue] } in
	  [`NewFunction m]
    | `Var name ->
	  [`GetLex (QName.of_node name)]
    | `BindVar {value=Binding.Member (Binding.Scope scope,(ns,name)) } ->
	[`GetScopeObject scope;
	 `GetProperty (QName.make ns name)]
    | `BindVar {value=Binding.Member (Binding.Global,(ns,name)) } ->
	[`GetGlobalScope;
	 `GetProperty (QName.make ns name)]
    | `BindVar {value=Binding.Register n } ->
	[`GetLocal n]
    | `BindVar {value=Binding.Slot (Binding.Global,id) } ->
	[`GetGlobalSlot id]
    | `BindVar {value=Binding.Slot (Binding.Scope scope,id) } ->
	[`GetScopeObject scope;
	 `GetSlot id]
    | `Let (vars,body) ->
	let inits =
	  vars +> HList.concat_map
	    (fun ({value = var},init)->
	       [`PushString var] @
		 generate_expr init) in
	List.concat [inits;
		     [`NewObject (List.length vars);
		      `PushWith];
		     generate_expr body;
		     [`PopScope]]
    | `LetRec (vars,body) ->
	let inits =
	  vars +> HList.concat_map
	    (fun ({value = var},init)->
	       List.concat [
		 [`Dup];
		 generate_expr init;
		 [`SetProperty (QName.make_global var)] ]) in
	  List.concat [[`NewObject 0;`Dup;`PushWith];
		       inits;
		       [`Pop];
		       generate_expr body;
		       [`PopScope]]
    | `Invoke (obj,{value = name},args)->
	List.concat [
	  gen obj;
	  HList.concat_map gen args;
	  [`CallProperty (QName.make_global name,List.length args)]]
    | `SlotRef (obj,{value = name}) ->
	List.concat [
	  gen obj;
	  [`GetProperty (QName.make_global name)]]
    | `SlotSet (obj,{value = name},value) ->
	List.concat [
	  gen value;
	  gen obj;
	  [`Swap;
	   `SetProperty (QName.make_global name);
	   `PushUndefined]]
    | `Call (`Var {value = ([],name)}::args) when is_builtin name args ->
	let inst,_ =
	  List.assoc name builtin in
	  List.concat [
	    HList.concat_map gen args;
	    [inst]]
    | `Call (`Var {value = (ns,name)}::args) ->
	let qname =
	  QName.make ns name in
	  List.concat [[`FindPropStrict qname];
		       HList.concat_map generate_expr args;
		       [`CallPropLex (qname,List.length args)]]
    | `Call (`BindVar {value =
		 Binding.Member (Binding.Scope scope,(ns,name))}::args) ->
	List.concat [[`GetScopeObject scope];
		     HList.concat_map generate_expr args;
		     [`CallPropLex (QName.make ns name,
				    List.length args)]]
    | `Call (`BindVar {value =
		 Binding.Member (Binding.Global,(ns,name))}::args) ->
	List.concat [[`GetGlobalScope];
		     HList.concat_map generate_expr args;
		     [`CallPropLex (QName.make ns name,
				    List.length args)]]
    | `Call (`BindVar {value = Binding.Register n}::args) ->
	List.concat [[`GetLocal n;
		      `GetGlobalScope];
		     HList.concat_map generate_expr args;
		     [`Call (List.length args)]]
    | `Call (name::args) ->
	let nargs =
	  List.length args in
	  List.concat [gen name;
		       [`GetGlobalScope];
		       HList.concat_map gen args;
		       [`Call nargs]]
    | `Call [] ->
	failwith "must not happen"
    | `If (cond,cons,alt) ->
	let l_alt =
	  Label.make () in
	let l_if =
	  Label.make () in
	let prefix = List.concat @@ match cond with
	    `Call [`Var {value = (_var,"=")};a;b] ->
	      [gen a;gen b;[`IfNe l_alt]]
	  | `Call [`Var {value = (_,">")};a;b] ->
	      [gen a;gen b;[`IfNgt l_alt]]
	  | `Call [`Var {value = (_,">=")};a;b] ->
	      [gen a;gen b;[`IfNge l_alt]]
	  | `Call [`Var {value = (_,"<")};a;b] ->
	      [gen a;gen b;[`IfNlt l_alt]]
	  | `Call [`Var {value = (_,"<=")};a;b] ->
	      [gen a;gen b;[`IfNle l_alt]]
	  | _ ->
	      [gen cond;[`IfFalse l_alt]] in
	  List.concat [prefix;
		       gen cons;
		       [`Coerce_a];
		       [`Jump l_if;`Label l_alt];
		       gen alt;
		       [`Coerce_a];
		       [`Label l_if]]

(* class *)
let init_prefix =
  [ `GetLocal_0;
    `ConstructSuper 0 ]

let generate_method scope ctx {Ast.method_name = name;
			       args = args;
			       body = body} =
  let {instructions = inst} as m =
    {Asm.empty_method with
       fun_scope    = scope;
       instructions = generate_expr body} in
    match name with
	`Public {Node.value="init"} ->
	  {ctx with
	     Asm.iinit =
	      {m with
		 method_name  = QName.make_global "init";
		 params       = List.map (const 0) @@ List.tl args;
		 instructions = init_prefix @ inst @ [`Pop; `ReturnVoid]}}
      | `Public {Node.value=name} ->
	  {ctx with
	     Asm.instance_methods =
	      {m with
		 method_name  = QName.make_global name;
		 params       = List.map (const 0) @@ List.tl args;
		 instructions = inst @ [`ReturnValue] } :: ctx.instance_methods}
      | `Static {Node.value="init"} ->
	  {ctx with
	     Asm.cinit =
	      {m with
		 method_name  = QName.make_global "init";
		 params       = List.map (const 0) args;
		 instructions = inst @ [`Pop; `ReturnVoid]}}
      | `Static {Node.value=name} ->
	  {ctx with
	     Asm.static_methods =
	      {m with
		 method_name  = QName.make_global name;
		 params       = List.map (const 0) args;
		 instructions = inst @ [`ReturnValue] } :: ctx.static_methods}

let generate_class name {value = (ns,sname)} attrs methods =
  let qname =
    QName.of_stmt_name name in
  let super =
    QName.make ns sname in
  let init =
    { Asm.empty_method with
	method_name  = QName.make_global "init";
	fun_scope    = Asm.Class qname;
	instructions = init_prefix @ [`ReturnVoid] } in
  let cinit =
    {Asm.empty_method with
       method_name  = QName.make_global "cinit";
       fun_scope    = Asm.Class qname;
       instructions = [`ReturnVoid] } in
  let empty = {
    class_name       = qname;
    super            = super;
    class_flags      = [Sealed];
    cinit            = cinit;
    iinit            = init;
    interface        = [];
    instance_methods = [];
    static_methods   = [];
    attributes = attrs
  } in
  let klass =
    List.fold_left (generate_method @@ Asm.Class qname)
      empty methods in
    [
      (* init class *)
      `GetLex super;
      `PushScope;
      `GetLex super;
      `NewClass klass;
      `PopScope;

      (* add to scope *)
      `GetGlobalScope;
      `Swap;
      `InitProperty qname
    ]

let generate_stmt stmt  =
  match stmt with
      `Expr expr ->
	(generate_expr expr) @ [`Pop]
    | `Define (name,body) ->
	let qname =
	  QName.of_stmt_name name in
	  List.concat [
	    [
	      `FindPropStrict (QName.make_global "$Scope");
	      `ConstructProp (QName.make_global "$Scope",0);
	      `Dup;
	      `PushWith];
	    generate_expr body;
	    [`SetProperty qname]]
    | `ReDefine (name,n,body) ->
	let qname =
	  QName.of_stmt_name name in
	  List.concat [
	    generate_expr body;
	    [`GetScopeObject n;
	     `Swap;
	     `SetProperty qname]]
    | `Class {Ast.class_name=name;
	      super=super;
	      attrs=attrs;
	      methods=methods} ->
	generate_class
	  name super
	  (List.map (QName.make_global $ Node.value) attrs)
	  methods

let generate_program xs =
  HList.concat_map generate_stmt xs

let generate_scope_class slots =
  let attrs =
    List.map (fun ((ns,name),_)-> QName.make ns name)
      slots in
    generate_class
      (`Public (Node.ghost ([],"$Scope")))
      (Node.ghost ([],"Object"))
      attrs
      []

let generate_script slots program =
  let scope_class =
    generate_scope_class slots in
  let program' =
    generate_program program in
    {Asm.empty_method with
       method_name =
	QName.make_global "";
       instructions =
	[ `GetLocal_0; `PushScope ] @
	  scope_class @ program' @
	  [`ReturnVoid]}

let generate slots program =
  let script =
    generate_script slots program in
  let { Asm.cpool = cpool;
	method_info   = info;
	method_body   = body;
	class_info    = class_info;
	instance_info = instance_info} =
    assemble script in
  let slot_traits =
    assemble_slot_traits cpool @@
      List.map (fun ((ns,name),i)->
		  (QName.make ns name,i))
      slots in
  let class_traits =
    let n =
      List.length slots in
      ExtList.List.mapi
	(fun i {Abc.instance_name=name} ->
	   {Abc.trait_name=name; data=Abc.ClassTrait (i+n+1,i)})
	instance_info in
    { Abc.cpool   = Cpool.to_abc cpool;
      method_info = info;
      method_bodies = body;
      metadata    = [];
      classes     = class_info;
      instances   = instance_info;
      scripts     = [{
		       Abc.init = 0;
		       script_traits = slot_traits @ class_traits
		     }]}

