open Base

type 'a expr_type =
    'a ModuleTrans.expr_type

type ('expr,'stmt) stmt_type =
    [ ('expr,'stmt) ModuleTrans.expr_stmt_type
    | ('expr,'stmt) ModuleTrans.module_stmt_type
    | `DefineClass  of class_
    | `DefineMethod of 'expr method_
    | `DefineStaticMethod of 'expr method_ ]
and class_  = {
  class_name: Ast.sname;
  super: Ast.qname;
  attrs: Ast.attr list;
} and 'expr method_ = {
  method_name: Ast.sname;
  to_class:    Ast.sname;
  args: Ast.sname list;
  body: 'expr
}

let fold f g fold_rec env s =
  ModuleTrans.fold f g fold_rec env s

let fold_stmt f g fold_rec env  =
  function
      #ModuleTrans.expr_stmt_type as s ->
	ModuleTrans.fold_expr_stmt f g env s
    | #ModuleTrans.module_stmt_type as s ->
	ModuleTrans.fold_module_stmt f g fold_rec env s
    | `DefineClass _ | `DefineMethod _ | `DefineStaticMethod _ as s ->
	g (f env s) s

let lift f lift_rec =
  function
      #ModuleTrans.expr_stmt_type as s ->
	ModuleTrans.lift_expr f s
    | #ModuleTrans.module_stmt_type as s ->
	ModuleTrans.lift_module f lift_rec s
    | `DefineClass _ as s ->
	s
    | `DefineMethod m ->
	`DefineMethod {m with body = f m.body }
    | `DefineStaticMethod m ->
	`DefineMethod {m with body = f m.body }

(* ------------------------------ *)
type expr =
    expr expr_type

type stmt =
    (expr,stmt) stmt_type

type program = stmt list

let rec fold' f g env (s : [< stmt ]) = fold_stmt f g (fold' f g) env s
let rec lift' f s = lift f (lift' f) s

let rec klass2method tbl nss s =
  fold'
    (fun nss s ->
       match s with
	   `Module {ModuleTrans.module_name={Node.value = ns}} ->
	     ns::nss
	 | `DefineMethod {method_name = name;
			  to_class = {Node.value = klass};
			  args = args;
			  body = body} ->
	     Hashtbl.add tbl (nss,klass)
	       {Ast.method_name = `Public name;
		args = args;
		body = body};
	     nss
	 | `DefineStaticMethod {method_name = name;
				to_class = {Node.value = klass};
				args = args;
				body = body} ->
	     Hashtbl.add tbl (nss,klass)
	       {Ast.method_name = `Static name;
		args = args;
		body = body};
	     nss
	 | `DefineClass _ | `Expr _ | `Define _ ->
	     nss)
    (fun _ _ -> ()) nss s

(*
  function
      `DefineMethod {method_name = name;
		     to_class = {Node.value = klass};
		     args = args;
		     body = body} ->
	Hashtbl.add tbl (nss,klass)
	  {Ast.method_name = `Public name;
	   args = args;
	   body = body}
    | `DefineStaticMethod {method_name = name;
			   to_class = {Node.value = klass};
			   args = args;
			   body = body} ->
	Hashtbl.add tbl (nss,klass)
	  {Ast.method_name = `Static name;
	   args = args;
	   body = body}
    | `Module {ModuleTrans.module_name={Node.value = ns};
	       stmts=stmts} ->
	stmts +> List.iter (klass2method tbl (ns::nss))
    | `Class _ | `Expr _ | `DefineClass _ ->
	()
*)

let klass2methods program =
  let tbl =
    Hashtbl.create 0 in
    List.iter (klass2method tbl []) program;
    tbl

let rec methods s =
  fold' (fun xs s ->
	   match s with
	       `DefineMethod {method_name={Node.value = name}}
	     | `DefineStaticMethod {method_name={Node.value = name}} ->
		 name::xs
	     | `Module _ | `Define _ | `Expr _ | `DefineClass _ ->
		 xs)
    const [] s;;

let methods_set program =
  PSet.set_of_list @@ HList.concat_map methods program

let call_to_invoke (set,tbl) =
  Ast.map
    (function
	 `Call ((`Var ({Node.value = ("",f)} as node))::obj::args)
	   when PSet.mem f set || InterCode.mem_method f tbl ->
	     `Invoke (obj,Node.lift snd node,args)
       | #expr as e ->
	   e)
(*
let rec stmt_trans nss tbl set s =
  fold'
    begin fun nss s ->
       match s with
	   `Module {ModuleTrans.module_name={Node.value = ns}} ->
	     ns::nss
	 | `DefineClass _ | `DefineMethod _ | `DefineStaticMethod _ | `Define _ | `Expr _ ->
	     nss
    end
    begin fun nss s ->
      match s with
	  `DefineClass {class_name={Node.value=name} as klass;
			super = super;
			attrs = attrs} ->
	    [`Class {Ast.class_name=klass;
		     super=super;
		     attrs=attrs;
		     methods=Hashtbl.find_all tbl (nss,name)}]
	 | `DefineMethod _ | `DefineStaticMethod _ ->
	     []
	 | `Expr _ | `Define _ ->
	     [lift' (call_to_invoke set) s]
	 | `Module m ->
	     []
	 | #stmt_type as s ->
	     [lift' (call_to_invoke set) s]
    end
    nss s
  *)
let trans table program =
  Obj.magic 42
(*  let k2m =
    klass2methods program in
  let ms =
    methods_set program in
    program
    +>  HList.concat_map (stmt_trans [] k2m (ms,table))*)
