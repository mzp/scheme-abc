open Base

type 'a expr =
    'a Ast.expr

type ('expr,'stmt) stmt =
    [ ('expr,'stmt) Ast.expr_stmt
    | ('expr,'stmt) Ast.module_stmt
    | `DefineClass  of class_
    | `DefineMethod of 'expr method_
    | `DefineStaticMethod of 'expr method_ ]
and class_  = {
  class_name: Ast.sname;
  super: Ast.qname;
  attrs: Ast.sname list;
} and 'expr method_ = {
  method_name: Ast.sname;
  to_class:    Ast.sname;
  args: Ast.sname list;
  body: 'expr
}

let fold f g fold_rec env s =
  Ast.fold f g fold_rec env s

let fold_stmt f g fold_rec env  =
  function
      #Ast.expr_stmt as s ->
	Ast.fold_expr_stmt f g env s
    | #Ast.module_stmt as s ->
	Ast.fold_module_stmt f g fold_rec env s
    | `DefineClass _ | `DefineMethod _ | `DefineStaticMethod _ as s ->
	g (f env s) s

let lift f lift_rec =
  function
      #Ast.expr_stmt as s ->
	Ast.lift_expr f s
    | #Ast.module_stmt as s ->
	Ast.lift_module f lift_rec s
    | `DefineClass _ as s ->
	s
    | `DefineMethod m ->
	`DefineMethod {m with body = f m.body }
    | `DefineStaticMethod m ->
	`DefineMethod {m with body = f m.body }

type expr' =
    expr' expr

type stmt' =
    (expr',stmt') stmt

type program = stmt' list

(* ------------------------------ *)
let rec klass2method tbl nss s =
  Ast.fix_fold fold_stmt
    begin fun nss s ->
       match s with
	   `Module {Ast.module_name={Node.value = ns}} ->
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
	 | `DefineClass _ | `Expr _ | `Define _ | `Open _ ->
	     nss
    end
    begin fun _ _ ->
      ()
    end nss s

let klass2methods program =
  let tbl =
    Hashtbl.create 0 in
    List.iter (klass2method tbl []) program;
    tbl

let rec methods s =
  Ast.fix_fold fold_stmt
    begin fun xs s ->
	   match s with
	       `DefineMethod {method_name={Node.value = name}}
	     | `DefineStaticMethod {method_name={Node.value = name}} ->
		 name::xs
	     | `Module _ | `Define _ | `Expr _ | `DefineClass _ | `Open _ ->
		 xs
    end
    const [] s;;

let methods_set program =
  program
  +> HList.concat_map methods
  +> PSet.set_of_list

let mem_method name (set,table) =
  PSet.mem name set or table#mem_method name

let call_to_invoke table e =
  e +> Ast.map fold begin function
      `Call ((`Var ({Node.value = ([],f)} as node))::obj::args)
	when mem_method f table ->
	  `Invoke (obj,Node.lift snd node,args)
    | #expr as e ->
	e
  end

let rec expand_class nss tbl table s =
  Ast.fix_fold fold_stmt
    begin fun nss s ->
       match s with
	   `Module {Ast.module_name={Node.value = ns}} ->
	     ns::nss
	 | `Define (({Node.value = name} as loc),`Lambda _)
	     when mem_method name table ->
	     Node.report "warning" {loc with
				      Node.value =
		 Printf.sprintf "%s is already defined as method" name};
	     nss
	 | `DefineClass _ | `DefineMethod _ | `DefineStaticMethod _
	 | `Define _ | `Expr _ | `Open _ ->
	     nss
    end
    begin fun nss s ->
      match s with
	| `DefineClass {class_name={Node.value=name} as klass;
			super = super;
			attrs = attrs} ->
	    let rec lift f s =
	      Ast.lift f (lift f) s in
	      [lift (call_to_invoke table) @@
		 `Class {
		    Ast.class_name = klass;
		   super          = super;
		   attrs          = attrs;
		   methods        = List.rev @@ Hashtbl.find_all tbl (nss,name)
		 }]
	 | `DefineMethod _ | `DefineStaticMethod _  ->
	     []
	 | #Ast.expr_stmt as s ->
	     let rec lift f s =
	       Ast.lift_expr f s in
	       [lift (call_to_invoke table) s]
	 | `Module m  ->
	     let rec lift f s =
	       Ast.lift f (lift f) s in
	       [lift (call_to_invoke table)

		 (`Module {m with
			     Ast.stmts = List.concat m.Ast.stmts})]
	 | `Open _ as s ->
	     [s]
    end
    nss s

let to_ast table program =
  let k2m =
    klass2methods program in
  let methods =
    methods_set program in
    program
    +>  HList.concat_map (expand_class [] k2m (methods,table))
