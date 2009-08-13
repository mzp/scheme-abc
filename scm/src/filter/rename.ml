open Base
type qname = string list * string
type env = {env: (qname * int) list; current: string list}
type rename_map = { map : (qname * qname) list }

let update_env qname env =
  try
    let n =
      List.assoc qname env.env in
    let name' =
      Printf.sprintf "%s$%d" (snd qname) n in
      ({env with env = (qname,n+1)::env.env},
       (fst qname, name'))
  with Not_found ->
    ({env with env = (qname,0)::env.env},qname)

let add_map qname qname' {map=map} =
  {map=(qname,qname')::map}

let find_map qname {map=map} =
  List.assoc qname map

let local_var xs {map=map} =
  let ys =
    xs
    +> List.map (fun {Node.value=x} -> ([],x))
    +> List.map (fun x -> (x,x)) in
    {map=ys @ map}

let empty_map =
  {map=[]}

let rename_expr rename_map xs =
  Ast.fix_fold Ast.fold
    begin fun map e ->
      match e with
	| `Let (xs,_) | `LetRec (xs,_) ->
	    local_var (List.map fst xs) map
	| `Lambda (args,_) ->
	    local_var args map
	| `Var _ | `Int _ | `String  _ | `Bool _
	| `Float _ | `Array _ | `Call _ | `If _
	| `Block _ | `New _ | `Invoke _
	| `SlotRef _ | `SlotSet _ ->
	    map
    end
    begin fun map e ->
      match e with
	| `Var name ->
	    begin try
	      `Var {name with Node.value=(find_map (Node.value name) map)}
	    with Not_found ->
	      e
	    end
	| `Let _ | `LetRec _| `Lambda _
	| `Int _ | `String  _ | `Bool _
	| `Float _ | `Array _ | `Call _ | `If _
	| `Block _ | `New _ | `Invoke _
	| `SlotRef _ | `SlotSet _ ->
	    e
    end
    rename_map xs

let rename_define env stmts (`Define (node, expr)) =
  let qname =
    (env.current,Node.value node) in
  let env',qname' =
    update_env qname env in
  let stmt m =
    let m' =
      add_map qname qname' m in
      (`Define ({node with
		   Node.value = snd qname'},
		rename_expr m' expr))::stmts m' in
    env',stmt

let rename_method map ({Ast.args=args; body=e} as meth) =
  let map' =
    local_var args map in
    {meth with Ast.body = rename_expr map' e}

let rename_class env stmts (`Class ({ Ast.class_name=node;
				      methods=methods} as info)) =
  let qname =
    (env.current,Node.value node) in
  let env',qname' =
    update_env qname env in
  let stmt m =
    let m' =
      add_map qname qname' m in
      (`Class {info with
		 Ast.class_name = {node with Node.value=snd qname'};
		 methods = List.map (rename_method m') methods
	      })::stmts m' in
    env',stmt

let rec rename_stmt env stmts =
  List.fold_right begin
    fun stmt (env,stmts) ->
      match stmt with
	  `Expr e ->
	    env,(fun m -> `Expr (rename_expr m e)::stmts m)
       | `Define _ as define ->
	   rename_define env stmts define
       | `Class _ as m ->
	   rename_class env stmts m
       | `Module ({Ast.module_name=name;
		   stmts = child} as info) ->
	   let child' =
	     rename_stmt ({env with current=env.current @ [Node.value name]},
			  const []) child in
	     env,(fun m ->
		    let s =
		      `Module {info with Ast.stmts = (snd @@ child') m} in
		      s::stmts m)
       | `Open _ as s ->
	   env,(fun m -> s ::stmts m)
    end stmts env

let rename program =
  (snd @@ rename_stmt ({env=[];current=[]},const []) program) empty_map

let _ = rename [`Module {
		  Ast.module_name = Node.ghost "foo";
		  exports=`All;
		  stmts = [
		    `Define(Node.ghost "x",`Int (Node.ghost 1));
		    `Expr (`Var (Node.ghost (["foo"],"x")));
		    `Define(Node.ghost "x",`Int (Node.ghost 1))]
		}]
