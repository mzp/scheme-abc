open Base
type 'a expr_type = 'a ModuleTrans.expr_type
type ('a,'b) stmt_type = [
  ('a,'b) ModuleTrans.stmt_type
| `Open of Ast.sname
]

let fold f g fold_rec env s =
  ModuleTrans.fold f g fold_rec env s

let lift f lift_rec =
  function
      #ModuleTrans.stmt_type as s ->
	ModuleTrans.lift f lift_rec s
    | `Open _ as s ->
	s

let fold_stmt f g fold_rec env =
  function
      #ModuleTrans.stmt_type as s ->
	ModuleTrans.fold_stmt f g fold_rec env s
    | `Open _ as s ->
	g (f env s) s

type env = {
  opened  : string list;
  symbols : (string * string list) list;
  current : string list
}
let empty = {
  opened = [];
  symbols=[];
  current = [];
}

let rec fold_stmt' f g env s=
  fold_stmt f g (fold_stmt' f g) env s

let expand_open env s =
  fold_stmt'
    begin fun env s ->
      env
    end
    begin fun env s ->
      match s with
	  `Define (name,_) ->
	    {env with
	       current = Node.value name::env.current}
	| `Class {Ast.class_name = name} ->
	    {env with
	       current = Node.value name::env.current}
	| `Module {ModuleTrans.module_name = name; stmts=s} ->
	    {env with
	       current = [];
	       symbols = (Node.value name,HList.concat_map (fun e->e.current) s)::env.symbols}
	| _ ->
	    env
    end env s

let trans x = x

open Node
let sample =
[`Module
   {ModuleTrans.module_name =
     {value = "Foo"; filename = "<string>"; lineno = 0; start_pos = 0;
      end_pos = 0};
    ModuleTrans.exports = `All;
    ModuleTrans.stmts =
     [`Define
        ({value = "x"; filename = "<string>"; lineno = 0; start_pos = 0;
          end_pos = 0},
         `Block [])]};
 `Module
   {ModuleTrans.module_name =
     {value = "Bar"; filename = "<string>"; lineno = 0; start_pos = 0;
      end_pos = 0};
    ModuleTrans.exports = `All;
    ModuleTrans.stmts =
     [`Open
        {value = "foo"; filename = "<string>"; lineno = 0; start_pos = 0;
         end_pos = 0};
      `Expr
        (`Var
           {value = ("", "x"); filename = "<string>"; lineno = 0;
            start_pos = 0; end_pos = 0})]}]
