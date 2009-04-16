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

let trans x = x
