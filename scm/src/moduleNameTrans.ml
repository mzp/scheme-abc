open Base
let rec fold' f g env expr =
  ModuleTrans.fold f g (fold' f g) env expr

let rec lift' f s =
  ModuleTrans.lift f (lift' f) s

let rec fold_stmt' f g env s =
  ModuleTrans.fold_stmt f g (fold_stmt' f g) env s

let map_expr f expr =
  fold'
    (flip const)
    (fun _ b -> f b)
    expr expr

let map_stmt f s =
  fold_stmt'
    (flip const)
    (fun _ b -> f b)
    s s

let uncap xs =
  xs
  +> Str.split (Str.regexp "\\.")
  +> List.map String.uncapitalize
  +> String.concat "."

let uncap_qname x =
  Node.lift (Tuple.T2.map1 uncap) x

let uncap_expr e =
  e +> map_expr begin function
      `Var var ->
	`Var (uncap_qname var)
    | `New (c,args) ->
	`New (uncap_qname c,args)
    | `Int _ | `Bool _ | `String _ | `Float _ | `Lambda _ | `Call _ | `If _ | `Let _
    | `LetRec _ | `Block _ | `Invoke _ | `SlotRef _ | `SlotSet _ as e ->
	e
  end

let uncap_stmt s =
  s +> map_stmt begin function
      `Module m ->
	`Module {m with
		   ModuleTrans.module_name =
	             Node.lift String.uncapitalize m.ModuleTrans.module_name }
    | `Class c ->
	`Class {c with
		  Ast.super = uncap_qname c.Ast.super}
    | `Define _ | `Expr _ as s ->
	s
  end

let trans xs =
  List.map (uncap_stmt $ lift' uncap_expr) xs
