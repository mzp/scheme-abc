open Base
open Ast
open ClosTrans
open Node

let eq_ident {value = x} {value = y} =
  x = y

let rec eq_expr a b =
  match a,b with
      `Int    {value = x}, `Int {value = y} ->
	x = y
    | `String {value = x}, `String {value = y} ->
	x = y
    | `Bool   {value = x}, `Bool {value = y} ->
	x = y
    | `Float  {value = x}, `Float {value = y} ->
	x = y
    | `Var    {value = x}, `Var {value = y} ->
	x = y
    | `Lambda (args,expr), `Lambda (args',expr') ->
	(List.for_all2 eq_ident args args') && eq_expr expr expr'
    | `Call   args, `Call args' ->
	List.for_all2 eq_expr args args'
    | `If  (a,b,c), `If (a',b',c') ->
	List.for_all2 eq_expr [a;b;c] [a';b';c']
    | `Let (decls,body), `Let (decls',body')
    | `LetRec (decls,body), `LetRec (decls',body')  ->
	let b =
	  List.for_all2
	    (fun (v,e) (v',e') -> eq_ident v v' && eq_expr e e')
	    decls' decls' in
	  b && eq_expr body body'
    | `Block xs, `Block xs' ->
	List.for_all2 eq_expr xs xs'
    | `New ({value=name},args), `New ({value=name'},args') ->
	name = name' && HList.conj @@ List.map2 eq_expr args args'
    | `Invoke (obj,name,args), `Invoke (obj',name',args') ->
	eq_expr obj obj' && eq_ident name name' &&
	  List.for_all2 eq_expr args args'
    | `SlotRef (obj,name), `SlotRef (obj',name') ->
	eq_expr obj obj' && eq_ident name name'
    | `SlotSet (obj,name,value), `SlotSet (obj',name',value') ->
	eq_expr obj obj' && eq_ident name name' && eq_expr value' value'
    | _ ->
	false

let eq_method (name,args,body) (name',args',body') =
  eq_ident name name' &&
    (List.for_all2 eq_ident args args') &&
    eq_expr body body'

let eq_stmt a b =
  match a,b with
      `Define (name,body), `Define (name',body') ->
	eq_ident name name' && eq_expr body body'
    | `Expr expr, `Expr expr' ->
	eq_expr expr expr'
    | `Class (name,{value=super},attrs,methods),
	`Class (name',{value=super'},attrs',methods') ->
	eq_ident name name' &&
	  super = super' &&
	  (List.for_all2 eq_ident attrs attrs') &&
	  (List.for_all2 eq_method methods methods')
    | _ ->
	false

let eq_bind a b=
  match a,b with
     `External name , `External name' ->
	eq_ident name name'
    | `ExternalClass ({value=name},methods), `ExternalClass ({value=name'},methods') ->
	name = name' && List.for_all2 eq_ident methods methods'
    | a,b ->
	eq_stmt a b


let eq_clos a b =
  match a,b with
      `DefineClass (name,{value=super},attrs), `DefineClass (name',{value=super'},attrs') ->
	eq_ident name name' &&
	  super = super' && List.for_all2 eq_ident attrs attrs'
    | `DefineMethod (name,(self,obj),args,body), `DefineMethod (name',(self',obj'),args',body') ->
	eq_ident name name' &&
	  eq_ident self self' &&
	  eq_ident obj obj' &&
	  (List.for_all2 eq_ident args args') &&
	  eq_expr body body'
    | a,b ->
	eq_bind a b

(* random node *)
let count =
  ref 0

let node x =
  let pos ()=
    incr count;
    !count in
    {(Node.empty x) with
       Node.filename = "<string>";
       Node.lineno   = 0;
       start_pos     = pos ();
       end_pos       = pos ()}

let name x =
  node ("",x)

let string x =
  `String (node x)

let int x =
  `Int (node x)

let float x =
  `Float (node x)

let bool x =
  `Bool (node x)

let var x =
  `Var (node ("",x))
