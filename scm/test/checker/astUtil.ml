open Base
open Ast

let sname name =
  Node.ghost name

let qname ns name =
  Node.ghost (ns,name)

let global name =
  qname [] name


let int x : Ast.expr' =
  `Int (Node.ghost x)

let var ns name : Ast.expr' =
  `Var (qname ns name)

let new_ ns name args : Ast.expr' =
  `New (qname ns name,args)

let invoke obj name args : Ast.expr' =
  `Invoke (obj, Node.ghost name, args)

let let_ inits body : Ast.expr' =
  `Let (List.map (Tuple.T2.map1 Node.ghost) inits,body)

let let_rec inits body : Ast.expr' =
  `LetRec (List.map (Tuple.T2.map1 Node.ghost) inits,body)

let call xs : Ast.expr' =
  `Call xs

let block xs : Ast.expr' =
  `Block xs

let define name body : Ast.stmt' =
  `Define ((Node.ghost name),body)

let expr expr : Ast.stmt' =
  `Expr expr

let module_ name exports xs : Ast.stmt' =
  `Module {Ast.module_name = Node.ghost name;
	   exports         = exports;
	   stmts           = xs}

let foo_mod xs =
  module_ "foo" `All xs

let bar_mod xs =
  module_ "bar" `All xs


let lambda args body =
  `Lambda (List.map Node.ghost args, body)

let slot_ref obj name =
  `SlotRef (obj, Node.ghost name)

let slot_set obj name value =
  `SlotSet (obj, Node.ghost name, value)

let class_ name super attrs methods =
  `Class {Ast.class_name = Node.ghost name;
	  super = Node.ghost super;
	  attrs = List.map Node.ghost attrs;
	  methods = methods}

let public_meth name args body =
  {Ast.method_name=`Public (Node.ghost name);
   args = List.map Node.ghost args;
   body = body}


let static_meth name args body =
  {Ast.method_name=`Static (Node.ghost name);
   args = List.map Node.ghost args;
   body = body}
