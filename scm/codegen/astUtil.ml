open Base
open Ast

let qname ns name =
  Node.ghost (ns,name)

let int x =
  `Int (Node.ghost x)

let float x =
  `Float (Node.ghost x)

let bool  x =
  `Bool (Node.ghost x)

let string x =
  `String (Node.ghost x)

let var ns name =
  `Var (qname ns name)

let new_ ns name args =
  `New (qname ns name,args)

let if_ a b c =
  `If (a,b,c)

let invoke obj name args =
  `Invoke (obj, Node.ghost name, args)

let let_ inits body =
  `Let (List.map (Tuple.T2.map1 Node.ghost) inits,body)

let let_rec inits body =
  `LetRec (List.map (Tuple.T2.map1 Node.ghost) inits,body)

let call xs  =
  `Call xs

let block xs  =
  `Block xs

let define name body  =
  `Define ((Node.ghost name),body)

let expr expr =
  `Expr expr

let lambda args body =
  `Lambda (List.map Node.ghost args, body)

let slot_ref obj name =
  `SlotRef (obj, Node.ghost name)

let slot_set obj name value =
  `SlotSet (obj, Node.ghost name, value)

let class_ name super attrs methods =
  `Class {Ast.class_name =name;
	  super = super;
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

let module_ name exports xs : Ast.stmt' =
  `Module {Ast.module_name = Node.ghost name;
	   exports         = exports;
	   stmts           = xs}

let foo_mod xs =
  module_ "foo" `All xs

let bar_mod xs =
  module_ "bar" `All xs
