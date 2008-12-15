open Base
open Ast
open ClosTrans
open Node

let node x =
  {(Node.empty x) with
     Node.filename = "<string>";
     Node.lineno   = 0;
     start_pos     = 0;
     end_pos       = 0}

let sname =
  node

let global x =
  node ("",x)

let qname ns name =
  node (ns,name)

let sname =
  node

let string x =
  `String (node x)

let int x =
  `Int (node x)

let float x =
  `Float (node x)

let bool x =
  `Bool (node x)

let let1 name init body =
  `Let ([(sname name,init)],body)

let letrec1 name init body =
  `LetRec ([(sname name,init)],body)

let lambda args body =
  `Lambda (List.map sname args,body)

let var x =
  `Var x

let invoke obj name args =
  `Invoke (obj,node name,args)

let new_klass k args =
  `New (k,args)

let block x =
  `Block x

let expr x=
  `Expr x

let meth name args body =
  (node name,List.map node args,body)

let public_meth name args body =
  (sname name,List.map node args,body)

let klass k super attrs methods =
  `Class (k,super,List.map node attrs,methods)

let define x expr =
  `Define (x,expr)

let define_class k super attrs =
  `DefineClass (k,super,List.map node attrs)

let define_method f self obj args body =
  `DefineMethod (node f,(node self,obj),List.map node args,body)

let external_var name =
  `External (name)

let external_class k methods =
  `ExternalClass (k,List.map node methods)

let module_ name exports xs =
  `Module (sname name,exports,xs)

let foo_mod xs =
  module_ "foo" ModuleTrans.All xs

let bar_mod xs =
  module_ "bar" ModuleTrans.All xs

