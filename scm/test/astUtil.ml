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

let public_meth name args body =
  {Ast.method_name=`Public (sname name);
   args = List.map node args;
   body = body}

let static_meth name args body =
  {Ast.method_name=`Static (sname name);
   args = List.map node args;
   body = body}

let meth name args body =
  public_meth name args body

let klass name super attrs methods =
  `Class {Ast.class_name=name;
	  super = super;
	  attrs = List.map node attrs;
	  methods = methods}

let define x expr =
  `Define (x,expr)

let redefine x n expr =
  `ReDefine (x,n,expr)

let define_class k super attrs =
  `DefineClass {ClosTrans.class_name = k;
		super = super;
		attrs = List.map node attrs}

let define_method f self obj args body =
  `DefineMethod {ClosTrans.method_name = node f;
		 to_class = obj;
		 args = List.map node (self::args);
		 body = body}

let define_static_method f obj args body =
  `DefineStaticMethod {ClosTrans.method_name = node f;
		       to_class = obj;
		       args = List.map node args;
		       body = body}


let external_var name =
  `External (name)

let external_class k methods =
  `ExternalClass (k,List.map node methods)

let module_ name exports xs =
  `Module {ModuleTrans.module_name=sname name;
	   exports=exports;
	   stmts=xs}

let foo_mod xs =
  module_ "foo" `All xs

let bar_mod xs =
  module_ "bar" `All xs


