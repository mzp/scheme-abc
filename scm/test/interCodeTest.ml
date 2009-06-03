open Base
open OUnit
open InterCode

let int x =
  `Int (Node.ghost x)

let define name body : Ast.stmt' =
  `Define ((Node.ghost name),body)

let module_ name exports xs : Ast.stmt' =
  `Module {Ast.module_name = Node.ghost name;
	   exports         = exports;
	   stmts           = xs}

let class_ name methods =
  `Class {Ast.class_name =Node.ghost name;
	  super = Node.ghost ([],"Object");
	  attrs = [];
	  methods = methods}

let meth name args body =
  {Ast.method_name=`Public (Node.ghost name);
   args = List.map Node.ghost args;
   body = body}

let v_ok variables program =
  let tbl =
    add "Foo" program empty in
    assert_equal true @@
      List.for_all tbl#mem_symbol variables

let v_ng variables program =
  let tbl =
    add "Foo" program empty in
    assert_equal false @@
      List.for_all tbl#mem_symbol variables

let m_ok methods program =
  let tbl =
    add "Foo" program empty in
    assert_equal true @@
      List.for_all tbl#mem_method methods

let m_ng methods program =
  let tbl =
    add "Foo" program empty in
    assert_equal false @@
      List.for_all tbl#mem_method methods

let mo_ok name program =
  let tbl =
    add "Foo" program empty in
    assert_equal true @@
      List.for_all tbl#mem_module name

let mo_ng name program =
  let tbl =
    add "Foo" program empty in
    assert_equal false @@
      List.for_all tbl#mem_module name

let _ =
  ("interCode.ml" >::: [
     "mem_symbol" >::
       (fun () ->
	  v_ng [["Hoge";"Foo"],"x"] []);
     "'define' should export its name" >::
       (fun () ->
	  v_ok [["Foo";"Foo"],"x"] [
	    module_ "Foo" `All
	      [define "x" (int 42)]];
	  v_ng [["Foo";"Foo"],"x"] [
	    module_ "Foo" (`Only [])
	      [define "x" (int 42)]]);
     "'class' should export its name" >::
       (fun () ->
	  v_ok [["Foo";"Foo"],"Bar"]  [
	    module_ "Foo" `All
	      [class_ "Bar" []]];
	  v_ng [["Foo";"Foo"],"Foo"] [
	    module_ "Foo"  (`Only [])
	      [class_ "Foo" []]]);
     "'class' should export its member methods" >::
       (fun () ->
	  let k m =
	    class_ "Bar" m in
	    m_ok ["f"]
	      [k [meth "f" [] (int 42)]]);
     "mem_module" >::
       (fun () ->
	  mo_ok [["Foo";"Foo"]] [
	    module_ "Foo" `All
	      [define "x" (int 42)]];
	  mo_ng [["Foo";"Bar"]] [
	    module_ "Foo" (`Only [])
	      [define "x" (int 42)]]);
   ]) +> run_test_tt
