open Base
open Util
open OUnit
open AstUtil
open InterCode

let v_ok variables program =
  let tbl =
    add_program empty "Foo" @@ program in
    ok true @@
      List.for_all (flip InterCode.mem_variable tbl) variables

let v_ng variables program =
  let tbl =
    add_program empty "Foo" @@ program in
    ok false @@
      List.for_all (flip InterCode.mem_variable tbl) variables

let m_ok methods program =
  let tbl =
    add_program empty "Foo" @@ program in
    ok true @@
      List.for_all (flip InterCode.mem_method tbl) methods

let m_ng methods program =
  let tbl =
    add_program empty "Foo" @@ program in
    ok true @@
      List.for_all (flip InterCode.mem_method tbl) methods

let _ =
  ("interCode.ml" >::: [
     "'define' should export its name" >::
       (fun () ->
	  v_ok ["Foo.Foo","x"] [
	    module_ "Foo" `All
	      [define (sname "x") (int 42)]];
	  v_ng ["Foo.Foo","x"] [
	    module_ "Foo" (`Only [])
	      [define (sname "x") (int 42)]]);
     "'class' should export its name" >::
       (fun () ->
	  v_ok ["Foo.Foo","Bar"]  [
	    module_ "Foo" `All
	      [klass (sname "Bar") (global "Object") [] []]];
	  v_ng ["Foo.Foo","Foo"] [
	    module_ "Foo"  (`Only [])
	      [klass (sname "Foo") (global "Object") [] []]]);
     "'class' should export its member methods" >::
       (fun () ->
	  let k m =
	    klass (sname "Bar") (global "Object") [] m in
	    m_ok ["f"]
	      [k [meth "f" [] (int 42)]]);
   ]) +> run_test_tt
