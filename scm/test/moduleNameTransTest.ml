open Base
open OUnit
open ModuleNameTrans
open AstUtil

let ok x y =
  OUnit.assert_equal
    x @@ trans y

let _ =
  ("moduleNameTrans.ml" >::: [
     "module" >::
       (fun () ->
	  ok
	    [module_ "foo" (`All) []]
	    [module_ "Foo" (`All) []]);
     "super Class" >::
       (fun () ->
	  ok
	    [klass (sname "Baz") (qname "foo" "Object") [] []]
	    [klass (sname "Baz") (qname "Foo" "Object") [] []]);
     "var" >::
       (fun () ->
	  ok
	    [expr @@ var @@ qname "foo" "x"]
	    [expr @@ var @@ qname "Foo" "x"]);
     "new" >::
       (fun () ->
	  ok
	    [expr @@ new_klass (qname "foo" "x") []]
	    [expr @@ new_klass (qname "Foo" "x") []]);
     "nested module" >::
       (fun () ->
	  ok
	    [expr @@ var @@ qname "foo.bar" "x"]
	    [expr @@ var @@ qname "Foo.Bar" "x"])
   ]) +> run_test_tt
