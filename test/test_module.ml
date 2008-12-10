open Base
open OUnit
open ModuleTrans
open AstUtil

let ok x y =
  OUnit.assert_equal
    x @@ trans y

let sname =
  node

let define x y =
  `Define (x,y)

let _ =
  ("moduleTrans.ml" >::: [
     "define trans" >::
       (fun () ->
	  ok
	    [define (node ("foo","bar")) (`Block [])]
	    [`Module (sname "foo",[],[
			define (sname "bar") (`Block []) ] ) ]);
     "class trans" >::
       (fun () ->
	  ok
	    [`Class (node ("foo","bar"),qname "Object",[],[])]
	    [`Module (sname "foo",[],[
			`Class (sname "bar",qname "Object",[],[])])])
   ]) +> run_test_tt


