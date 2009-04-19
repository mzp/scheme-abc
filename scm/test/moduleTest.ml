open Base
open OUnit
open ModuleTrans
open AstUtil

let ok x y =
  OUnit.assert_equal
    x @@ trans y

let define x =
  `Define (x,`Block [])

let _ =
  ("moduleTrans.ml" >::: [
     "public_symbols" >:::[
       "define" >::
	 (fun () ->
	    assert_equal [qname "foo" "bar"] @@
	      public_symbols [foo_mod [define (sname "bar")]]);
       "class" >::
	 (fun () ->
	    assert_equal [qname "foo" "Bar"] @@
	      public_symbols [foo_mod [
				klass (sname "Bar") (global "Object") [] []
			      ]]);
       "global" >::
	 (fun () ->
	    assert_equal [qname "" "bar"] @@
	      public_symbols [define (sname "bar")])
     ];
     "public_methods" >::: [
       "class" >::
	 (fun () ->
	    assert_equal [sname "f"] @@
	      public_methods [foo_mod [
				klass (sname "Bar") (global "Object") [] [
				  public_meth "f" [] @@ `Block []
				]
			      ]])
     ];
     "define trans" >::
       (fun () ->
	  ok
	    [define (`Public (qname "foo" "bar"))]
	    [foo_mod [
	       define (sname "bar")]]);
     "class trans" >::
       (fun () ->
	  ok
	    [klass (`Public (qname "foo" "Bar")) (global "Object") [] []]
	    [foo_mod [
	       klass (sname "Bar") (global "Object") [] []
	     ]]);
     "baz should be internal" >::
       (fun () ->
	  ok
	    [define (`Public (qname "foo" "bar"));
	     define (`Internal (qname "foo" "baz"))]
	    [module_ "foo" (`Only [sname "bar"]) [
	       define (sname "bar");
	       define (sname "baz")]]);
     "Baz should be internal" >::
       (fun () ->
	  ok
	    [klass (`Public (qname "foo" "Bar"))   (global "Object") [] [];
	     klass (`Internal (qname "foo" "Baz")) (global "Object") [] []] @@
	    [module_ "foo" (`Only [sname "Bar"]) [
	       klass (sname "Bar") (global "Object") [] [];
	       klass (sname "Baz") (global "Object") [] []]])
   ]) +> run_test_tt
