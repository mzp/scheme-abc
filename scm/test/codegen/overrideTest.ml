open Base
open Ast
open Override
open AstUtil
open OUnit

let ok x y =
  assert_equal x (Override.of_binding y)

let foo = Node.ghost ([],"Foo")
let bar = Node.ghost ([],"Bar")
let obj = Node.ghost ([],"Object")

let class_ name super meths = AstUtil.class_ (`Public name) super [] meths
let meth name = public_meth name ["self"] @@ block []

let _ = ("override.ml" >::: [
	   "non-override" >::
	     (fun () ->
		ok
		  [class_ foo obj [meth "f",[]]]
		  [class_ foo obj [meth "f"]]);
	   "primitive" >::
	     (fun () ->
		ok
		  [class_ foo obj [meth "toString",[]]]
		  [class_ foo obj [meth "toString"]]);
	   "non-override" >::
	     (fun () ->
		ok
		  [class_ foo obj [meth "f",[]];
		   class_ bar foo [meth "f",[`Override]]]
		  [class_ foo obj [meth "f"];
		   class_ bar foo [meth "f"];]);
	   "mix" >::
	     (fun () ->
		ok
		  [class_ foo obj [meth "f",[]];
		   class_ bar foo [meth "f",[`Override];
				   meth "g",[]]]
		  [class_ foo obj [meth "f"];
		   class_ bar foo [meth "f"; meth "g"];]);
	 ]) +> run_test_tt
