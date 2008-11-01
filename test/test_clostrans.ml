open Base
open ClosTrans
open Ast

let assert_equal x y =
  OUnit.assert_equal ~printer:Ast.to_string_stmt x y

let the_one =
  function [x] -> x
    | _ -> failwith "list is not one"

test trans =
    let expect = Class ("Foo",("bar","Baz"),
			[("f",["self";"x"],Int 42)]) in
    let source = [DefineClass ("Foo",("bar","Baz"),[]);
		  DefineMethod ("f",("self","Foo"),["x"],Int 42)] in
      assert_equal expect @@ the_one @@ trans source

test plain_is_not_change =
    assert_equal (Expr (Int 42)) @@ the_one @@ trans [Plain (Expr (Int 42))]
