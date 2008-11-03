open Base
open ClosTrans
open Ast

let assert_equal x y =
  OUnit.assert_equal ~printer:(string_of_list $ List.map Ast.to_string_stmt) x y

test trans =
  let expect = [Class ("Foo",("bar","Baz"),[],
		      [("f",["self";"x"],Int 42)])] in
  let source = [DefineClass ("Foo",("bar","Baz"),[]);
		DefineMethod ("f",("self","Foo"),["x"],Int 42)] in
    assert_equal expect @@ trans source

test plain_is_not_change =
  assert_equal [Expr (Int 42)] @@ trans [Plain (Expr (Int 42))]

test trans_with_mix =
  let expect = [Class ("Foo",("bar","Baz"),[],
		      [("f",["self";"x"],Int 42)]);
	        Expr (Int 42)] in
  let source = [DefineClass ("Foo",("bar","Baz"),[]);
		Plain (Expr (Int 42));
		DefineMethod ("f",("self","Foo"),["x"],Int 42)] in
    assert_equal expect @@ trans source

test invoke =
  let expect = 
    [Class ("Foo",("bar","Baz"),[],
	    [("f",["self";"x"],Int 42)]);
     Expr (Invoke (Var "obj","f",[Int 10]))] in
  let source = 
    [DefineClass ("Foo",("bar","Baz"),[]);
     DefineMethod ("f",("self","Foo"),["x"],Int 42);
     Plain (Expr (Call [Var "f";Var "obj";Int 10]))] in
    assert_equal expect @@ trans source
