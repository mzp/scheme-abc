open Base
open Ast
open Util
open ClosureTrans

let ok x y =
  OUnit.assert_equal
    ~printer:(fun x-> (Std.dump (List.map Ast.to_string_stmt x) ^ "\n"))
    x y

test closure_with_args =
  ok
    [Define ("f",Lambda (["x"],(Let (["x",Var "x"],Block [Lambda ([],Block [Var "x"])]))))]
    (trans @@ compile_string "(define (f x) (lambda () x))")

test closure_with_class =
  ok [
    Class ("Foo",("","Object"),
	   ["init",["self"],
	      Let (["self",Var "self"],
		   Block [Lambda ([],Block [Var "self"])])])] @@
    trans @@ compile_string "(define-class Foo (Object) ())  (define-method init ((self Foo)) (lambda () self))"

