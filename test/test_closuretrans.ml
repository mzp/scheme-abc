open Base
open Ast
open Util
open ClosureTrans

test closure_with_args =
    OUnit.assert_equal
      ~printer:(fun x-> (Std.dump (List.map Ast.to_string_stmt x)))
      [Define ("f",Lambda (["x"],(Let (["x",Var "x"],Block [Lambda ([],Block [Var "x"])]))))]
      (trans @@ compile_string "(define (f x) (lambda () x))")

