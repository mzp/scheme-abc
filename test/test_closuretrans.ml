open Base
open Ast
open Util
open Closuretrans

test closure_with_args =
    OUnit.assert_equal
      ~printer:(fun x-> (Std.dump (List.map Ast.to_string_stmt x)))
      [Define ("f",Lambda (["x"],(Let (["x",Var "x"],Block [Lambda ([],Block [Var "x"])]))))]
      (trans @@ Lisp.compile_string "(define (f x) (lambda () x))")

