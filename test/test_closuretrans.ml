open Base
open Ast
open Util
open Closuretrans

let dump =
  function
      Define (x,y) ->
	Printf.sprintf "Define (%s,%s)" x (Ast.to_string y)
    | Expr x ->
	Printf.sprintf "Expr (%s)" (Ast.to_string x)
test closure_with_args =
    OUnit.assert_equal
      ~printer:(fun x-> (Std.dump (List.map dump x)))
      [Define ("f",Lambda (["x"],(Let (["x",Var "x"],Block [Lambda ([],Block [Var "x"])]))))]
      (trans @@ Lisp.compile_string "(define (f x) (lambda () x))")

