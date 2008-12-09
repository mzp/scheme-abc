(*
Example:
 (package A :export '(f g h))
 (define (f ..) ...)

 (package B)
 (A.f 42)

Spec:
 - import/openは考えない
 -- 今後、このモジュールで拡張できるようにはする
 - export可能なのは、defineとclassのみ
 - stmtの変換
 -- define/classの名前を修飾されたやつに
 - exprの変換
 -- var/callなど識別子を利用するやつ
 -- 修飾されてないやつは、自分のモジュール

TODO:
 - BindCheckとどう統合するか？
 -- moduleを展開(名前解決)したあと、チェックする？

Flow:
 Clos -> [[ModuleTrans]] -> BindCheck -> Closure -> Codegen

*)

type stmt =
    [ `Class  of Ast.ident * Ast.name * Ast.attr list * Ast.method_ list
    | `Define of Ast.ident * Ast.expr
    | `Expr   of Ast.expr
    | `Module of Ast.ident * Ast.ident list * stmt ]


let trans x = x
