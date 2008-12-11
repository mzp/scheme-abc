open Base
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

type stmt_term =
    [ `Class  of Ast.sname * Ast.qname * Ast.attr list * Ast.method_ list
    | `Define of Ast.sname * Ast.expr
    | `Expr   of Ast.expr
    | `ExternalClass of Ast.sname * Ast.sname list
    | `External of Ast.sname]

type stmt =
    [stmt_term | `Module of Ast.sname * Ast.sname list * stmt list ]

let (++) ({Node.value = ns} as loc) ({Node.value=name;end_pos=pos}) =
  {loc with
     Node.value = (ns,name);
     end_pos = pos}

let rec trans_stmt ns : stmt -> BindCheck.stmt list =
  function
      `Class  (klass,super,attrs,methods) ->
	[`Class (ns ++ klass,super,attrs,methods)]
    | `Define (name,body) ->
	[`Define (ns ++ name,body)]
    | `External name ->
	[`External (ns ++ name)]
    | `ExternalClass (klass,methods) ->
	[`ExternalClass (ns++klass,methods)]
    | `Expr _ as expr ->
	[expr]
    | `Module (ns,_,stmts) ->
	HList.concat_map (trans_stmt ns) stmts

let trans =
  HList.concat_map (trans_stmt (Node.ghost ""))
