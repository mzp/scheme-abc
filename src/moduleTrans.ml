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

type program = stmt list

let (++) ns ({Node.value=name} as loc) =
  {loc with
     Node.value = (String.concat "." ns,name)}

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
    | `Module ({Node.value=name},_,stmts) ->
	HList.concat_map (trans_stmt (ns@[name])) stmts

let rec lift f : stmt -> stmt =
  function
      `Class (klass,super,attrs,methods) ->
	let methods' =
	  List.map (Tuple.T3.map3 f) methods in
	  `Class (klass,super,attrs,methods')
    | `Define (name,body) ->
	`Define (name,f body)
    | `External _ | `ExternalClass _ as e ->
	e
    | `Expr expr ->
	`Expr (f expr)
    | `Module (name,exports,stmts) ->
	`Module (name,exports,List.map (lift f) stmts)

let trans =
  HList.concat_map (trans_stmt [])
