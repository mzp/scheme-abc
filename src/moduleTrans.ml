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
type method_ =
  Ast.sname * Ast.sname list * Ast.expr

type exports =
    All
  | Restrict of Ast.sname list

type 'stmt stmt_type =
    [ `Class  of Ast.sname * Ast.qname * Ast.attr list * method_ list
    | `Define of Ast.sname * Ast.expr
    | `Expr   of Ast.expr
    | `Module of Ast.sname * exports * 'stmt list ]

type stmt =
    stmt stmt_type

type program = stmt list

let (++) ns ({Node.value=name} as loc) =
  {loc with
     Node.value = (String.concat "." ns,name)}

let access exports ns name =
  let qname =
    ns ++ name in
  match exports with
      All ->
	`Public qname
    | Restrict names ->
	if List.exists (fun {Node.value=v} -> name.Node.value = v) names then
	  `Public qname
	else
	  `Internal qname



let rec trans_stmt ns exports : stmt -> Ast.stmt list =
  function
      `Class  (klass,super,attrs,methods) ->
	[`Class (access exports ns klass,super,attrs,methods)]
    | `Define (name,body) ->
	[`Define (access exports ns name,body)]
    | `Expr _ as expr ->
	[expr]
    | `Module ({Node.value=name},exports,stmts) ->
	HList.concat_map (trans_stmt (ns@[name]) exports) stmts

let rec lift f : stmt -> stmt =
  function
      `Class (klass,super,attrs,methods) ->
	let methods' =
	  List.map (Tuple.T3.map3 f) methods in
	  `Class (klass,super,attrs,methods')
    | `Define (name,body) ->
	`Define (name,f body)
    | `Expr expr ->
	`Expr (f expr)
    | `Module (name,exports,stmts) ->
	`Module (name,exports,List.map (lift f) stmts)

let trans =
  HList.concat_map (trans_stmt [] All)
