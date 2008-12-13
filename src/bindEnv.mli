(** Enviroment operation *)

(** enviroment type *)
type env

(**{6 Default enviroment}*)
val empty_env : env
val script_bootstrap : 'a -> Asm.instruction list * env

val arguments : string Node.t list -> (env -> int list -> 'a) -> 'a
val arguments_self : string Node.t list -> (env -> int list -> 'a) -> 'a

val let_scope :
  env ->
  (string Node.t * Asm.instruction list) list ->
  (env -> Asm.instruction list) -> Asm.instruction list
val let_rec_scope :
  env ->
  (string Node.t * (env -> Asm.instruction list)) list ->
  (env -> Asm.instruction list) -> Asm.instruction list
val define_scope : Ast.stmt_name -> env -> (env -> Asm.instruction list) -> env * Asm.instruction list

val qname_of_stmt_name : Ast.stmt_name -> Cpool.multiname
val define_class : Ast.stmt_name -> Asm.klass -> env -> env * Asm.instruction list
val var_ref : string * string -> env -> Asm.instruction list
val var_call : string * string -> Asm.instruction list list -> env -> Asm.instruction list

