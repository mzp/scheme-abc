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
val define_scope :
  string ->
  env -> (env -> Asm.instruction list) -> env * Asm.instruction list
val define_class : string -> Asm.klass -> env -> env * Asm.instruction list
val var_ref : string -> env -> Asm.instruction list
val var_call :
  string -> Asm.instruction list list -> env -> Asm.instruction list
