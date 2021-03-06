exception Unbound_var    of (string list * string) Node.t
exception Forbidden_var  of (string list * string) Node.t
exception Unbound_method of string Node.t

class type table = object
  method mem_symbol : (string list * string) -> bool
  method mem_method : string -> bool
  method mem_module : string list -> bool
end

val bind : #table -> Ast.program -> Ast.program

