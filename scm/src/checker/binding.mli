exception Unbound_var    of (string list * string) Type.Node.t
exception Forbidden_var  of (string list * string) Type.Node.t
exception Unbound_method of string Type.Node.t

class type table = object
  method mem_symbol : string list * string -> bool
  method mem_method : string -> bool
end

val check : table -> Type.Ast.program -> Type.Ast.program

