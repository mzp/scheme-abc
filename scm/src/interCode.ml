(** intermediate code *)
type inter_code = {
  variables: Ast.qname list;
  methods: Ast.qname list;
  program: Ast.program
}

let of_program program = {
  variables = [];
  methods   = [];
  program   = program
}

