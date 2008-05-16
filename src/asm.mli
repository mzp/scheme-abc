include Types.S
type meth = {
  name: string;
  params: int list;
  return: int;
  flags:int;
  instructions:instruction list;
  traits: int list;
  exceptions: int list;
}

val assemble : meth list -> Abc.cpool * Abc.method_info list * Abc.method_body list

