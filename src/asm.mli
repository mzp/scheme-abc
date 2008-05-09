type namespace = Namespace of string | PackageNamespace of string
type namespace_set = namespace list
type multiname = QName of namespace * string | Multiname of string * namespace_set

type instruction = 
    GetLocal 
  | PushScope 
  | ReturnVoid 
  | FindPropStrict of multiname
  | PushString of string 
  | PushInt of int
  | CallPropLex of multiname * int
  | Pop

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

