type function_scope =
    [ `Global
    | `Class of Cpool.multiname]

type class_type     =
    [ `Sealed
    | `Final
    | `Interface
    | `ProtectedNs of Cpool.namespace]

type 'a method_ = {
    method_name:     Cpool.multiname;
    params:          int list;
    return:          int;
    method_flags:    int;
    instructions:    'a list;
    traits:          int list;
    exceptions:      int list;
    fun_scope:       function_scope;
    method_attrs :   [`Override | `Final] list
}
type 'a class_ = {
  class_name:       Cpool.multiname;
  super:            Cpool.multiname;
  class_flags:      class_type list;
  cinit:            'a method_;
  iinit:            'a method_;
  interface:        'a class_ list;
  instance_methods: 'a method_ list;
  static_methods:   'a method_ list;
  attributes:       Cpool.multiname list
}

(*
  Because I want use structual subtyping, I use object as record.
*)
class type ['a] context = object
  method cpool:   Cpool.t
  method methods: 'a method_ RevList.t
  method classes: 'a class_ RevList.t
end

type 'a t = {
  op:     int;
  args:   'a context -> Bytes.t list;
  prefix: 'a context -> Bytes.t list;
  const:  Cpool.entry list;
  method_: 'a method_  option;
  class_ :  'a class_ option;
  stack  :  int;
  scope  :  int;
  count  :  int;
}

let empty_method = {
  method_attrs = [];
  method_name = `QName (`Namespace "","");
  params = [];
  return = 0;
  method_flags = 0;
  instructions = [];
  traits= [];
  exceptions= [];
  fun_scope= `Global
}
