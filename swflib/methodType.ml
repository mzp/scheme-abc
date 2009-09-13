open AbcType

type function_scope =
    [ `Global
    | `Class of Cpool.multiname]

type class_type     =
    [ `Sealed
    | `Final
    | `Interface
    | `ProtectedNs of Cpool.namespace]

type 'a method_ = {
    method_name:  Cpool.multiname;
    params:       int list;
    return:       int;
    method_flags: int;
    code:         'a list;
    traits:       int list;
    exceptions:   int list;
    fun_scope:    function_scope;
    method_attrs: [`Override | `Final] list
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
  attrs:       Cpool.multiname list
}

let empty = {
  method_attrs = [];
  method_name        = `QName (`Namespace "","");
  params             = [];
  return             = 0;
  method_flags       = 0;
  code = [];
  traits             = [];
  exceptions         = [];
  fun_scope          = `Global
}
