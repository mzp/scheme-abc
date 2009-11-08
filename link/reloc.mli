open Swflib.AbcType

type reloc = int -> int

val do_cpool :
  < string: reloc; namespace : reloc; namespace_set : reloc; ..> -> cpool -> cpool
val do_namespace : < string: reloc; .. > -> namespace list -> namespace list
val do_namespace_set : < string: reloc; namespace : reloc; .. > -> namespace_set list -> namespace_set list
val do_multiname :
  < string: reloc; namespace : reloc; namespace_set : reloc; ..> -> multiname list -> multiname list


val do_abc :
  < int : reloc; uint : reloc; double : reloc; string : reloc;
    namespace : reloc; namespace_set : reloc; multiname : reloc;
    classes : reloc; methods : reloc;.. > -> Swflib.Abc.t -> Swflib.Abc.t
