type reloc = int -> int

val reloc_cpool :
  < string: reloc; namespace : reloc; namespace_set : reloc; ..> -> Swflib.AbcType.cpool -> Swflib.AbcType.cpool
val reloc :
  < int : reloc; uint : reloc; double : reloc; string : reloc; namespace : reloc; namespace_set : reloc; multiname : reloc; classes : reloc; methods : reloc;.. > -> Swflib.Abc.t -> Swflib.Abc.t
