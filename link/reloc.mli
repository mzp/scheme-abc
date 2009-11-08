type reloc = int -> int
type t = {
  int       : reloc;
  uint      : reloc;
  double    : reloc;
  string    : reloc;
  namespace     : reloc;
  namespace_set : reloc;
  multiname : reloc;
  methods   : reloc;
  classes   : reloc
}

val reloc : t -> Swflib.Abc.t -> Swflib.Abc.t
