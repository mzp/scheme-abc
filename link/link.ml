open Base
open Swflib.AbcType

let method_sigs n ms =
  List.map (fun m -> {m with method_sig= n + m.method_sig} ) ms

let link a1 a2 =
  let ctx = {|
    int     = (+) @@ List.length a1.cpool.int;
    uint          = (+) @@ List.length a1.cpool.uint;
    double        = (+) @@ List.length a1.cpool.double;
    string        = (+) @@ List.length a1.cpool.string;
    namespace     = (+) @@ List.length a1.cpool.namespace;
    namespace_set = (+) @@ List.length a1.cpool.namespace_set;
    multiname     = (fun i -> if i = 0 then 0 else i + List.length a1.cpool.multiname);
    methods       = (+) @@ List.length a1.method_info;
    classes       = (+) @@ List.length a1.classes
  |} in
  let a2 =
    Reloc.do_abc ctx a2 in
    {a1 with
       cpool = {
	 int           = a1.cpool.int           @ a2.cpool.int;
	 uint          = a1.cpool.uint          @ a2.cpool.uint;
	 double        = a1.cpool.double        @ a2.cpool.double;
	 string        = a1.cpool.string        @ a2.cpool.string;
	 namespace     = a1.cpool.namespace     @ a2.cpool.namespace;
	 namespace_set = a1.cpool.namespace_set @ a2.cpool.namespace_set;
	 multiname     = a1.cpool.multiname     @ a2.cpool.multiname
       };
       method_info   = a1.method_info   @ a2.method_info;
       method_bodies = a1.method_bodies @ method_sigs (List.length a1.method_info) a2.method_bodies;
       scripts       = a1.scripts       @ a2.scripts;
       classes       = a1.classes       @ a2.classes;
       instances     = a1.instances     @ a2.instances;
    }
