open Base
open Cpool
open Bytes

module Set = Core.Std.Set
type 'a set = 'a Set.t

type klass_type = Sealed | Final | Interface | ProtectedNs of Cpool.namespace

type instruction =
#include "opcode.ml"
 and meth = {
  name:   multiname;
  params: int list;
  return: int;
  flags:int;
  instructions:instruction list;
  traits: int list;
  exceptions: int list;
} and klass = {
   cname:     multiname;
   sname:     multiname;
   flags_k:   klass_type list;
   cinit:     meth;
   iinit:     meth;
   interface: klass list;
   methods:   meth list
}

type context = {
  cpool:   Cpool.t;
  meths:   meth list;
  klasses: klass list;
}

type config = {
  op:     int;
  args:   context -> Bytes.t list;
  prefix: context -> Bytes.t list;
  const:  Cpool.t;
  meth:   meth  list;
  klass:  klass option;
  stack:  int;
  scope:  int;
  count:  int;
}

let default = {
  op=0;
  args=const [];
  prefix=const [];
  const= Cpool.empty;
  meth  = [];
  klass = None;
  stack=0;
  scope=0;
  count=0;
}

let method_const {name=name} = 
  multiname name

let klass_const {cname=cname;
		 sname=sname;
		 flags_k=flags;
		 cinit=cinit;
		 iinit=iinit;
		 methods=methods} = 
  HList.fold_left1
    Cpool.append
  @@ List.concat [
    [try 
       match List.find (function ProtectedNs _ -> true | _ -> false ) flags with
	   ProtectedNs ns -> namespace ns 
	 | _ -> failwith "must not happen"
     with Not_found -> Cpool.empty];
    List.map multiname [cname;sname];
    List.map method_const methods]
    

#include "match.ml"
