open ExtList

open Base
open Swflib.AbcType

let nth xs i =
  List.nth xs (i+1)


let compact_cpool cpool =
  {cpool with
     int       = List.unique cpool.int;
     uint      = List.unique cpool.uint;
     double    = List.unique cpool.double;
     string    = List.unique cpool.string;
  }

let compact abc = {
  abc with
    cpool = compact_cpool abc.cpool
}
