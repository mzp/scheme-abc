open ExtList

open Base
open Swflib.AbcType

let rec index x xs i =
  match xs with
      y::ys ->
	if x = y then i
	else index x ys (i+1)
    | [] ->
	failwith (Std.dump x)

let index x xs = index x xs 1

let make_compact before after i =
  if i = 0 then 0
  else
    let x =
      List.nth before (i-1) in
      index x after

let compact_for xs reloc =
  let ys =
    reloc xs in
  let zs =
    List.unique ys in
    zs,make_compact ys zs

let compact_cpool cpool =
  let int,int_c =
    compact_for cpool.int id in
  let uint,uint_c =
    compact_for cpool.uint id in
  let double,double_c =
    compact_for cpool.double id in
  let str,str_c =
    compact_for cpool.string id in
  let ns,ns_c =
    compact_for cpool.namespace @@
      Reloc.do_namespace {| string = str_c |} in
  let nss,nss_c =
    compact_for cpool.namespace_set @@
      Reloc.do_namespace_set {| string = str_c; namespace = ns_c |} in
  let mname,mname_c =
    compact_for cpool.multiname @@
      Reloc.do_multiname {| string = str_c; namespace =ns_c; namespace_set = nss_c |} in
  let cpool = {
      int           = int;
      uint          = uint;
      double        = double;
      string        = str;
      namespace     = ns;
      namespace_set = nss;
      multiname     = mname;
    } in
  let relocs = {|
      int           = int_c;
      uint          = uint_c;
      double        = double_c;
      string        = str_c;
      namespace     = ns_c;
      namespace_set = nss_c;
      multiname     = mname_c;
      classes = id;
      methods = id
  |} in
    relocs,cpool

let compact abc =
  let relocs, cpool =
    compact_cpool abc.cpool in
    { Reloc.do_abc relocs abc with cpool = cpool }
