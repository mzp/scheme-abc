open Base
open Bytes

include Instruction

type t = {
  abc_cpool:     Abc.cpool;
  method_info:   Abc.method_info list;
  method_body:   Abc.method_body list;
  class_info:    Abc.class_info  list;
  instance_info: Abc.instance_info list
}

(* data flow *)
let fork2 f g x       = (f x, g x)
let fork3 f g h x     = (f x, g x, h x)
let fork4 f g h i x   = (f x, g x, h x, i x)

let with2 f g (a,b) = (f a, g b)
let with3 f g h (a,b,c) = (f a, g b, h c)
let with4 f g h i (a,b,c,d) = (f a, g b, h c, i d)

let join2 f (a,b)     = f a b
let join3 f (a,b,c)   = f a b c
let join4 f (a,b,c,d) = f a b c d

(* map_accum *)
type inst =
  [ `InstanceMethod of method_
  | `StaticMethod of method_
  | instruction ]

let method_ =
  function
      `InstanceMethod m | `StaticMethod m ->
	Some m
    | #instruction as inst ->
	((spec inst).method_)

let class_ =
  function
      `InstanceMethod m | `StaticMethod m ->
	None
    | #instruction as inst ->
	((spec inst).class_)

let rec fold f ctx inst =
  let ctx_method =
    match method_ inst with
	Some {instructions=instructions} ->
	  List.fold_left (fold f) ctx (instructions :> inst list)
      | None ->
	  ctx in
  let ctx_class =
    match class_ inst with
	Some { instance_methods = im; static_methods = sm } ->
	  let ctx' =
	    List.fold_left (fun ctx m -> fold f ctx (`InstanceMethod m)) ctx_method im in
	    List.fold_left (fun ctx m -> fold f ctx (`StaticMethod m)) ctx' sm
      | None ->
	  ctx_method in
    f ctx_class inst

let assemble m =
  undefined
