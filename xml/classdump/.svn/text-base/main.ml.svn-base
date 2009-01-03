open Base

let string cpool i =
  List.nth cpool#string @@ Int32.to_int i - 1

let namespace cpool i =
  match List.nth cpool#namespace @@ (Int32.to_int i - 1) with
      `ExplicitNamespace i ->
	Printf.sprintf "[explicit %s]" @@ string cpool i
    | `Namespace i ->
       Printf.sprintf "%s" @@ string cpool i
    | `PackageInternaNs i ->
	Printf.sprintf "[internal %s]" @@ string cpool i
    | `PackageNamespace i ->
	Printf.sprintf "%s" @@ string cpool i
    | `PrivateNs i ->
	Printf.sprintf "[private %s]" @@ string cpool i
    | `ProtectedNamespace i ->
	Printf.sprintf "[protected %s]" @@ string cpool i
    | `StaticProtectedNs i ->
	Printf.sprintf "[static protected %s]" @@ string cpool i

let namespace_set cpool i =
  string_of_list @@ List.map (namespace cpool) @@ 
    (List.nth cpool#ns_set @@ Int32.to_int i - 1)#ns

let multiname cpool i =
  let ns,name = 
    match List.nth cpool#multiname @@ Int32.to_int i - 1 with
	`QName obj ->
	  (namespace cpool obj#ns),(string cpool obj#name)
      | `Multiname obj ->
	  (namespace_set cpool obj#ns_set),(string cpool obj#name)
      | _ ->
	  failwith "not yet" in
    if ns = "" then
      name
    else
      ns ^ "." ^name

let method_info abc i =
  let m =
    List.nth abc#methods @@ Int32.to_int i in
    multiname abc#constant_pool m#name

let method_trait trait =
  match trait#data with
      `Method obj 
    | `Getter obj
    | `Setter obj ->
	[obj#methodi]
    | _ ->
	[]


let instance_list abc =
  let cpool =
    abc#constant_pool in
    List.map (fun i -> (multiname cpool i#name,
			List.map (fun t -> multiname cpool t#name) i#traits))
      abc#instances

let dump file =
  let abc =
    Abc.of_stream @@ Byte.of_channel @@ open_in_bin file in
    instance_list abc

let _ =
  let argv = 
    Array.to_list Sys.argv in
    match argv with
      _::xs ->
	List.iter (List.iter (fun (klass,methods)->
				Printf.printf "(external-class %s (%s))\n" klass @@ String.concat " " methods) $ dump) xs
      | [] ->
	  failwith "must not happen"

