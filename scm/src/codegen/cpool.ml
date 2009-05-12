open Base

type namespace = [
  `Namespace of string
| `PackageNamespace of string
| `PackageInternalNamespace of string
| `ProtectedNamespace of string
| `ExplicitNamespace of string
| `StaticProtectedNamespace of string
| `PriavteNamespace of string ]

type namespace_set = namespace list

type multiname = [
  `QName of namespace * string
| `Multiname of string * namespace_set
]

type entry = [
| `Int of int
| `UInt of int
| `Double of float
| `String of string
| multiname
]

type t = {
  int: int list;
  uint: int list;
  double: float list;
  string: string list;
  namespace: namespace list;
  namespace_set: namespace_set list;
  multiname: multiname list;
}

let empty =
  {int           = [];
   uint          = [];
   double        = [];
   string        = [];
   namespace     = [];
   namespace_set = [];
   multiname     = []}

let ns_name =
  function
      `Namespace name
    | `PackageNamespace name
    | `PackageInternalNamespace name
    | `ProtectedNamespace name
    | `ExplicitNamespace name
    | `StaticProtectedNamespace name
    | `PriavteNamespace name ->
	name

let (^::) x xs =
  if List.mem x xs then
    xs
  else
    x::xs

let (^@) xs ys =
  List.fold_right (^::) xs ys

let add_multiname cpool name =
  match name with
      `QName (ns,str) ->
	{cpool with
	   string    = (ns_name ns) ^:: str ^:: cpool.string;
	   namespace = ns ^:: cpool.namespace;
	   multiname = name ^:: cpool.multiname }
    | `Multiname (str,ns_set) ->
	{cpool with
	   string        = str ^:: (List.map ns_name ns_set ^@ cpool.string);
	   namespace     = ns_set ^@ cpool.namespace;
	   namespace_set = ns_set ^:: cpool.namespace_set;
	   multiname     = name ^:: cpool.multiname }

let add cpool : entry -> t =
  function
      `Int n ->
	{ cpool with int= n ^:: cpool.int }
    | `UInt n ->
	{ cpool with uint= n ^:: cpool.uint }
    | `String s ->
	{ cpool with string = s ^:: cpool.string }
    | `Double d ->
	{ cpool with double = d ^::cpool.double }
    | #multiname as m ->
	add_multiname cpool m

(* conversion *)
(*
  assumption:
  - list has only unique element
*)
let rec rindex x =
  function
      [] ->
	failwith "empty list(must not happen)"
    | y::ys ->
	if x = y then
	  1 + List.length ys
	else
	  rindex x ys

let index cpool : entry -> int =
  function
      `Int n ->
	rindex n cpool.int
    | `UInt n ->
	rindex n cpool.uint
    | `Double d ->
	rindex d cpool.double
    | `String s ->
	rindex s cpool.string
    | #multiname as m ->
	rindex m cpool.multiname

let of_namespace ~string (ns : namespace) =
  let i =
    rindex (ns_name ns) string in
  let kind =
    match ns with
	`Namespace _ ->
	  0x08
      | `PackageNamespace _ ->
	  0x16
      | `PackageInternalNamespace _ ->
	  0x17
      | `ProtectedNamespace _ ->
	  0x18
      | `ExplicitNamespace _ ->
	  0x19
      | `StaticProtectedNamespace _ ->
	  0x1A
      | `PriavteNamespace _ ->
	  0x05 in
    {Abc.kind=kind; namespace_name=i}

let of_namespace_set ~string ~namespace nss =
  List.map (fun ns -> rindex (of_namespace ~string:string ns) namespace) nss

let of_multiname ~string ~namespace ~namespace_set : multiname -> Abc.multiname =
  function
      `QName (ns,s) ->
	Abc.QName (rindex (of_namespace ~string:string ns) namespace, rindex s string)
    | `Multiname (s,nss) ->
	Abc.Multiname (rindex s string,rindex (of_namespace_set ~string:string ~namespace:namespace nss) namespace_set)

let to_abc cpool =
  let int,uint,double,str,ns,nss =
    cpool.int,
    cpool.uint,
    cpool.double,
    cpool.string,
    cpool.namespace,
    cpool.namespace_set in
  let ns' =
    List.map (of_namespace ~string:str) ns in
  let nss' =
    List.map (of_namespace_set ~namespace:ns' ~string:str) nss in
  let mname =
    List.map (of_multiname ~string:str ~namespace:ns' ~namespace_set:nss') cpool.multiname in
    { Abc.int           = List.rev int;
      Abc.uint          = List.rev uint;
      Abc.double        = List.rev double;
      Abc.string        = List.rev str;
      Abc.namespace     = List.rev ns';
      Abc.namespace_set = List.rev nss';
      Abc.multiname     = List.rev mname
    }


