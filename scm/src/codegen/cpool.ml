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
  int: int ISet.t;
  uint: int ISet.t;
  double: float ISet.t;
  string: string ISet.t;
  namespace: namespace ISet.t;
  namespace_set: namespace_set ISet.t;
  multiname: multiname ISet.t;
}

let empty =
  {int           = ISet.empty;
   uint          = ISet.empty;
   double        = ISet.empty;
   string        = ISet.empty;
   namespace     = ISet.empty;
   namespace_set = ISet.empty;
   multiname     = ISet.empty}

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

let add_multiname name cpool =
  match name with
      `QName (ns,str) ->
	{cpool with
	   string    = cpool.string
	               +> ISet.add str
		       +> ISet.add (ns_name ns);
	   namespace = ISet.add ns cpool.namespace;
	   multiname = ISet.add name cpool.multiname }
    | `Multiname (str,ns_set) ->
	{cpool with
	   string        = cpool.string
	                   +> ISet.add_list (List.map ns_name ns_set)
			   +> ISet.add str;
	   namespace     = ISet.add_list ns_set cpool.namespace;
	   namespace_set = ISet.add ns_set cpool.namespace_set;
	   multiname     = ISet.add name cpool.multiname }

let add entry cpool =
  match entry with
      `Int n ->
	{ cpool with int= ISet.add n cpool.int }
    | `UInt n ->
	{ cpool with uint= ISet.add n cpool.uint }
    | `String s ->
	{ cpool with string = ISet.add s cpool.string }
    | `Double d ->
	{ cpool with double = ISet.add d cpool.double }
    | #multiname as m ->
	add_multiname m cpool

(* conversion *)
(*
  assumption:
  - list has only unique element
*)
let rindex x set =
  1 + ISet.index x set

let index entry cpool =
  match entry with
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

let of_namespace {string=string} (ns : namespace) =
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

let of_namespace_set {namespace=namespace} nss =
  List.map (fun ns -> rindex ns namespace) nss

let of_multiname {namespace=namespace; namespace_set=namespace_set; string=string} : multiname -> Abc.multiname =
  function
      `QName (ns,s) ->
	Abc.QName (rindex ns namespace, rindex s string)
    | `Multiname (s,nss) ->
	Abc.Multiname (rindex s string,rindex nss namespace_set)

let to_abc cpool =
  { Abc.int           = ISet.to_list cpool.int;
    Abc.uint          = ISet.to_list cpool.uint;
    Abc.double        = ISet.to_list cpool.double;
    Abc.string        = ISet.to_list cpool.string;
    Abc.namespace     = cpool.namespace
                        +> ISet.to_list
                        +> List.map (of_namespace cpool);
    Abc.namespace_set = cpool.namespace_set
                        +> ISet.to_list
                        +> List.map (of_namespace_set cpool);
    Abc.multiname     = cpool.multiname
                        +> ISet.to_list
                        +> List.map (of_multiname cpool)
  }
