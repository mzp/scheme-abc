open Base

type namespace = [
  `Namespace of string
| `PackageNamespace of string
| `PackageInternalNamespace of string
| `ProtectedNamespace of string
| `ExplicitNamespace of string
| `StaticProtectedNamespace of string
| `PrivateNamespace of string ]

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
| namespace
| multiname
]

type t = {
  int: int RevList.t;
  uint: int RevList.t;
  double: float RevList.t;
  string: string RevList.t;
  namespace: namespace RevList.t;
  namespace_set: namespace_set RevList.t;
  multiname: multiname RevList.t;
}

let empty =
  {int           = RevList.empty;
   uint          = RevList.empty;
   double        = RevList.empty;
   string        = RevList.empty;
   namespace     = RevList.empty;
   namespace_set = RevList.empty;
   multiname     = RevList.empty}

let ns_name =
  function
      `Namespace name
    | `PackageNamespace name
    | `PackageInternalNamespace name
    | `ProtectedNamespace name
    | `ExplicitNamespace name
    | `StaticProtectedNamespace name
    | `PrivateNamespace name ->
	name

let add x xs =
  if RevList.mem x xs then
    xs
  else
    RevList.add x xs

let add_list xs ys =
  RevList.add_list (List.filter (fun x -> not (RevList.mem x ys)) xs) ys

let add_namespace ns cpool =
  {cpool with
     string    = cpool.string
                 +> add (ns_name ns);
     namespace = add ns cpool.namespace }

let add_multiname name cpool =
  match name with
      `QName (ns,str) ->
	let cpool =
	  {cpool with
	     string    = cpool.string
	                 +> add str;
	     multiname = add name cpool.multiname } in
	  add_namespace ns cpool
      | `Multiname (str,ns_set) ->
	{cpool with
	   string        = cpool.string
	                   +> add_list (List.map ns_name ns_set)
			   +> add str;
	   namespace     = add_list ns_set cpool.namespace;
	   namespace_set = add ns_set cpool.namespace_set;
	   multiname     = add name cpool.multiname }

let add entry cpool =
  match entry with
      `Int n ->
	{ cpool with int= add n cpool.int }
    | `UInt n ->
	{ cpool with uint= add n cpool.uint }
    | `String s ->
	{ cpool with string = add s cpool.string }
    | `Double d ->
	{ cpool with double = add d cpool.double }
    | #namespace as ns ->
	add_namespace ns cpool
    | #multiname as m ->
	add_multiname m cpool

(* conversion *)
(*
  assumption:
  - list has only unique element
*)
let rindex x set =
  1 + RevList.index x set

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
    | #namespace as ns ->
	rindex ns cpool.namespace
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
      | `PrivateNamespace _ ->
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
  { Abc.int           = RevList.to_list cpool.int;
    Abc.uint          = RevList.to_list cpool.uint;
    Abc.double        = RevList.to_list cpool.double;
    Abc.string        = RevList.to_list cpool.string;
    Abc.namespace     = cpool.namespace
                        +> RevList.to_list
                        +> List.map (of_namespace cpool);
    Abc.namespace_set = cpool.namespace_set
                        +> RevList.to_list
                        +> List.map (of_namespace_set cpool);
    Abc.multiname     = cpool.multiname
                        +> RevList.to_list
                        +> List.map (of_multiname cpool)
  }
