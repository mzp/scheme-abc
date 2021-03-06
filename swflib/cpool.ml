open Base
open AbcType

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

let add x xs=
  if RevList.mem x xs then
    xs
  else
    RevList.add x xs

let add_list xs ys =
  RevList.add_list (List.filter (fun x -> not (RevList.mem x ys)) xs) ys

let add_namespace ns cpool =
  {cpool with
     string = cpool.string
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

let add cpool entry =
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

let index cpool entry =
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
    match ns with
	`Namespace _ ->
	  Namespace i
      | `PackageNamespace _ ->
	  PackageNamespace i
      | `PackageInternalNamespace _ ->
	  PackageInternalNamespace i
      | `ProtectedNamespace _ ->
	  ProtectedNamespace i
      | `ExplicitNamespace _ ->
	  ExplicitNamespace i
      | `StaticProtectedNamespace _ ->
	  StaticProtectedNamespace i
      | `PrivateNamespace _ ->
	  PrivateNamespace i

let of_namespace_set {namespace=namespace} nss =
  List.map (fun ns -> rindex ns namespace) nss

let of_multiname {namespace=namespace; namespace_set=namespace_set; string=string} =
  function
      `QName (ns,s) ->
	QName (rindex ns namespace, rindex s string)
    | `Multiname (s,nss) ->
	Multiname (rindex s string,rindex nss namespace_set)

let to_abc cpool =
  { AbcType.int   =
      RevList.to_list cpool.int;
    uint          =
      RevList.to_list cpool.uint;
    double        =
      RevList.to_list cpool.double;
    string        =
      RevList.to_list cpool.string;
    namespace     =
      cpool.namespace
      +> RevList.to_list
      +> List.map (of_namespace cpool);
    namespace_set =
      cpool.namespace_set
      +> RevList.to_list
      +> List.map (of_namespace_set cpool);
    multiname     =
      cpool.multiname
      +> RevList.to_list
      +> List.map (of_multiname cpool)
  }

let add_list cpool xs =
  List.fold_left add cpool xs
