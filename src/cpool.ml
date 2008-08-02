open Base
module Set = Core.Std.Set

type namespace = 
    Namespace of string 
  | PackageNamespace of string
type namespace_set = namespace list

type multiname = 
    QName of namespace * string 
  | Multiname of string * namespace_set

type 'a set = 'a Set.t

type t = {
  int: int set;
  uint: int set;
  double: float set;
  string: string set;
  namespace: namespace set;
  namespace_set: namespace_set set;
  multiname: multiname set;
}

(* lift *)
type op = {app: 'a . 'a set -> 'a set -> 'a set}

let lift2 {app=f} x y =
  {int           = f x.int           y.int;
   uint          = f x.uint          y.uint;
   double        = f x.double        y.double;
   string        = f x.string        y.string;
   namespace     = f x.namespace     y.namespace;
   namespace_set = f x.namespace_set y.namespace_set;
   multiname     = f x.multiname     y.multiname}

let empty = 
  {int           = Set.empty;
   uint          = Set.empty;
   double        = Set.empty;
   string        = Set.empty;
   namespace     = Set.empty;
   namespace_set = Set.empty;
   multiname     = Set.empty }

let append x y = 
  lift2 {app=Set.union} x y

let int x = {
  empty with int=Set.singleton x
}

let uint x = {
  empty with uint=Set.singleton x
}

let string x = {
  empty with string=Set.singleton x
}

let double x = {
  empty with double=Set.singleton x
}

let ns_name = 
  function Namespace name | PackageNamespace name ->
    name

let namespace x = {
  empty with 
    namespace=Set.singleton x;
    string=Set.singleton @@ ns_name x
}

let multiname name= 
  match name with
      QName (ns,str) ->
	{empty with 
	   string    = Set.of_list [ns_name ns; str];
	   namespace = Set.singleton ns;
	   multiname = Set.singleton name }
    | Multiname (str,ns_set) ->
	{empty with
	   string        = Set.of_list @@ str :: List.map ns_name ns_set ;
	   namespace     = Set.of_list ns_set;
	   namespace_set = Set.singleton ns_set;
	   multiname     = Set.singleton name }

(* conversion *)    
let of_namespace ~string ns =
  let i =
    index (ns_name ns) string in
    match ns with
      Namespace name ->
	{Abc.kind=0x08; Abc.ns_name=i}
    | PackageNamespace name ->
	{Abc.kind=0x16; Abc.ns_name=i}

let of_namespace_set ~string ~namespace nss =
  List.map (fun ns -> index (of_namespace ~string:string ns) namespace) nss

let of_multiname ~string ~namespace ~namespace_set = 
  function 
      QName (ns,s) ->
	Abc.QName (index (of_namespace ~string:string ns) namespace,index s string)
    | Multiname (s,nss) ->
	Abc.Multiname (index s string,index (of_namespace_set ~string:string ~namespace:namespace nss) namespace_set)

let to_abc tbl = 
  let int,uint,double,str,ns,nss =
    Set.to_list tbl.int,
    Set.to_list tbl.uint,
    Set.to_list tbl.double,
    Set.to_list tbl.string,
    Set.to_list tbl.namespace,
    Set.to_list tbl.namespace_set in
  let ns' =
    List.map (of_namespace ~string:str) ns in
  let nss' =
    List.map (of_namespace_set ~namespace:ns' ~string:str) nss in
  let mname =
    List.map (of_multiname ~string:str ~namespace:ns' ~namespace_set:nss') @@ Set.to_list tbl.multiname in
    { Abc.int           = int;
      Abc.uint          = uint;
      Abc.double        = double;
      Abc.string        = str;
      Abc.namespace     = ns';
      Abc.namespace_set = nss';
      Abc.multiname     = mname
    }

let index_u30 x xs=
  Bytes.u30 @@ index x xs

let accessor f = 
  let nget value map =
    index value @@ Set.to_list @@ f map in
  let get value map =
    index_u30 value @@ Set.to_list @@ f map in
    nget,get

let int_nget,int_get =
  accessor (fun {int=map}->map)

let uint_nget,uint_get =
  accessor (fun {uint=map}->map)

let string_nget,string_get =
  accessor (fun {string=map}->map)

let double_nget,double_get =
  accessor (fun {double=map}->map)

let namespace_nget,namespace_get =
  accessor (fun {namespace=map}->map)

let multiname_nget,multiname_get =
  accessor (fun {multiname=map}->map)



