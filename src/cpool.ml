open Base

type namespace =
    Namespace of string
  | PackageNamespace of string
  | PackageInternalNamespace of string
  | ProtectedNamespace of string
  | ExplicitNamespace of string
  | StaticProtectedNamespace of string
  | PriavteNamespace of string
type namespace_set = namespace list

type multiname =
    QName of namespace * string
  | Multiname of string * namespace_set

type 'a set = 'a PSet.t

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
  {int           = PSet.empty;
   uint          = PSet.empty;
   double        = PSet.empty;
   string        = PSet.empty;
   namespace     = PSet.empty;
   namespace_set = PSet.empty;
   multiname     = PSet.empty}

let to_string {int=n; uint=un; double=d; string=str; namespace=ns; namespace_set=nss; multiname=mname} =
  let dump x =
    Std.dump @@ PSet.to_list x in
  Printf.sprintf "{int=%s; uint=%s; double=%s; string=%s; namespace=%s; namespace_set=%s; multiname=%s}"
    (dump n)
    (dump un)
    (dump d)
    (dump str)
    (dump ns)
    (dump nss)
    (dump mname)

let append x y =
  lift2 {app=PSet.union} x y

let int x = {
  empty with int=PSet.singleton x
}

let uint x = {
  empty with uint=PSet.singleton x
}

let string x = {
  empty with string=PSet.singleton x
}

let double x = {
  empty with double=PSet.singleton x
}

let ns_name =
  function
      Namespace name
    | PackageNamespace name
    | PackageInternalNamespace name
    | ProtectedNamespace name
    | ExplicitNamespace name
    | StaticProtectedNamespace name
    | PriavteNamespace name ->
	name

let namespace x = {
  empty with
    namespace=PSet.singleton x;
    string=PSet.singleton @@ ns_name x
}

let multiname name=
  match name with
      QName (ns,str) ->
	{empty with
	   string    = PSet.of_list [ns_name ns; str];
	   namespace = PSet.singleton ns;
	   multiname = PSet.singleton name }
    | Multiname (str,ns_set) ->
	{empty with
	   string        = PSet.of_list @@ str :: List.map ns_name ns_set ;
	   namespace     = PSet.of_list ns_set;
	   namespace_set = PSet.singleton ns_set;
	   multiname     = PSet.singleton name }

(* conversion *)
let index x xs =
  1+Base.index x xs

let of_namespace ~string ns =
  let i =
    index (ns_name ns) string in
  let kind =
    match ns with
	Namespace _ ->
	  0x08
      | PackageNamespace _ ->
	  0x16
      | PackageInternalNamespace _ ->
	  0x17
      | ProtectedNamespace _ ->
	  0x18
      | ExplicitNamespace _ ->
	  0x19
      | StaticProtectedNamespace _ ->
	  0x1A
      | PriavteNamespace _ ->
	  0x05 in
    {Abc.kind=kind; ns_name=i}

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
    PSet.to_list tbl.int,
    PSet.to_list tbl.uint,
    PSet.to_list tbl.double,
    PSet.to_list tbl.string,
    PSet.to_list tbl.namespace,
    PSet.to_list tbl.namespace_set in
  let ns' =
    List.map (of_namespace ~string:str) ns in
  let nss' =
    List.map (of_namespace_set ~namespace:ns' ~string:str) nss in
  let mname =
    List.map (of_multiname ~string:str ~namespace:ns' ~namespace_set:nss') @@ PSet.to_list tbl.multiname in
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
    index value @@ PSet.to_list @@ f map in
  let get value map =
    index_u30 value @@ PSet.to_list @@ f map in
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

let make_qname ?(ns="") x =
  QName ((Namespace ns),x)
