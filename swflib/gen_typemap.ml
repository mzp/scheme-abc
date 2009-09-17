open Base
open Printf

let print_type name ocaml =
  if name <> ocaml then
    printf "type %s = %s\n" name ocaml

let print_let  prefix name body =
  printf "let %s_%s = %s\n" prefix name body

type map = (string*string) list

type t = {
  name:string;
  types:map;
  funs:map;
}

let (=>) a b = (a,b)
let map = ref []
let regist name ~low ~high ~funs =
  map := {
    name  = name;
    types = ["low",low;"high",high];
    funs  = funs
  }::!map;;

let none =
  "fun _ -> None"

let cpool name ~high ~entry =
  regist name ~low:"int" ~high
    ~funs:[
      "byte"  => "BytesOut.u30";
      "read"  => "BytesIn.u30";
      "const" => sprintf "fun x -> Some ((%s x) :> Cpool.entry)" entry;
      "arg"   => sprintf "fun ctx x -> Cpool.index ctx#cpool (%s x)" entry;
      "class" => none;
      "method"=> none;
    ];;

let literal name =
  regist name ~low:"int" ~high:"int"
    ~funs:[
      "byte"  => sprintf "BytesOut.%s" name;
      "read"  => sprintf "BytesIn.%s"  name;
      "const" => none;
      "arg"   => "fun _ -> id";
      "class" => none;
      "method"=> none;
    ];;

(* type regist *)
regist "method_" ~low:"int" ~high:"method_"
    ~funs:[
      "byte" => "BytesOut.u30";
      "read" => "BytesIn.u30";
      "const"  => none;
      "method" => "fun x -> Some x";
      "class"  => none;
      "arg"    => "fun ctx x -> index x ctx#methods"
    ];;

regist "class_" ~low:"int" ~high:"class_"
  ~funs:[
    "byte" => "BytesOut.u30";
    "read" => "BytesIn.u30";
    "const"  => none;
    "method" => none;
    "class"  => "fun x -> Some x";
    "arg"    => "fun ctx x -> index x ctx#classes"
  ];;

literal "u8";;
literal "u30";;
regist "label" ~low:"(Label.t,int) either" ~high:"Label.t"
  ~funs:[
    "byte" => "function
                   Left  label   -> label_ref label
                 | Right address -> s24 address";
    "read"   => "fun s -> Right (BytesIn.s24 s)";
    "const"  => none;
    "method" => none;
    "class"  => none;
    "arg"    => "fun _ x -> Left x"
  ];;

regist "label_def" ~low:"Label.t" ~high:"Label.t"
  ~funs:[
    "byte" => "fun l ->label l";
    "read"   => "fun _ -> Label.make()";
    "const"  => none;
    "method" => none;
    "class"  => none;
    "arg"    => "fun _ -> id"
  ];;

cpool "c_int"     ~high:"int"             ~entry:"`Int";;
cpool "c_uint"    ~high:"int"             ~entry:"`UInt";;
cpool "c_string"  ~high:"string"          ~entry:"`String";;
cpool "c_float"   ~high:"float"           ~entry:"`Double";;
cpool "namespace" ~high:"Cpool.namespace" ~entry:"";;
cpool "multiname" ~high:"Cpool.multiname" ~entry:"";;

let print_field t fs =
  ListLabels.iter !map ~f:begin fun {name;types;funs}->
    print_type name @@ List.assoc t types;
    List.iter (fun f -> print_let f name @@ List.assoc f funs) fs
  end

let _ =
  match Sys.argv.(1) with
      "-low" ->
	print_field "low" ["byte";"read"]
    | "-high" ->
	print_field "high" ["const";
			    "arg";
			    "class";
			    "method"]
    | _ ->
	failwith "usage: gen_typemap TYPE"


