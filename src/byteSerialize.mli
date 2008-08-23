(**
   Byte serializer for {!Abc}.
*)
val to_bytes : Abc.abc -> Bytes.t list

(**{6 Debug only}*)

val of_cpool : Abc.cpool -> Bytes.t list
val of_method_info : Abc.method_info -> Bytes.t list
val of_script : Abc.script -> Bytes.t list
val of_trait : Abc.trait -> Bytes.t list
val of_method_body : Abc.method_body -> Bytes.t list

val of_class : Abc.class_info -> Bytes.t list
val of_instance : Abc.instance_info -> Bytes.t list
