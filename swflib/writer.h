type method_ = int
let write_method_= u30
type class_ = int
let write_class_= u30
type c_int = int
let write_c_int= u30
type c_uint = int
let write_c_uint= u30
type c_string = int
let write_c_string= u30
type c_float = int
let write_c_float= u30
type namespace = int
let write_namespace= u30
type multiname = int
let write_multiname= u30
type u30 = int
let write_u30= u30
type u8 = int
let write_u8= u8
type label = (Label.t,int) either
let write_label= function
                   Left  label   -> label_ref label
                 | Right address -> s24 address
