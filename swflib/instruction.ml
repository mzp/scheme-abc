(* CAUTION: this is a generated file.  If you edit it, all changes will be lost! *)
# 1 "instruction.mlp"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "instruction.mlp"
open Base
open Bytes


# 1 "write_type.h" 1
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
# 5 "instruction.mlp" 2
type t = [

# 1 "type.h" 1
| `NewFunction of method_
| `NewClass of class_
| `Coerce
| `Coerce_a
| `Coerce_s
| `Convert_i
| `Convert_s
| `Convert_d
| `Convert_b
| `Convert_u
| `Add_i
| `Subtract_i
| `Multiply_i
| `Add
| `Subtract
| `Multiply
| `Divide
| `Modulo
| `Equals
| `StrictEquals
| `LessThan
| `LessEquals
| `GreaterThan
| `GreaterEquals
| `Label of label
| `IfNlt of label
| `IfNle of label
| `IfNgt of label
| `IfNge of label
| `Jump of label
| `IfTrue of label
| `IfFalse of label
| `IfEq of label
| `IfNe of label
| `IfLt of label
| `IfLe of label
| `IfGt of label
| `IfGe of label
| `IfStrictEq of label
| `IfStrictNe of label
| `PushNull
| `PushUndefined
| `PushByte of u8
| `PushShort of u30
| `PushTrue
| `PushFalse
| `PushNaN
| `PushString of c_string
| `PushInt of c_int
| `PushUInt of c_uint
| `PushDouble of c_float
| `PushNamespace of namespace
| `PushScope
| `PushWith
| `GetGlobalScope
| `GetScopeObject of u8
| `GetLocal_0
| `GetLocal_1
| `GetLocal_2
| `GetLocal_3
| `GetLocal of u30
| `SetLocal_0
| `SetLocal_1
| `SetLocal_2
| `SetLocal_3
| `SetLocal of u30
| `GetSlot of u30
| `SetSlot of u30
| `GetGlobalSlot of u30
| `SetGlobalSlot of u30
| `GetLex of multiname
| `GetProperty of multiname
| `SetProperty of multiname
| `InitProperty of multiname
| `ReturnVoid
| `ReturnValue
| `FindPropStrict of multiname
| `CallProperty of multiname*u30
| `CallPropLex of multiname*u30
| `Call of u30
| `Pop
| `Swap
| `PopScope
| `NewObject of u30
| `NewArray of u30
| `NewActivation
| `Dup
| `ConstructSuper of u30
| `ConstructProp of multiname*u30
# 7 "instruction.mlp" 2
]

let to_bytes =
  function

# 1 "write.h" 1
| `NewFunction (arg0) -> [u8 0x40; write_method_ arg0]
| `NewClass (arg0) -> [u8 0x58; write_class_ arg0]
| `Coerce  -> [u8 0x80; ]
| `Coerce_a  -> [u8 0x82; ]
| `Coerce_s  -> [u8 0x85; ]
| `Convert_i  -> [u8 0x73; ]
| `Convert_s  -> [u8 0x74; ]
| `Convert_d  -> [u8 0x75; ]
| `Convert_b  -> [u8 0x76; ]
| `Convert_u  -> [u8 0x77; ]
| `Add_i  -> [u8 0xc5; ]
| `Subtract_i  -> [u8 0xc6; ]
| `Multiply_i  -> [u8 0xc7; ]
| `Add  -> [u8 0xa0; ]
| `Subtract  -> [u8 0xa1; ]
| `Multiply  -> [u8 0xa2; ]
| `Divide  -> [u8 0xa3; ]
| `Modulo  -> [u8 0xa4; ]
| `Equals  -> [u8 0xab; ]
| `StrictEquals  -> [u8 0xac; ]
| `LessThan  -> [u8 0xad; ]
| `LessEquals  -> [u8 0xae; ]
| `GreaterThan  -> [u8 0xaf; ]
| `GreaterEquals  -> [u8 0xb0; ]
| `Label (arg0) -> [u8 0x9; write_label arg0]
| `IfNlt (arg0) -> [u8 0xc; write_label arg0]
| `IfNle (arg0) -> [u8 0xd; write_label arg0]
| `IfNgt (arg0) -> [u8 0xe; write_label arg0]
| `IfNge (arg0) -> [u8 0xf; write_label arg0]
| `Jump (arg0) -> [u8 0x10; write_label arg0]
| `IfTrue (arg0) -> [u8 0x11; write_label arg0]
| `IfFalse (arg0) -> [u8 0x12; write_label arg0]
| `IfEq (arg0) -> [u8 0x13; write_label arg0]
| `IfNe (arg0) -> [u8 0x14; write_label arg0]
| `IfLt (arg0) -> [u8 0x15; write_label arg0]
| `IfLe (arg0) -> [u8 0x16; write_label arg0]
| `IfGt (arg0) -> [u8 0x17; write_label arg0]
| `IfGe (arg0) -> [u8 0x18; write_label arg0]
| `IfStrictEq (arg0) -> [u8 0x19; write_label arg0]
| `IfStrictNe (arg0) -> [u8 0x1a; write_label arg0]
| `PushNull  -> [u8 0x20; ]
| `PushUndefined  -> [u8 0x21; ]
| `PushByte (arg0) -> [u8 0x24; write_u8 arg0]
| `PushShort (arg0) -> [u8 0x25; write_u30 arg0]
| `PushTrue  -> [u8 0x26; ]
| `PushFalse  -> [u8 0x27; ]
| `PushNaN  -> [u8 0x28; ]
| `PushString (arg0) -> [u8 0x2c; write_c_string arg0]
| `PushInt (arg0) -> [u8 0x2d; write_c_int arg0]
| `PushUInt (arg0) -> [u8 0x2e; write_c_uint arg0]
| `PushDouble (arg0) -> [u8 0x2f; write_c_float arg0]
| `PushNamespace (arg0) -> [u8 0x31; write_namespace arg0]
| `PushScope  -> [u8 0x30; ]
| `PushWith  -> [u8 0x1c; ]
| `GetGlobalScope  -> [u8 0x64; ]
| `GetScopeObject (arg0) -> [u8 0x65; write_u8 arg0]
| `GetLocal_0  -> [u8 0xd0; ]
| `GetLocal_1  -> [u8 0xd1; ]
| `GetLocal_2  -> [u8 0xd2; ]
| `GetLocal_3  -> [u8 0xd3; ]
| `GetLocal (arg0) -> [u8 0x62; write_u30 arg0]
| `SetLocal_0  -> [u8 0xd4; ]
| `SetLocal_1  -> [u8 0xd5; ]
| `SetLocal_2  -> [u8 0xd6; ]
| `SetLocal_3  -> [u8 0xd7; ]
| `SetLocal (arg0) -> [u8 0x63; write_u30 arg0]
| `GetSlot (arg0) -> [u8 0x6c; write_u30 arg0]
| `SetSlot (arg0) -> [u8 0x6d; write_u30 arg0]
| `GetGlobalSlot (arg0) -> [u8 0x6e; write_u30 arg0]
| `SetGlobalSlot (arg0) -> [u8 0x6f; write_u30 arg0]
| `GetLex (arg0) -> [u8 0x60; write_multiname arg0]
| `GetProperty (arg0) -> [u8 0x66; write_multiname arg0]
| `SetProperty (arg0) -> [u8 0x61; write_multiname arg0]
| `InitProperty (arg0) -> [u8 0x68; write_multiname arg0]
| `ReturnVoid  -> [u8 0x47; ]
| `ReturnValue  -> [u8 0x48; ]
| `FindPropStrict (arg0) -> [u8 0x5d; write_multiname arg0]
| `CallProperty (arg0,arg1) -> [u8 0x46; write_multiname arg0;write_u30 arg1]
| `CallPropLex (arg0,arg1) -> [u8 0x4c; write_multiname arg0;write_u30 arg1]
| `Call (arg0) -> [u8 0x41; write_u30 arg0]
| `Pop  -> [u8 0x29; ]
| `Swap  -> [u8 0x2b; ]
| `PopScope  -> [u8 0x1d; ]
| `NewObject (arg0) -> [u8 0x55; write_u30 arg0]
| `NewArray (arg0) -> [u8 0x56; write_u30 arg0]
| `NewActivation  -> [u8 0x57; ]
| `Dup  -> [u8 0x2a; ]
| `ConstructSuper (arg0) -> [u8 0x49; write_u30 arg0]
| `ConstructProp (arg0,arg1) -> [u8 0x4a; write_multiname arg0;write_u30 arg1]
# 12 "instruction.mlp" 2
