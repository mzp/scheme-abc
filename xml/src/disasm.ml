open Base
open Byte

let op n stream =
  match Stream.peek stream with
      Some m when m = n ->
	Stream.next stream
    | _ ->
	raise Stream.Failure

let code =
  parser
      [< _ = op 0xc5 >] ->
	`Add_i
    | [< _ = op 0x86; index = u30 >] ->
	`AsType index
    | [< _ = op 0x87 >] ->
	`AsTypeLate
    | [< _ = op 0xa8 >] ->
	`BitAnd
    | [< _ = op 0x97 >] ->
	`BitNot
    | [< _ = op 0xa9 >] ->
	`BitOr
    | [< _ = op 0xaa >] ->
	`BitXor
    | [< _ = op 0x41; arg_count = u30 >] ->
	`Call arg_count
    | [< _ = op 0x43; index = u30; arg_count = u30 >] ->
	`CallMethod (index,arg_count)
    | [< _ = op 0x46; index = u30; arg_count = u30 >] ->
	`CallProperty (index,arg_count)
    | [< _ = op 0x4c; index = u30; arg_count = u30 >] ->
	`CallPropLex (index,arg_count)
    | [< _ = op 0x4f; index = u30; arg_count = u30 >] ->
	`CallPropVoid (index,arg_count)
    | [< _ = op 0x44; index = u30; arg_count = u30 >] ->
	`CallStatic (index,arg_count)
    | [< _ = op 0x45; index = u30; arg_count = u30 >] ->
	`CallSuper (index,arg_count)
    | [< _ = op 0x4e; index = u30; arg_count = u30 >] ->
	`CallSuperVoid (index,arg_count)
    | [< _ = op 0x78 >] ->
	`CheckFilter
    | [< _ = op 0x80; index = u30 >] ->
	`Coerce index
    | [< _ = op 0x82 >] ->
	`Coerce_a
    | [< _ = op 0x85 >] ->
	`Coerce_s
    | [< _ = op 0x42; arg_count = u30 >] ->
	`Construct arg_count
    | [< _ = op 0x4a; index = u30; arg_count = u30 >] ->
	`ConstructProp (index,arg_count)
    | [< _ = op 0x49; arg_count = u30 >] ->
	`ConstructSuper arg_count
    | [< _ = op 0x76 >] ->
	`Convert_b
    | [< _ = op 0x73 >] ->
	`Convert_i
    | [< _ = op 0x75 >] ->
	`Convert_d
    | [< _ = op 0x77 >] ->
	`Convert_o
    | [< _ = op 0x74 >] ->
	`Convert_u
    | [< _ = op 0x70 >] ->
	`Convert_s
    | [< _ = op 0xef; debug_type = u8; index = u30; reg = u8; extra = u30 >] ->
	`Debug (debug_type,index,reg,extra)
    | [< _ = op 0xf1; index = u30 >] ->
	`DebugFile index
    | [< _ = op 0xf0; linenum = u30 >] ->
	`DebugLine linenum
    | [< _ = op 0x94; index = u30 >] ->
	`DecLocal index
    | [< _ = op 0xc3; index = u30 >] ->
	`DecLocal_i index
    | [< _ = op 0x93 >] ->
	`Decrement
    | [< _ = op 0xc1 >] ->
	`Decrement_i
    | [< _ = op 0x6a; name = u30 >] ->
	`DeleteProperty name
    | [< _ = op 0xa3 >] ->
	`Divide
    | [< _ = op 0x2a >] ->
	`Dup
    | [< _ = op 0x06; string = u30 >] ->
	`Dxns string
    | [< _ = op 0x07 >] ->
	`DxnsLate
    | [< _ = op 0xab >] ->
	`Equals
    | [< _ = op 0x72 >] ->
	`Esc_xattr
    | [< _ = op 0x71 >] ->
	`Esc_xelem
    | [< _ = op 0x5f; _ = u30 >] ->
	failwith "finddef: undocumented instruction"
    | [< _ = op 0x5e; name = u30 >] ->
	`FindProperty name
    | [< _ = op 0x5d; name = u30 >] ->
	`FindPropStrict name
    | [< _ = op 0x59; name = u30 >] ->
	`GetDescendants name
    | [< _ = op 0x64 >] ->
	`GetGlobalScope
    | [< _ = op 0x6e; slot_id = u30 >] ->
	`GetGlobalSlot slot_id
    | [< _ = op 0x60; name = u30 >] ->
	`GetLex name
    | [< _ = op 0x62; index = u30 >] ->
	`GetLocal index
    | [< _ = op 0xD0 >] ->
	`GetLocal_0
    | [< _ = op 0xD1 >] ->
	`GetLocal_1
    | [< _ = op 0xD2 >] ->
	`GetLocal_2
    | [< _ = op 0xD3 >] ->
	`GetLocal_3
    | [< _ = op 0x66; name = u30 >] ->
	`GetProperty name
    | [< _ = op 0x65; index = u8 >] ->
	`GetScopeObject index
    | [< _ = op 0x6c; slot_id = u30 >] ->
	`GetSlot slot_id
    | [< _ = op 0x04; name = u30 >] ->
	`GetSuper name
    | [< _ = op 0xb0 >] ->
	`GreaterEquals 
    | [< _ = op 0xaf >] ->
	`GreaterThan
    | [< _ = op 0x1f >] ->
	`HasNext
    | [< _ = op 0x32; object_reg = u32; index_reg = u32 >] ->
	`HasNext2 (object_reg,index_reg)
    | [< _ = op 0x13; offset = s24 >] ->
	`IfEq offset
    | [< _ = op 0x12; offset = s24 >] ->
	`IfFalse offset
    | [< _ = op 0x17; offset = s24 >] ->
	`IfGt offset
    | [< _ = op 0x16; offset = s24 >] ->
	`IfLe offset
    | [< _ = op 0x15; offset = s24 >] ->
	`IfLt offset
    | [< _ = op 0x15; offset = s24 >] ->
	`IfLt offset
    | [< _ = op 0x0f; offset = s24 >] ->
	`IfNge offset
    | [< _ = op 0x0e; offset = s24 >] ->
	`IfNgt offset
    | [< _ = op 0x0d; offset = s24 >] ->
	`IfNle offset
    | [< _ = op 0x0c; offset = s24 >] ->
	`IfNlt offset
    | [< _ = op 0x14; offset = s24 >] ->
	`IfNe offset
    | [< _ = op 0x19; offset = s24 >] ->
	`IfStrictEq offset
    | [< _ = op 0x1a; offset = s24 >] ->
	`IfStrictNe offset
    | [< _ = op 0x11; offset = s24 >] ->
	`IfTrue offset
    | [< _ = op 0xb4 >] ->
	`In
    | [< _ = op 0x92; index = u30 >] ->
	`IncLocal index
    | [< _ = op 0xc2; index = u30 >] ->
	`IncLocal_i index
    | [< _ = op 0x91 >] ->
	`Increment
    | [< _ = op 0xc0 >] ->
	`Increment_i
    | [< _ = op 0x68; index = u30 >] ->
	`InitProperty index
    | [< _ = op 0xb1 >] ->
	`InstanceOf
    | [< _ = op 0xb2; index = u30 >] ->
	`IsType index
    | [< _ = op 0xb3 >] ->
	`IsTypeLate
    | [< _ = op 0x10; offset = s24 >] ->
	`Jump offset
    | [< _ = op 0x08; index = u30 >] ->
	`Kill index
    | [< _ = op 0x09 >] ->
	`Label
    | [< _ = op 0xae >] ->
	`LessEquals
    | [< _ = op 0xad >] ->
	`LessThan
    | [< _ = op 0x1b; 
	 default_offset = s24; 
	 case_count = u30; 
	 case_offsets = Parsec.repeat_l (Int32.add case_count 1l) s24 >] ->
	`LookupSwitch (default_offset,case_offsets)
    | [< _ = op 0xa5 >] ->
	`LShift
    | [< _ = op 0xa4 >] ->
	`Modulo
    | [< _ = op 0xa2 >] ->
	`Multiply
    | [< _ = op 0xc7 >] ->
	`Multiply_i
    | [< _ = op 0x90 >] ->
	`Negate
    | [< _ = op 0xc4 >] ->
	`Negate_i
    | [< _ = op 0x57 >] ->
	`NewActivation
    | [< _ = op 0x56; arg_count = u30 >] ->
	`NewArray arg_count
    | [< _ = op 0x5a; index = u30 >] ->
	`NewCatch index
    | [< _ = op 0x58; index = u30 >] ->
	`NewClass index
    | [< _ = op 0x40; index = u30 >] ->
	`NewFunction index
    | [< _ = op 0x55; arg_count = u30 >] ->
	`NewObject arg_count
    | [< _ = op 0x1e >] ->
	`NextName
    | [< _ = op 0x23 >] ->
	`NextValue
    | [< _ = op 0x02 >] ->
	`Nop
    | [< _ = op 0x96 >] ->
	`Not
    | [< _ = op 0x29 >] ->
	`Pop
    | [< _ = op 0x1d >] ->
	`PopScope
    | [< _ = op 0x24; byte_value = u8 >] ->
	`PushByte byte_value
    | [< _ = op 0x2f; index = u30 >] ->
	`PushDouble index
    | [< _ = op 0x27 >] ->
	`PushFalse
    | [< _ = op 0x2d; index = u30 >] ->
	`PushInt index
    | [< _ = op 0x31; index = u30 >] ->
	`PushNamespace index
    | [< _ = op 0x28 >] ->
	`PushNan
    | [< _ = op 0x20 >] ->
	`PushNull
    | [< _ = op 0x30 >] ->
	`PushScope
    | [< _ = op 0x25; value = u30 >] ->
	`PushShort value
    | [< _ = op 0x2c; index = u30 >] ->
	`PushString index
    | [< _ = op 0x26 >] ->
	`PushTrue
    | [< _ = op 0x2e; index = u30 >] ->
	`PushUInt index
    | [< _ = op 0x21 >] ->
	`PushUndefined
    | [< _ = op 0x1c >] ->
	`PushWith
    | [< _ = op 0x48 >] ->
	`ReturnValue
    | [< _ = op 0x47 >] ->
	`ReturnVoid
    | [< _ = op 0xa6 >] ->
	`RShift
    | [< _ = op 0x63; index = u30 >] ->
	`SetLocal index
    | [< _ = op 0xd4 >] ->
	`SetLocal_0
    | [< _ = op 0xd5 >] ->
	`SetLocal_1
    | [< _ = op 0xd6 >] ->
	`SetLocal_2
    | [< _ = op 0xd7 >] ->
	`SetLocal_3
    | [< _ = op 0x6f; slot_index = u30 >] ->
	`SetGlobalSlot slot_index
    | [< _ = op 0x61; index = u30 >] ->
	`SetProperty index
    | [< _ = op 0x6d; slot_index = u30 >] ->
	`SetSlot slot_index
    | [< _ = op 0x05; index = u30 >] ->
	`SetSuper index
    | [< _ = op 0xac >] ->
	`StrictEquals
    | [< _ = op 0xa1 >] ->
	`Subtract
    | [< _ = op 0xc6 >] ->
	`Subtract_i
    | [< _ = op 0x2b >] ->
	`Swap
    | [< _ = op 0x03 >] ->
	`Throw
    | [< _ = op 0x95 >] ->
	`TypeOf
    | [< _ = op 0xa7 >] ->
	`URShift
