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
| `PushString of string
| `PushInt of int
| `PushUInt of uint
| `PushDouble of float
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
