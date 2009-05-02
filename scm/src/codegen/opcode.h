| ConstructProp of Cpool.multiname*int
| ConstructSuper of int
| NewClass of klass
| Dup
| NewFunction of meth
| NewActivation
| NewArray of int
| NewObject of int
| PopScope
| Swap
| Pop
| Call of int
| CallPropLex of Cpool.multiname*int
| CallProperty of Cpool.multiname*int
| FindPropStrict of Cpool.multiname
| ReturnValue
| ReturnVoid
| InitProperty of Cpool.multiname
| SetProperty of Cpool.multiname
| GetProperty of Cpool.multiname
| GetLex of Cpool.multiname
| SetGlobalSlot of int
| GetGlobalSlot of int
| SetSlot of int
| GetSlot of int
| SetLocal of int
| SetLocal_3
| SetLocal_2
| SetLocal_1
| SetLocal_0
| GetLocal of int
| GetLocal_3
| GetLocal_2
| GetLocal_1
| GetLocal_0
| GetScopeObject of int
| GetGlobalScope
| PushWith
| PushScope
| PushNamespace of Cpool.namespace
| PushDouble of float
| PushUInt of int
| PushInt of int
| PushString of string
| PushNaN
| PushFalse
| PushTrue
| PushShort of int
| PushByte of int
| PushUndefined
| PushNull
| IfStrictNe of Label.t
| IfStrictEq of Label.t
| IfGe of Label.t
| IfGt of Label.t
| IfLe of Label.t
| IfLt of Label.t
| IfNe of Label.t
| IfEq of Label.t
| IfFalse of Label.t
| IfTrue of Label.t
| Jump of Label.t
| IfNge of Label.t
| IfNgt of Label.t
| IfNle of Label.t
| IfNlt of Label.t
| Label of Label.t
| GreaterEquals
| GreaterThan
| LessEquals
| LessThan
| StrictEquals
| Equals
| Modulo
| Divide
| Multiply
| Subtract
| Add
| Multiply_i
| Subtract_i
| Add_i
| Coerce_s
| Coerce_a
| Coerce
