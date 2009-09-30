open Base
open EasyXml

let label =
  function
      Left _ ->
	failwith "must not happen"
    | Right adr ->
	adr

let op name =
  attr name []

let op_a name attrs =
  attr name @@ List.map (fun (x,y) -> (x,string_of_int y)) attrs

let op_i name attrs =
  attr name @@ List.map (fun (x,y) -> (x,string_of_int y)) attrs

let to_xml = function
    `Add ->
      op "OpAdd"
  | `Add_i ->
      op "OpAddI"
  | `AsType index ->
      op_a "OpAsType" ["name",index]
  | `AsTypeLate ->
      op "OpAsTypeLate"
  | `BitAnd ->
      op "OpBitAnd"
  | `BitNot ->
      op "OpBitNot"
  | `BitOr ->
      op "OpBitOr"
  | `BitXor ->
      op "OpBitXor"
  | `Call argc ->
      op_a "OpCall" ["argc",argc]
  | `CallMethod (name,argc) ->
      op_a "OpCallMethod" ["name",name; "argc",argc]
  | `CallProperty (name,argc) ->
      op_a "OpCallProperty" ["name",name; "argc",argc]
  | `CallPropLex (name,argc) ->
      op_a "OpCallPropLex" ["name",name; "argc",argc]
  | `CallPropVoid (name,argc) ->
      op_a "OpCallPropVoid" ["name",name; "argc",argc]
  | `CallStatic (name,argc) ->
      op_a "OpCallStatic" ["name",name; "argc",argc]
  | `CallSuper (name,argc) ->
      op_a "OpCallSuper" ["name",name; "argc",argc]
  | `CallSuperVoid (name,argc) ->
      op_a "OpCallSuperVoid" ["name",name; "argc",argc]
  | `CheckFilter ->
      op "OpCheckFilter"
  | `Coerce ->
      op "OpCoerce"
  | `Coerce_a ->
      op "OpCoerceA"
  | `Coerce_s ->
      op "OpCoerceS"
  | `Construct argc ->
      op_a "OpConstruct" ["argc",argc]
  | `ConstructProp (name,argc) ->
      op_a "OpConstructProp" ["name",name; "argc",argc]
  | `ConstructSuper argc ->
      op_a "OpConstructSuper" ["argc",argc]
  | `Convert_b ->
      op "OpConvertB"
  | `Convert_i ->
      op "OpConvertI"
  | `Convert_d ->
      op "OpConvertD"
  | `Convert_o ->
      op "OpConvertO"
  | `Convert_u ->
      op "OpConvertU"
  | `Convert_s ->
      op "OpConvertS"
  | `Debug (unknown,name,reg,line)->
      attr "OpDebug" ["unknown", string_of_int unknown;
		      "name"   , Int32.to_string name;
		      "reg"    , string_of_int reg;
		      "line"   , Int32.to_string line]
  | `DebugFile file ->
      op_a "OpDebugFile" ["file",file]
  | `DebugLine line ->
      op_a "OpDebugLine" ["line",line]
  | `DecLocal address ->
      op_a "OpDecLocal" ["address",address]
  | `DecLocal_i address ->
      op_a "OpDecLocalI" ["address",address]
  | `Decrement ->
      op "OpDecrement"
  | `Decrement_i ->
      op "OpDecrementI"
  | `DeleteProperty name ->
      op_a "OpDeleteProperty" ["name",name]
  | `Divide ->
      op "OpDivide"
  | `Dup ->
      op "OpDup"
  | `Dxns name ->
      op_a "OpDXNs" ["name",name]
  | `DxnsLate ->
      op "OpDXNsLate"
  | `Equals ->
      op "OpEquals"
  | `Esc_xattr ->
      op "OpEscXattr"
  | `Esc_xelem ->
      op "OpEscXelem"
  | `FindProperty name ->
      op_a "OpFindProperty" ["name",name]
  | `FindPropStrict name ->
      op_a "OpFindPropStrict" ["name",name]
  | `GetDescendants name ->
      op_a "OpGetDescendants" ["name",name]
  | `GetGlobalScope ->
      op "OpGetGlobalScope"
  | `GetGlobalSlot slot_id ->
      op_a "OpGetGlobalSlot" ["soltID",slot_id]
  | `GetLex name ->
      op_a "OpGetLex" ["name",name]
  | `GetLocal address ->
      op_a "OpGetLocal" ["address",address]
  | `GetLocal_0 ->
      op "OpGetLocal0"
  | `GetLocal_1 ->
      op "OpGetLocal1"
  | `GetLocal_2 ->
      op "OpGetLocal2"
  | `GetLocal_3 ->
      op "OpGetLocal3"
  | `GetProperty name ->
      op_a "OpGetProperty" ["name",name]
  | `GetScopeObject scope_index ->
      attr "OpGetScopeObject" ["scopeIndex",string_of_int scope_index]
  | `GetSlot slot_id ->
      op_a "OpGetSlot" ["slotID",slot_id]
  | `GetSuper name ->
      op_a "OpGetSuper" ["name",name]
  | `GreaterEquals ->
      op "OpGreaterEquals"
  | `GreaterThan ->
      op "OpGreaterThan"
  | `HasNext  ->
      op "OpHasNext"
  | `HasNext2 (object_reg,index_reg) ->
      op_a "OpHasNext2" ["object",object_reg;"index",index_reg]
  | `IfEq target ->
      op_i "OpIfEq" ["target",label target]
  | `IfFalse target ->
      op_i "OpIfFalse" ["target",label target]
  | `IfGe target ->
      op_i "OpIfGe" ["target",label target]
  | `IfGt target ->
      op_i "OpIfGt" ["target",label target]
  | `IfLe target ->
      op_i "OpIfLe" ["target",label target]
  | `IfLt target ->
      op_i "OpIfLt" ["target",label target]
  | `IfNge target ->
      op_i "OpIfNge" ["target",label target]
  | `IfNgt target ->
      op_i "OpIfNgt" ["target",label target]
  | `IfNle target ->
      op_i "OpIfNle" ["target",label target]
  | `IfNlt target ->
      op_i "OpIfNgt" ["target",label target]
  | `IfNe target ->
      op_i "OpIfNe"  ["target",label target]
  | `IfStrictEq target ->
      op_i "OpIfStrictEq" ["target",label target]
  | `IfStrictNe target ->
      op_i "OpIfStrictNe" ["target",label target]
  | `IfTrue target ->
      op_i "OpIfTrue" ["target",label target]
  | `In ->
      op "OpIn"
  | `IncLocal address ->
      op_a "OpIncLocal" ["address",address]
  | `IncLocal_i address ->
      op_a "OpIncLocalI" ["address",address]
  | `Increment ->
      op "OpIncrement"
  | `Increment_i ->
      op "OpIncrementI"
  | `InitProperty name ->
      op_a "OpInitProperty" ["name",name]
  | `InstanceOf ->
      op "OpInstanceOf"
  | `IsType name ->
      op_a "OpIsType" ["name",name]
  | `IsTypeLate ->
      op "OpIsTypeLate"
  | `Jump target ->
      op_i "OpJump" ["target",label target]
  | `Kill address ->
      op_a "OpKill" ["address",address]
  | `Label _ ->
      op "OpLabel"
  | `LessEquals ->
      op "OpLessEquals"
  | `LessThan ->
      op "OpLessThan"
  | `LookupSwitch (default_target,target_table) ->
      element "OpLookupSwitch" ["defaultTarget",string_of_int default_target;
				"caseCount"    ,string_of_int @@ List.length target_table]
	[elem "targetTable" @@ List.map (fun v -> attr "U30" ["value",string_of_int v]) target_table]
  | `LShift ->
      op "OpLShift"
  | `Modulo ->
      op "OpModulo"
  | `Multiply ->
      op "OpMultiply"
  | `Multiply_i ->
      op "OpMultiplyI"
  | `Negate ->
      op "OpNegate"
  | `Negate_i ->
      op "OpNegateI"
  | `NewActivation ->
      op "OpNewActivation"
  | `NewArray argc ->
      op_a "OpNewArray" ["argc",argc]
  | `NewCatch exceptionIndex ->
      op_a "OpNewCatch" ["exceptionIndex",exceptionIndex]
  | `NewClass classIndex ->
      op_a "OpNewClass" ["classIndex",classIndex]
  | `NewFunction index ->
      op_a "OpNewFunction" ["index",index]
  | `NewObject argc ->
      op_a "OpNewObject" ["argc",argc]
  | `NextName ->
      op "OpNextName"
  | `NextValue ->
      op "OpNextValue"
  | `Nop ->
      op "OpNop"
  | `Not ->
      op "OpNot"
  | `Pop ->
      op "OpPop"
  | `PopScope ->
      op "OpPopScope"
  | `PushByte byte ->
      attr "OpPushByte" ["value",string_of_int byte]
  | `PushDouble index ->
      op_a "OpPushDouble" ["index",index]
  | `PushFalse ->
      op "OpPushFalse"
  | `PushNaN ->
      op "OpPushNaN"
  | `PushInt index ->
      op_a "OpPushInt" ["index",index]
  | `PushNamespace index ->
      op_a "OpPushNamespace" ["index",index]
  | `PushNan ->
      op "OpPushNan"
  | `PushNull ->
      op "OpPushNull"
  | `PushScope ->
      op "OpPushScope"
  | `PushShort value ->
      op_a "OpPushShort" ["value",value]
  | `PushString index ->
      op_a "OpPushString" ["index",index]
  | `PushTrue ->
      op "OpPushTrue"
  | `PushUInt index ->
      op_a "OpPushUInt" ["index",index]
  | `PushUndefined ->
      op "OpPushUndefined"
  | `PushWith ->
      op "OpPushWith"
  | `ReturnValue ->
      op "OpReturnValue"
  | `ReturnVoid ->
      op "OpReturnVoid"
  | `RShift ->
      op "OpRShift"
  | `SetLocal address ->
      op_a "OpSetLocal" ["address",address]
  | `SetLocal_0 ->
      op "OpSetLocal0"
  | `SetLocal_1 ->
      op "OpSetLocal1"
  | `SetLocal_2 ->
      op "OpSetLocal2"
  | `SetLocal_3 ->
      op "OpSetLocal3"
  | `SetGlobalSlot slot_id ->
      op_a "OpSetGlobalSlot" ["slotID",slot_id]
  | `SetProperty name ->
      op_a "OpSetProperty" ["name",name]
  | `SetSlot slot_id ->
      op_a "OpSetSlot" ["slotID",slot_id]
  | `SetSuper name ->
      op_a "OpSetSuper" ["name",name]
  | `StrictEquals ->
      op "OpStrictEquals"
  | `Subtract ->
      op "OpSubtract"
  | `Subtract_i ->
      op "OpSubtractI"
  | `SubtractI ->
      op "OpSubtractI"
  | `Swap ->
      op "OpSwap"
  | `Throw ->
      op "OpThrow"
  | `TypeOf ->
      op "OpTypeOf"
  | `URShift ->
      op "OpURShift"

