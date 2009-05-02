let get_config = function| ConstructProp (arg0,arg1) -> {default with op=0x4a; stack= ~-arg1; args=(fun {cpool=cpool} -> [multiname_get arg0 cpool;Bytes.u30 arg1]);}
| ConstructSuper (arg0) -> {default with op=0x49; args=const [Bytes.u30 arg0]; stack= ~-(arg0+1)}
| NewClass (arg0) -> {default with op=0x58; klass=Some arg0; const=klass_const arg0; meth=(arg0.cinit::arg0.iinit::arg0.static_methods@arg0.methods); args=(fun {klasses=klasses} -> [Bytes.u30 (index arg0 klasses)]);}
| Dup  -> {default with op=0x2a; stack= 2}
| NewFunction (arg0) -> {default with op=0x40; stack=1; const=method_const arg0; meth=[arg0]; args=fun {meths=meths}->[Bytes.u30 (index arg0 meths)];}
| NewActivation  -> {default with op=0x57; stack=1}
| NewArray (arg0) -> {default with op=0x56; args=const [Bytes.u30 arg0]}
| NewObject (arg0) -> {default with op=0x55; args=const [Bytes.u30 arg0]; stack=1-arg0}
| PopScope  -> {default with op=0x1d; scope= ~-1}
| Swap  -> {default with op=0x2b}
| Pop  -> {default with op=0x29; stack= ~-1}
| Call (arg0) -> {default with op=0x41; stack= 1-(2+arg0); args=const [Bytes.u30 arg0];}
| CallPropLex (arg0,arg1) -> {default with op=0x4c; stack= 1-arg1; const=multiname arg0;args=fun {cpool=cmap} ->[multiname_get arg0 cmap;Bytes.u30 arg1]}
| CallProperty (arg0,arg1) -> {default with op=0x46; stack= 1-arg1; const=multiname arg0;args=fun {cpool=cmap} ->[multiname_get arg0 cmap;Bytes.u30 arg1]}
| FindPropStrict (arg0) -> {default with op=0x5D; stack=1; const=multiname arg0; args=fun {cpool=cmap} ->[multiname_get arg0 cmap]}
| ReturnValue  -> {default with op=0x48; stack= ~-1}
| ReturnVoid  -> {default with op=0x47}
| InitProperty (arg0) -> {default with op=0x68; stack= ~-2; const=multiname arg0; args=fun {cpool=cmap} ->[multiname_get arg0 cmap]}
| SetProperty (arg0) -> {default with op=0x61; stack= ~-2; const=multiname arg0; args=fun {cpool=cmap} ->[multiname_get arg0 cmap]}
| GetProperty (arg0) -> {default with op=0x66; const=multiname arg0; args=fun {cpool=cmap} ->[multiname_get arg0 cmap]}
| GetLex (arg0) -> {default with op=0x60; stack=1; const=multiname arg0; args=fun {cpool=cmap} ->[multiname_get arg0 cmap]}
| SetGlobalSlot (arg0) -> {default with op=0x6f; stack= ~-1; args=const [Bytes.u30 arg0]}
| GetGlobalSlot (arg0) -> {default with op=0x6e; stack=1; args=const [Bytes.u30 arg0]}
| SetSlot (arg0) -> {default with op=0x6d; stack= ~-2; args=const [Bytes.u30 arg0]}
| GetSlot (arg0) -> {default with op=0x6c; args=const [Bytes.u30 arg0]}
| SetLocal (arg0) -> {default with op=0x63; stack=1; args=const [Bytes.u30 arg0]}
| SetLocal_3  -> {default with op=0xD7; stack=1}
| SetLocal_2  -> {default with op=0xD6; stack=1}
| SetLocal_1  -> {default with op=0xD5; stack=1}
| SetLocal_0  -> {default with op=0xD4; stack=1}
| GetLocal (arg0) -> {default with op=0x62; stack=1; args=const [Bytes.u30 arg0];count=(arg0+1)}
| GetLocal_3  -> {default with op=0xD3; stack=1;count=4}
| GetLocal_2  -> {default with op=0xD2; stack=1;count=3}
| GetLocal_1  -> {default with op=0xD1; stack=1;count=2}
| GetLocal_0  -> {default with op=0xD0; stack=1;count=1}
| GetScopeObject (arg0) -> {default with op=0x65; stack=1; args=const[Bytes.u8 arg0]}
| GetGlobalScope  -> {default with op=0x64; stack=1}
| PushWith  -> {default with op=0x1c; stack= ~-1; scope=1}
| PushScope  -> {default with op=0x30; stack= ~-1; scope=1}
| PushNamespace (arg0) -> {default with op=0x31; stack=1; const=namespace arg0; args=fun {cpool=cpool} -> [namespace_get arg0 cpool]}
| PushDouble (arg0) -> {default with op=0x2F; stack=1; const=double arg0; args=fun {cpool=cpool} -> [double_get arg0 cpool]}
| PushUInt (arg0) -> {default with op=0x2E; stack=1; const=uint arg0;   args=fun {cpool=cpool} -> [uint_get arg0 cpool]}
| PushInt (arg0) -> {default with op=0x2D; stack=1; const=int arg0;    args=fun {cpool=cpool} -> [int_get arg0 cpool]}
| PushString (arg0) -> {default with op=0x2C; stack=1; const=string arg0; args=fun {cpool=cpool} -> [string_get arg0 cpool]}
| PushNaN  -> {default with op=0x28; stack=1}
| PushFalse  -> {default with op=0x27; stack=1}
| PushTrue  -> {default with op=0x26; stack=1}
| PushShort (arg0) -> {default with op=0x25; stack=1; args=const [Bytes.u30 arg0]}
| PushByte (arg0) -> {default with op=0x24; stack=1; args=const [Bytes.u8  arg0]}
| PushUndefined  -> {default with op=0x21; stack=1}
| PushNull  -> {default with op=0x20; stack=1}
| IfStrictNe (arg0) -> {default with op=0x1a; stack= ~-1; args=const [label_ref arg0]}
| IfStrictEq (arg0) -> {default with op=0x19; stack= ~-1; args=const [label_ref arg0]}
| IfGe (arg0) -> {default with op=0x18; stack= ~-1; args=const [label_ref arg0]}
| IfGt (arg0) -> {default with op=0x17; stack= ~-1; args=const [label_ref arg0]}
| IfLe (arg0) -> {default with op=0x16; stack= ~-1; args=const [label_ref arg0]}
| IfLt (arg0) -> {default with op=0x15; stack= ~-1; args=const [label_ref arg0]}
| IfNe (arg0) -> {default with op=0x14; stack= ~-1; args=const [label_ref arg0]}
| IfEq (arg0) -> {default with op=0x13; stack= ~-1; args=const [label_ref arg0]}
| IfFalse (arg0) -> {default with op=0x12; stack= ~-1; args=const [label_ref arg0]}
| IfTrue (arg0) -> {default with op=0x11; stack= ~-1; args=const [label_ref arg0]}
| Jump (arg0) -> {default with op=0x10; args=const [label_ref arg0]}
| IfNge (arg0) -> {default with op=0x0f; stack= ~-1; args=const [label_ref arg0]}
| IfNgt (arg0) -> {default with op=0x0e; stack= ~-1; args=const [label_ref arg0]}
| IfNle (arg0) -> {default with op=0x0d; stack= ~-1; args=const [label_ref arg0]}
| IfNlt (arg0) -> {default with op=0x0c; stack= ~-1; args=const [label_ref arg0]}
| Label (arg0) -> {default with op=0x09; prefix=const [label arg0]}
| GreaterEquals  -> {default with op=0xb0; stack= ~-1}
| GreaterThan  -> {default with op=0xaf; stack= ~-1}
| LessEquals  -> {default with op=0xae; stack= ~-1}
| LessThan  -> {default with op=0xad; stack= ~-1}
| StrictEquals  -> {default with op=0xac; stack= ~-1}
| Equals  -> {default with op=0xab; stack= ~-1}
| Modulo  -> {default with op=0xa4; stack= ~-1}
| Divide  -> {default with op=0xa3; stack= ~-1}
| Multiply  -> {default with op=0xa2; stack= ~-1}
| Subtract  -> {default with op=0xa1; stack= ~-1}
| Add  -> {default with op=0xa0; stack= ~-1}
| Multiply_i  -> {default with op=0xc7; stack= ~-1}
| Subtract_i  -> {default with op=0xc6; stack= ~-1}
| Add_i  -> {default with op=0xc5; stack= ~-1}
| Coerce_s  -> {default with op=0x85}
| Coerce_a  -> {default with op=0x82}
| Coerce  -> {default with op=0x80}
let string_of_instruction = function| ConstructProp (arg0,arg1) -> "ConstructProp(" ^ (Std.dump arg0) ^ (Std.dump arg1) ^ ")"
| ConstructSuper (arg0) -> "ConstructSuper(" ^ (Std.dump arg0) ^ ")"
| NewClass (arg0) -> "NewClass(" ^ (Std.dump arg0) ^ ")"
| Dup  -> "Dup(" ^ ")"
| NewFunction (arg0) -> "NewFunction(" ^ (Std.dump arg0) ^ ")"
| NewActivation  -> "NewActivation(" ^ ")"
| NewArray (arg0) -> "NewArray(" ^ (Std.dump arg0) ^ ")"
| NewObject (arg0) -> "NewObject(" ^ (Std.dump arg0) ^ ")"
| PopScope  -> "PopScope(" ^ ")"
| Swap  -> "Swap(" ^ ")"
| Pop  -> "Pop(" ^ ")"
| Call (arg0) -> "Call(" ^ (Std.dump arg0) ^ ")"
| CallPropLex (arg0,arg1) -> "CallPropLex(" ^ (Std.dump arg0) ^ (Std.dump arg1) ^ ")"
| CallProperty (arg0,arg1) -> "CallProperty(" ^ (Std.dump arg0) ^ (Std.dump arg1) ^ ")"
| FindPropStrict (arg0) -> "FindPropStrict(" ^ (Std.dump arg0) ^ ")"
| ReturnValue  -> "ReturnValue(" ^ ")"
| ReturnVoid  -> "ReturnVoid(" ^ ")"
| InitProperty (arg0) -> "InitProperty(" ^ (Std.dump arg0) ^ ")"
| SetProperty (arg0) -> "SetProperty(" ^ (Std.dump arg0) ^ ")"
| GetProperty (arg0) -> "GetProperty(" ^ (Std.dump arg0) ^ ")"
| GetLex (arg0) -> "GetLex(" ^ (Std.dump arg0) ^ ")"
| SetGlobalSlot (arg0) -> "SetGlobalSlot(" ^ (Std.dump arg0) ^ ")"
| GetGlobalSlot (arg0) -> "GetGlobalSlot(" ^ (Std.dump arg0) ^ ")"
| SetSlot (arg0) -> "SetSlot(" ^ (Std.dump arg0) ^ ")"
| GetSlot (arg0) -> "GetSlot(" ^ (Std.dump arg0) ^ ")"
| SetLocal (arg0) -> "SetLocal(" ^ (Std.dump arg0) ^ ")"
| SetLocal_3  -> "SetLocal_3(" ^ ")"
| SetLocal_2  -> "SetLocal_2(" ^ ")"
| SetLocal_1  -> "SetLocal_1(" ^ ")"
| SetLocal_0  -> "SetLocal_0(" ^ ")"
| GetLocal (arg0) -> "GetLocal(" ^ (Std.dump arg0) ^ ")"
| GetLocal_3  -> "GetLocal_3(" ^ ")"
| GetLocal_2  -> "GetLocal_2(" ^ ")"
| GetLocal_1  -> "GetLocal_1(" ^ ")"
| GetLocal_0  -> "GetLocal_0(" ^ ")"
| GetScopeObject (arg0) -> "GetScopeObject(" ^ (Std.dump arg0) ^ ")"
| GetGlobalScope  -> "GetGlobalScope(" ^ ")"
| PushWith  -> "PushWith(" ^ ")"
| PushScope  -> "PushScope(" ^ ")"
| PushNamespace (arg0) -> "PushNamespace(" ^ (Std.dump arg0) ^ ")"
| PushDouble (arg0) -> "PushDouble(" ^ (Std.dump arg0) ^ ")"
| PushUInt (arg0) -> "PushUInt(" ^ (Std.dump arg0) ^ ")"
| PushInt (arg0) -> "PushInt(" ^ (Std.dump arg0) ^ ")"
| PushString (arg0) -> "PushString(" ^ (Std.dump arg0) ^ ")"
| PushNaN  -> "PushNaN(" ^ ")"
| PushFalse  -> "PushFalse(" ^ ")"
| PushTrue  -> "PushTrue(" ^ ")"
| PushShort (arg0) -> "PushShort(" ^ (Std.dump arg0) ^ ")"
| PushByte (arg0) -> "PushByte(" ^ (Std.dump arg0) ^ ")"
| PushUndefined  -> "PushUndefined(" ^ ")"
| PushNull  -> "PushNull(" ^ ")"
| IfStrictNe (arg0) -> "IfStrictNe(" ^ (Std.dump arg0) ^ ")"
| IfStrictEq (arg0) -> "IfStrictEq(" ^ (Std.dump arg0) ^ ")"
| IfGe (arg0) -> "IfGe(" ^ (Std.dump arg0) ^ ")"
| IfGt (arg0) -> "IfGt(" ^ (Std.dump arg0) ^ ")"
| IfLe (arg0) -> "IfLe(" ^ (Std.dump arg0) ^ ")"
| IfLt (arg0) -> "IfLt(" ^ (Std.dump arg0) ^ ")"
| IfNe (arg0) -> "IfNe(" ^ (Std.dump arg0) ^ ")"
| IfEq (arg0) -> "IfEq(" ^ (Std.dump arg0) ^ ")"
| IfFalse (arg0) -> "IfFalse(" ^ (Std.dump arg0) ^ ")"
| IfTrue (arg0) -> "IfTrue(" ^ (Std.dump arg0) ^ ")"
| Jump (arg0) -> "Jump(" ^ (Std.dump arg0) ^ ")"
| IfNge (arg0) -> "IfNge(" ^ (Std.dump arg0) ^ ")"
| IfNgt (arg0) -> "IfNgt(" ^ (Std.dump arg0) ^ ")"
| IfNle (arg0) -> "IfNle(" ^ (Std.dump arg0) ^ ")"
| IfNlt (arg0) -> "IfNlt(" ^ (Std.dump arg0) ^ ")"
| Label (arg0) -> "Label(" ^ (Std.dump arg0) ^ ")"
| GreaterEquals  -> "GreaterEquals(" ^ ")"
| GreaterThan  -> "GreaterThan(" ^ ")"
| LessEquals  -> "LessEquals(" ^ ")"
| LessThan  -> "LessThan(" ^ ")"
| StrictEquals  -> "StrictEquals(" ^ ")"
| Equals  -> "Equals(" ^ ")"
| Modulo  -> "Modulo(" ^ ")"
| Divide  -> "Divide(" ^ ")"
| Multiply  -> "Multiply(" ^ ")"
| Subtract  -> "Subtract(" ^ ")"
| Add  -> "Add(" ^ ")"
| Multiply_i  -> "Multiply_i(" ^ ")"
| Subtract_i  -> "Subtract_i(" ^ ")"
| Add_i  -> "Add_i(" ^ ")"
| Coerce_s  -> "Coerce_s(" ^ ")"
| Coerce_a  -> "Coerce_a(" ^ ")"
| Coerce  -> "Coerce(" ^ ")"
