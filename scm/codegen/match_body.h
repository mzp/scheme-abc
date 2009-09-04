function| `ConstructProp (arg0,arg1) -> {default with op=0x4a; stack= ~-arg1; args=(fun ctx -> [u30 @@ Cpool.index arg0 ctx#cpool;u30 arg1]);}
| `ConstructSuper (arg0) -> {default with op=0x49; args=const [u30 arg0]; stack= ~-(arg0+1)}
| `Dup  -> {default with op=0x2a; stack= 2}
| `NewActivation  -> {default with op=0x57; stack=1}
| `NewArray (arg0) -> {default with op=0x56; args=const [u30 arg0]}
| `NewObject (arg0) -> {default with op=0x55; args=const [u30 arg0]; stack=1-arg0}
| `PopScope  -> {default with op=0x1d; scope= ~-1}
| `Swap  -> {default with op=0x2b}
| `Pop  -> {default with op=0x29; stack= ~-1}
| `Call (arg0) -> {default with op=0x41; stack= 1-(2+arg0); args=const [u30 arg0];}
| `CallPropLex (arg0,arg1) -> {default with op=0x4c; stack= 1-arg1; const=[entry arg0]; args=fun ctx ->[cindex arg0 ctx; u30 arg1]}
| `CallProperty (arg0,arg1) -> {default with op=0x46; stack= 1-arg1; const=[entry arg0]; args=fun ctx ->[cindex arg0 ctx; u30 arg1]}
| `FindPropStrict (arg0) -> {default with op=0x5D; stack=1; const=[entry arg0]; args=fun ctx -> [cindex arg0 ctx]}
| `ReturnValue  -> {default with op=0x48; stack= ~-1}
| `ReturnVoid  -> {default with op=0x47}
| `InitProperty (arg0) -> {default with op=0x68; stack= ~-2; const=[entry arg0]; args=fun ctx ->[cindex arg0 ctx]}
| `SetProperty (arg0) -> {default with op=0x61; stack= ~-2; const=[entry arg0]; args=fun ctx ->[cindex arg0 ctx]}
| `GetProperty (arg0) -> {default with op=0x66;             const=[entry arg0]; args=fun ctx ->[cindex arg0 ctx]}
| `GetLex (arg0) -> {default with op=0x60; stack=1;    const=[entry arg0]; args=fun ctx ->[cindex arg0 ctx]}
| `SetGlobalSlot (arg0) -> {default with op=0x6f; stack= ~-1; args=const [u30 arg0]}
| `GetGlobalSlot (arg0) -> {default with op=0x6e; stack=1;    args=const [u30 arg0]}
| `SetSlot (arg0) -> {default with op=0x6d; args=const [u30 arg0]; stack= ~-2}
| `GetSlot (arg0) -> {default with op=0x6c; args=const [u30 arg0]}
| `SetLocal (arg0) -> {default with op=0x63; stack=1; args=const [u30 arg0]}
| `SetLocal_3  -> {default with op=0xD7; stack=1}
| `SetLocal_2  -> {default with op=0xD6; stack=1}
| `SetLocal_1  -> {default with op=0xD5; stack=1}
| `SetLocal_0  -> {default with op=0xD4; stack=1}
| `GetLocal (arg0) -> {default with op=0x62; stack=1; args=const [u30 arg0];count=(arg0+1)}
| `GetLocal_3  -> {default with op=0xD3; stack=1;count=4}
| `GetLocal_2  -> {default with op=0xD2; stack=1;count=3}
| `GetLocal_1  -> {default with op=0xD1; stack=1;count=2}
| `GetLocal_0  -> {default with op=0xD0; stack=1;count=1}
| `GetScopeObject (arg0) -> {default with op=0x65; stack=1; args=const[u8 arg0]}
| `GetGlobalScope  -> {default with op=0x64; stack=1}
| `PushWith  -> {default with op=0x1c; stack= ~-1; scope=1}
| `PushScope  -> {default with op=0x30; stack= ~-1; scope=1}
| `PushNamespace (arg0) -> {default with op=0x31; stack=1; const=[entry arg0]; args=fun ctx -> [cindex arg0 ctx]}
| `PushDouble (arg0) -> {default with op=0x2F; stack=1; const=[`Double arg0]; args=fun ctx -> [cindex (`Double arg0) ctx]}
| `PushUInt (arg0) -> {default with op=0x2E; stack=1; const=[`UInt arg0];   args=fun ctx -> [cindex (`UInt arg0)   ctx]}
| `PushInt (arg0) -> {default with op=0x2D; stack=1; const=[`Int arg0];    args=fun ctx -> [cindex (`Int arg0)    ctx]}
| `PushString (arg0) -> {default with op=0x2C; stack=1; const=[`String arg0]; args=fun ctx -> [cindex (`String arg0) ctx]}
| `PushNaN  -> {default with op=0x28; stack=1}
| `PushFalse  -> {default with op=0x27; stack=1}
| `PushTrue  -> {default with op=0x26; stack=1}
| `PushShort (arg0) -> {default with op=0x25; stack=1; args=const [u30 arg0]}
| `PushByte (arg0) -> {default with op=0x24; stack=1; args=const [u8  arg0]}
| `PushUndefined  -> {default with op=0x21; stack=1}
| `PushNull  -> {default with op=0x20; stack=1}
| `IfStrictNe (arg0) -> {default with op=0x1a; stack= ~-1; args=const [label_ref arg0]}
| `IfStrictEq (arg0) -> {default with op=0x19; stack= ~-1; args=const [label_ref arg0]}
| `IfGe (arg0) -> {default with op=0x18; stack= ~-1; args=const [label_ref arg0]}
| `IfGt (arg0) -> {default with op=0x17; stack= ~-1; args=const [label_ref arg0]}
| `IfLe (arg0) -> {default with op=0x16; stack= ~-1; args=const [label_ref arg0]}
| `IfLt (arg0) -> {default with op=0x15; stack= ~-1; args=const [label_ref arg0]}
| `IfNe (arg0) -> {default with op=0x14; stack= ~-1; args=const [label_ref arg0]}
| `IfEq (arg0) -> {default with op=0x13; stack= ~-1; args=const [label_ref arg0]}
| `IfFalse (arg0) -> {default with op=0x12; stack= ~-1; args=const [label_ref arg0]}
| `IfTrue (arg0) -> {default with op=0x11; stack= ~-1; args=const [label_ref arg0]}
| `Jump (arg0) -> {default with op=0x10; args=const [label_ref arg0]}
| `IfNge (arg0) -> {default with op=0x0f; stack= ~-1; args=const [label_ref arg0]}
| `IfNgt (arg0) -> {default with op=0x0e; stack= ~-1; args=const [label_ref arg0]}
| `IfNle (arg0) -> {default with op=0x0d; stack= ~-1; args=const [label_ref arg0]}
| `IfNlt (arg0) -> {default with op=0x0c; stack= ~-1; args=const [label_ref arg0]}
| `Label (arg0) -> {default with op=0x09; prefix=const [label arg0]}
| `GreaterEquals  -> {default with op=0xb0; stack= ~-1}
| `GreaterThan  -> {default with op=0xaf; stack= ~-1}
| `LessEquals  -> {default with op=0xae; stack= ~-1}
| `LessThan  -> {default with op=0xad; stack= ~-1}
| `StrictEquals  -> {default with op=0xac; stack= ~-1}
| `Equals  -> {default with op=0xab; stack= ~-1}
| `Modulo  -> {default with op=0xa4; stack= ~-1}
| `Divide  -> {default with op=0xa3; stack= ~-1}
| `Multiply  -> {default with op=0xa2; stack= ~-1}
| `Subtract  -> {default with op=0xa1; stack= ~-1}
| `Add  -> {default with op=0xa0; stack= ~-1}
| `Multiply_i  -> {default with op=0xc7; stack= ~-1}
| `Subtract_i  -> {default with op=0xc6; stack= ~-1}
| `Add_i  -> {default with op=0xc5; stack= ~-1}
| `Convert_u  -> {default with op=0x77}
| `Convert_b  -> {default with op=0x76}
| `Convert_d  -> {default with op=0x75}
| `Convert_s  -> {default with op=0x74}
| `Convert_i  -> {default with op=0x73}
| `Coerce_s  -> {default with op=0x85}
| `Coerce_a  -> {default with op=0x82}
| `Coerce  -> {default with op=0x80}
| `NewClass (arg0) -> {default with op=0x58; class_=Some arg0; args=fun ctx -> [u30 @@ RevList.index arg0 ctx#classes];}
| `NewFunction (arg0) -> {default with op=0x40; stack=1; method_=Some arg0; args=fun ctx->[u30 @@ RevList.index arg0 ctx#methods];}
