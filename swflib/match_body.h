| `NewFunction (arg0) -> { {default with op=0x40; args=(fun _ctx -> [p_method_ _ctx arg0]); const=filter_map id [c_method_ arg0]} with stack=1; method_=Some arg0}
| `NewClass (arg0) -> {default with op=0x58; args=(fun _ctx -> [p_class_ _ctx arg0]); const=filter_map id [c_class_ arg0]}
| `Coerce  -> {default with op=0x80; args=(fun _ctx -> []); const=filter_map id []}
| `Coerce_a  -> {default with op=0x82; args=(fun _ctx -> []); const=filter_map id []}
| `Coerce_s  -> {default with op=0x85; args=(fun _ctx -> []); const=filter_map id []}
| `Convert_i  -> {default with op=0x73; args=(fun _ctx -> []); const=filter_map id []}
| `Convert_s  -> {default with op=0x74; args=(fun _ctx -> []); const=filter_map id []}
| `Convert_d  -> {default with op=0x75; args=(fun _ctx -> []); const=filter_map id []}
| `Convert_b  -> {default with op=0x76; args=(fun _ctx -> []); const=filter_map id []}
| `Convert_u  -> {default with op=0x77; args=(fun _ctx -> []); const=filter_map id []}
| `Add_i  -> { {default with op=0xc5; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `Subtract_i  -> { {default with op=0xc6; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `Multiply_i  -> { {default with op=0xc7; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `Add  -> { {default with op=0xa0; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `Subtract  -> { {default with op=0xa1; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `Multiply  -> { {default with op=0xa2; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `Divide  -> { {default with op=0xa3; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `Modulo  -> { {default with op=0xa4; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `Equals  -> { {default with op=0xab; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `StrictEquals  -> { {default with op=0xac; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `LessThan  -> { {default with op=0xad; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `LessEquals  -> { {default with op=0xae; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `GreaterThan  -> { {default with op=0xaf; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `GreaterEquals  -> { {default with op=0xb0; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `Label (arg0) -> { {default with op=0x9; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]} with prefix=const [label arg0];args=const []}
| `IfNlt (arg0) -> { {default with op=0xc; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]} with stack= ~-1}
| `IfNle (arg0) -> { {default with op=0xd; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]} with stack= ~-1}
| `IfNgt (arg0) -> { {default with op=0xe; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]} with stack= ~-1}
| `IfNge (arg0) -> { {default with op=0xf; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]} with stack= ~-1}
| `Jump (arg0) -> {default with op=0x10; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]}
| `IfTrue (arg0) -> { {default with op=0x11; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]} with stack= ~-1}
| `IfFalse (arg0) -> { {default with op=0x12; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]} with stack= ~-1}
| `IfEq (arg0) -> { {default with op=0x13; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]} with stack= ~-1}
| `IfNe (arg0) -> { {default with op=0x14; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]} with stack= ~-1}
| `IfLt (arg0) -> { {default with op=0x15; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]} with stack= ~-1}
| `IfLe (arg0) -> { {default with op=0x16; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]} with stack= ~-1}
| `IfGt (arg0) -> { {default with op=0x17; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]} with stack= ~-1}
| `IfGe (arg0) -> { {default with op=0x18; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]} with stack= ~-1}
| `IfStrictEq (arg0) -> { {default with op=0x19; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]} with stack= ~-1}
| `IfStrictNe (arg0) -> { {default with op=0x1a; args=(fun _ctx -> [p_label _ctx arg0]); const=filter_map id [c_label arg0]} with stack= ~-1}
| `PushNull  -> { {default with op=0x20; args=(fun _ctx -> []); const=filter_map id []} with stack=1}
| `PushUndefined  -> { {default with op=0x21; args=(fun _ctx -> []); const=filter_map id []} with stack=1}
| `PushByte (arg0) -> { {default with op=0x24; args=(fun _ctx -> [p_u8 _ctx arg0]); const=filter_map id [c_u8 arg0]} with stack=1}
| `PushShort (arg0) -> { {default with op=0x25; args=(fun _ctx -> [p_u30 _ctx arg0]); const=filter_map id [c_u30 arg0]} with stack=1}
| `PushTrue  -> { {default with op=0x26; args=(fun _ctx -> []); const=filter_map id []} with stack=1}
| `PushFalse  -> { {default with op=0x27; args=(fun _ctx -> []); const=filter_map id []} with stack=1}
| `PushNaN  -> { {default with op=0x28; args=(fun _ctx -> []); const=filter_map id []} with stack=1}
| `PushString (arg0) -> { {default with op=0x2c; args=(fun _ctx -> [p_string _ctx arg0]); const=filter_map id [c_string arg0]} with stack=1}
| `PushInt (arg0) -> { {default with op=0x2d; args=(fun _ctx -> [p_int _ctx arg0]); const=filter_map id [c_int arg0]} with stack=1}
| `PushUInt (arg0) -> { {default with op=0x2e; args=(fun _ctx -> [p_uint _ctx arg0]); const=filter_map id [c_uint arg0]} with stack=1}
| `PushDouble (arg0) -> { {default with op=0x2f; args=(fun _ctx -> [p_float _ctx arg0]); const=filter_map id [c_float arg0]} with stack=1}
| `PushNamespace (arg0) -> { {default with op=0x31; args=(fun _ctx -> [p_namespace _ctx arg0]); const=filter_map id [c_namespace arg0]} with stack=1}
| `PushScope  -> { {default with op=0x30; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1; scope=1}
| `PushWith  -> { {default with op=0x1c; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1; scope=1}
| `GetGlobalScope  -> { {default with op=0x64; args=(fun _ctx -> []); const=filter_map id []} with stack=1}
| `GetScopeObject (arg0) -> { {default with op=0x65; args=(fun _ctx -> [p_u8 _ctx arg0]); const=filter_map id [c_u8 arg0]} with stack=1}
| `GetLocal_0  -> { {default with op=0xd0; args=(fun _ctx -> []); const=filter_map id []} with stack=1;count=1}
| `GetLocal_1  -> { {default with op=0xd1; args=(fun _ctx -> []); const=filter_map id []} with stack=1;count=2}
| `GetLocal_2  -> { {default with op=0xd2; args=(fun _ctx -> []); const=filter_map id []} with stack=1;count=3}
| `GetLocal_3  -> { {default with op=0xd3; args=(fun _ctx -> []); const=filter_map id []} with stack=1;count=4}
| `GetLocal (arg0) -> { {default with op=0x62; args=(fun _ctx -> [p_u30 _ctx arg0]); const=filter_map id [c_u30 arg0]} with stack=1;count=(arg0+1)}
| `SetLocal_0  -> { {default with op=0xd4; args=(fun _ctx -> []); const=filter_map id []} with stack=1}
| `SetLocal_1  -> { {default with op=0xd5; args=(fun _ctx -> []); const=filter_map id []} with stack=1}
| `SetLocal_2  -> { {default with op=0xd6; args=(fun _ctx -> []); const=filter_map id []} with stack=1}
| `SetLocal_3  -> { {default with op=0xd7; args=(fun _ctx -> []); const=filter_map id []} with stack=1}
| `SetLocal (arg0) -> { {default with op=0x63; args=(fun _ctx -> [p_u30 _ctx arg0]); const=filter_map id [c_u30 arg0]} with stack=1}
| `GetSlot (arg0) -> {default with op=0x6c; args=(fun _ctx -> [p_u30 _ctx arg0]); const=filter_map id [c_u30 arg0]}
| `SetSlot (arg0) -> { {default with op=0x6d; args=(fun _ctx -> [p_u30 _ctx arg0]); const=filter_map id [c_u30 arg0]} with stack= ~-2}
| `GetGlobalSlot (arg0) -> { {default with op=0x6e; args=(fun _ctx -> [p_u30 _ctx arg0]); const=filter_map id [c_u30 arg0]} with stack=1}
| `SetGlobalSlot (arg0) -> { {default with op=0x6f; args=(fun _ctx -> [p_u30 _ctx arg0]); const=filter_map id [c_u30 arg0]} with stack= ~-1}
| `GetLex (arg0) -> { {default with op=0x60; args=(fun _ctx -> [p_multiname _ctx arg0]); const=filter_map id [c_multiname arg0]} with stack=1}
| `GetProperty (arg0) -> {default with op=0x66; args=(fun _ctx -> [p_multiname _ctx arg0]); const=filter_map id [c_multiname arg0]}
| `SetProperty (arg0) -> { {default with op=0x61; args=(fun _ctx -> [p_multiname _ctx arg0]); const=filter_map id [c_multiname arg0]} with stack= ~-2}
| `InitProperty (arg0) -> { {default with op=0x68; args=(fun _ctx -> [p_multiname _ctx arg0]); const=filter_map id [c_multiname arg0]} with stack= ~-2}
| `ReturnVoid  -> {default with op=0x47; args=(fun _ctx -> []); const=filter_map id []}
| `ReturnValue  -> { {default with op=0x48; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `FindPropStrict (arg0) -> { {default with op=0x5d; args=(fun _ctx -> [p_multiname _ctx arg0]); const=filter_map id [c_multiname arg0]} with stack=1}
| `CallProperty (arg0,arg1) -> { {default with op=0x46; args=(fun _ctx -> [p_multiname _ctx arg0;p_u30 _ctx arg1]); const=filter_map id [c_multiname arg0;c_u30 arg1]} with stack= 1-arg1}
| `CallPropLex (arg0,arg1) -> { {default with op=0x4c; args=(fun _ctx -> [p_multiname _ctx arg0;p_u30 _ctx arg1]); const=filter_map id [c_multiname arg0;c_u30 arg1]} with stack= 1-arg1}
| `Call (arg0) -> { {default with op=0x41; args=(fun _ctx -> [p_u30 _ctx arg0]); const=filter_map id [c_u30 arg0]} with stack= 1-(2+arg0)}
| `Pop  -> { {default with op=0x29; args=(fun _ctx -> []); const=filter_map id []} with stack= ~-1}
| `Swap  -> {default with op=0x2b; args=(fun _ctx -> []); const=filter_map id []}
| `PopScope  -> { {default with op=0x1d; args=(fun _ctx -> []); const=filter_map id []} with scope= ~-1}
| `NewObject (arg0) -> { {default with op=0x55; args=(fun _ctx -> [p_u30 _ctx arg0]); const=filter_map id [c_u30 arg0]} with stack=1-arg0}
| `NewArray (arg0) -> {default with op=0x56; args=(fun _ctx -> [p_u30 _ctx arg0]); const=filter_map id [c_u30 arg0]}
| `NewActivation  -> { {default with op=0x57; args=(fun _ctx -> []); const=filter_map id []} with stack=1}
| `Dup  -> { {default with op=0x2a; args=(fun _ctx -> []); const=filter_map id []} with stack= 2}
| `ConstructSuper (arg0) -> { {default with op=0x49; args=(fun _ctx -> [p_u30 _ctx arg0]); const=filter_map id [c_u30 arg0]} with stack= ~-(arg0+1)}
| `ConstructProp (arg0,arg1) -> { {default with op=0x4a; args=(fun _ctx -> [p_multiname _ctx arg0;p_u30 _ctx arg1]); const=filter_map id [c_multiname arg0;c_u30 arg1]} with stack= ~-arg1}
