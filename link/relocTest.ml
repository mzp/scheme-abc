open Base
open OUnit
open Swflib.AbcType
open EmptyAbc
open Reloc

let plus n x = n + x
let ctx = {|
  int       = plus 1;
  uint      = plus 2;
  double    = plus 3;
  string    = plus 4;
  namespace     = plus 5;
  namespace_set = plus 6;
  multiname = plus 7;
  methods   = plus 8;
  classes   = plus 9
|}
let ok x y = assert_equal x @@ reloc ctx y

let _ = begin "reloc.ml" >::: [
  "cpool" >:: begin fun () ->
    assert_equal
      { cpool with
	  namespace     = [Namespace 4];
	  namespace_set = [[6;7]];
	  multiname     = [QName (6,5)]; }
      @@ reloc_cpool ctx
      { cpool with
	  namespace     = [Namespace 0];
	  namespace_set = [[1;2]];
	  multiname     = [QName(1,1)] }
  end;
  "method_info" >:: begin fun () ->
    ok
      {abc with method_info = [{info with
				method_name = 7
			       }]}
      {abc with method_info = [ info ]}
  end;
  "method_body" >:: begin fun () ->
    ok
      {abc with method_bodies =
	  [{ body with
	       code = [
	         `PushInt 1;
	         `PushUInt 2;
	         `PushDouble 3;
		 `PushString 4;
	         `GetLex 7;
	         `GetProperty 7;
	         `SetProperty 7;
	         `InitProperty 7;
	         `FindPropStrict 7;
	         `CallProperty (7,1);
	         `CallPropLex (7,1);
	         `ConstructProp (7,1);
	         `NewFunction 8;
	         `NewClass 9;
	       ]
	   }]}
      {abc with method_bodies =
	  [{ body with
	       code = [
	         `PushInt 0;
	         `PushUInt 0;
	         `PushDouble 0;
		 `PushString 0;
	         `GetLex 0;
	         `GetProperty 0;
	         `SetProperty 0;
	         `InitProperty 0;
	         `FindPropStrict 0;
	         `CallProperty (0,1);
	         `CallPropLex (0,1);
	         `ConstructProp (0,1);
	         `NewFunction 0;
	         `NewClass 0;
	       ]
	   }]}
  end;
  "class" >:: begin fun () ->
    ok
      {abc with Swflib.AbcType.classes = [{class_ with cinit = 8 }]}
      {abc with Swflib.AbcType.classes = [class_]}
  end;
  "instance" >:: begin fun () ->
    ok
      {abc with instances = [{instance with iinit=8; super_name=7; instance_name=7} ]}
      {abc with instances = [instance]}
  end;
  "script" >:: begin fun () ->
    ok
      {abc with
	 scripts=[{init=8;script_traits=[]}]}
      {abc with
	 scripts=[{init=0;script_traits=[] }]}
  end;
  "trait" >:: begin fun () ->
    ok
      {abc with
	 scripts=[{init=8;
		   script_traits=[
		     {trait_name=7; trait_metadata=[]; data=SlotTrait  (0,7,0,0)};
		     {trait_name=7; trait_metadata=[]; data=ConstTrait (0,7,0,0)};
		     {trait_name=7; trait_metadata=[]; data=ClassTrait (0,9)};
		     {trait_name=7; trait_metadata=[]; data=MethodTrait (0,8,[])};
		     {trait_name=7; trait_metadata=[]; data=SetterTrait (0,8,[])};
		     {trait_name=7; trait_metadata=[]; data=GetterTrait (0,8,[])};
		     {trait_name=7; trait_metadata=[]; data=FunctionTrait (0,8)};
		   ]}]}
      {abc with
	 scripts=[{init=0;
		   script_traits=[
		     {trait_name=0; trait_metadata=[]; data=SlotTrait  (0,0,0,0)};
		     {trait_name=0; trait_metadata=[]; data=ConstTrait (0,0,0,0)};
		     {trait_name=0; trait_metadata=[]; data=ClassTrait (0,0)};
		     {trait_name=0; trait_metadata=[]; data=MethodTrait (0,0,[])};
		     {trait_name=0; trait_metadata=[]; data=SetterTrait (0,0,[])};
		     {trait_name=0; trait_metadata=[]; data=GetterTrait (0,0,[])};
		     {trait_name=0; trait_metadata=[]; data=FunctionTrait (0,0)};
		   ] }]}
  end
] end +> run_test_tt_main


