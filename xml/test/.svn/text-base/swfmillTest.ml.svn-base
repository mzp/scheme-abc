open Base
open OUnit
open Xml
open EasyXml

let ok x y =
  OUnit.assert_equal ~printer:Xml.to_string_fmt (normalize x) (normalize y)

let abc =
  TestSupport.example "hello"

let cpool =
  Swfmill.of_cpool abc#constant_pool

let methods =
  Swfmill.of_methods abc#methods

let metadata =
  Swfmill.of_metadata abc#metadata

let instances =
  Swfmill.of_instances abc#instances

let classes =
  Swfmill.of_classes abc#classes

let scripts =
  Swfmill.of_script abc#script

let method_bodies =
  Swfmill.of_method_bodies abc#method_body
  
let _ = 
  ("action module test" >::: [
     "constants" >::
       (fun () ->
	  flip ok cpool @@ elem "Constants"
	    [
	      elem "ints"    [];
	      elem "uints"   [];
	      elem "doubles" [];
	      elem "strings" [attr "String2" ["value",""];
			      attr "String2" ["value","Hello,world!!"];
			      attr "String2" ["value","print"] ];
	      elem "namespaces" [attr "Namespace" ["index","1"]];
	      elem "namespaceSets" [];
	      elem "multinames" [attr "QName" ["namespaceIndex","1";"nameIndex","1"];
				 attr "QName" ["namespaceIndex","1";"nameIndex","3"] ]
	    ]
       );
     "method info" >::
       (fun _ ->
	  flip ok methods @@ elem "methods" [       
	    element 
	      "MethodInfo"
	      ["retType"       ,"0";
	       "nameIndex"     ,"1";
	       "hasParamNames" ,"0";
	       "setSDXNs"      ,"0";
	       "isExplicit"    ,"0";
	       "ignoreRest"    ,"0";
	       "hasOptional"   ,"0";
	       "needRest"      ,"0";
	       "needActivation","0";
	       "needArguments" ,"0"]
	      [elem "paramTypes" []]
	  ]
       );
     "metadata" >::
       (fun _ ->
	  flip ok metadata @@ elem "metadata" []);
     "instances" >::
       (fun _ ->
	  flip ok instances @@ elem "instances" []);
     "classes" >::
       (fun _ ->
	  flip ok classes @@ elem "classes" []);
     "script" >::
       (fun _ ->
	  flip ok scripts @@ elem "scripts" [
	    element "ScriptInfo" ["initIndex","0"] [elem "traits" []]
	  ]);
     "method body" >::
       (fun _ ->
	  flip ok method_bodies @@ elem "methodBodies" [
	    element "MethodBody" ["methodInfo"    ,"0";
				  "maxStack"      ,"2";
				  "maxRegs"       ,"1";
				  "scopeDepth"    ,"0";
				  "maxScope"      ,"1";
				  "exceptionCount","0"]
	      [ elem "code" [
		  attr "OpGetLocal0" [];
		  attr "OpPushScope" [];
		  attr "OpFindPropStrict" ["name","2"];
		  attr "OpPushString" ["index","2"];
		  attr "OpCallPropLex" ["name","2";"argc","1"];
		  attr "OpPop" [];
		  attr "OpReturnVoid" []; ]; 
		elem "exceptions" [];
		elem "traits" [] ]
	  ]);
   ])  +> run_test_tt
