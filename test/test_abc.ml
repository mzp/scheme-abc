open Base
open Abc
open Util
open Bytes

test script =
  assert_equal [U30 0; U30 0] @@ bytes_of_script  {init=0; trait_s=[]};
  assert_equal [U30 0x7F; U30 0] @@ bytes_of_script {init=0x7F; trait_s=[]}
