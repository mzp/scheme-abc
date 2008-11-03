open Base
open Util

test range = 
    assert_equal [1;2;3] @@ range 1 4;
    assert_equal [] @@ range 1 1;
    assert_equal [1] @@ range 1 2;
    assert_equal [] @@ range 1 0

test unfold =
    let f x = 
      if x <= 0 then
	None
      else
	Some (x,x-1)
    in
      assert_equal [3;2;1] @@ unfold f 3;
      assert_equal [] @@ unfold f 0;
      assert_equal [1] @@ unfold f 1

test map_accum_left =
    assert_equal (0, []) @@ map_accum_left (fun i x->(i+1,x+i)) 0 [];
    assert_equal (4, [1; 2; 3; 4]) @@ map_accum_left (fun i x->(i+1,x+i)) 0 [1;1;1;1]

test group_by =
    assert_equal [[1];[2;2];[3]] @@ group_by (==) [1;2;2;3]

