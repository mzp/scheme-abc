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

test either =
   let l =
     Left "foo" in
   let r =
     Right 42 in
     assert_equal "foo" @@ left l;
     assert_equal 42    @@ right r;
     assert_equal false (l = r);
     assert_raises (Invalid_argument "right") (fun () -> right l);
     assert_raises (Invalid_argument "left") (fun () -> left r)

test concat_map = 
     assert_equal [] @@ concat_map id [];
     assert_equal [1;2;3] @@ concat_map (fun x->[x]) [1;2;3];
	
test map_accum_left =
    assert_equal (0, []) @@ map_accum_left (fun i x->(i+1,x+i)) 0 [];
    assert_equal (4, [1; 2; 3; 4]) @@ map_accum_left (fun i x->(i+1,x+i)) 0 [1;1;1;1]
