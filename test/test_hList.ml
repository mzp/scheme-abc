open Base
open HList
open Util

test last =
  assert_equal 3 @@ last [1;2;3];
  assert_equal 1 @@ last [1];
  assert_raises (Invalid_argument "HList.last") (fun ()->last [])

test init =
  assert_equal [1;2] @@ init [1;2;3]

test null =
  assert_equal true @@ null [];
  assert_equal false @@ null [1;2];

test foldl1 =
  assert_equal 6   @@ fold_left1 (+) [1;2;3];
  assert_equal ~-4 @@ fold_left1 (-) [1;2;3];

test foldr1 =
  assert_equal 6 @@ fold_right1 (+) [1;2;3];
  assert_equal 2 @@ fold_right1 (-) [1;2;3];

test conj =
  assert_equal true  @@ conj [];
  assert_equal true  @@ conj [true;true;true];
  assert_equal false @@ conj [true;false;true]

test disj =
  assert_equal false @@ disj [];
  assert_equal false @@ disj [false;false;false];
  assert_equal true  @@ disj [false;false;true]

test sum =
  assert_equal 10 @@ sum [1;2;3;4]

test product =
  assert_equal 24 @@ product [1;2;3;4]

test concat_map =
  assert_equal [1;2;3] @@ concat_map (fun x->[x+1]) [0;1;2]
