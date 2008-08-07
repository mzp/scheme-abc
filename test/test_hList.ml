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

test maximum =
    assert_equal 10 @@ maximum [1;2;10]

test maximum =
    assert_equal 1 @@ minimum [1;2;10]

test scanl =
    assert_equal [0; 1; 3; 6] @@ scanl (+) 0 [1;2;3];
    assert_equal [0;-1;-3;-6] @@ scanl (-) 0 [1;2;3]

test scanl1 =
    assert_equal [0; 1; 3; 6] @@ scanl1 (+) [0;1;2;3];
    assert_equal [0;-1;-3;-6] @@ scanl1 (-) [0;1;2;3];
    assert_equal []           @@ scanl1 (+) []

test scanr =
    assert_equal [6;5;3;0] @@ scanr (+) 0 [1;2;3]

test scanr1 =
    assert_equal [6;5;3;0] @@ scanr1 (+) [0;1;2;3];
    assert_equal []        @@ scanr1 (+) []

test replicate =
    assert_equal [1;1;1] @@ replicate 3 1;
    assert_equal []      @@ replicate 0 1

test take =
    assert_equal [1;2;3] @@ take 3 [1;2;3;4;5];
    assert_equal [] @@ take 0 [1;2;3;4;5];
    assert_equal [] @@ take ~-1 [1;2;3;4;5];
    assert_equal [1;2;3] @@ take 10 [1;2;3];

test splitAt =
    assert_equal ([1],[2;3]) @@ splitAt 1 [1;2;3];
    assert_equal ([],[1;2;3]) @@ splitAt 0 [1;2;3];
    assert_equal ([1;2;3],[]) @@ splitAt 100 [1;2;3];
    assert_equal ([],[]) @@ splitAt 100 []

test takeWhile =
    assert_equal [1;2] @@ takeWhile (fun x -> x < 3) [1;2;3]

test dropWhile =
    assert_equal [3] @@ dropWhile (fun x -> x < 3) [1;2;3]

test span =
    assert_equal ([1;2],[3;4;1;2;3;4]) @@ span (fun x -> x < 3) [1;2;3;4;1;2;3;4]

test break =
    assert_equal ([1;2;3],[4;1;2;3;4]) @@ break (fun x -> x > 3) [1;2;3;4;1;2;3;4]

test zip =
    assert_equal [(1,2);(3,4);(5,6)] @@ zip [1;3;5] [2;4;6];
    assert_equal []                  @@ zip [] [2;4;6]

test zip3 =
    assert_equal [(1,2,3);(4,5,6)]   @@ zip3 [1;4;5] [2;5;6] [3;6]

test unzip =
    assert_equal ([1;3],[2;4]) @@ unzip [(1,2);(3,4)]

test unzip3 =
    assert_equal ([1;3],[2;4],[3;5]) @@ unzip3 [(1,2,3);(3,4,5)]
