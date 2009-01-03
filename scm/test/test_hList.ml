open Base
open HList
open Util
open OUnit

let _ =
  ("Haskell like List operation" >::: [
     "last" >::
       (fun () ->
	  ok 3 @@ last [1;2;3];
	  ok 1 @@ last [1];
	  assert_raises (Invalid_argument "HList.last") (fun ()->last []));
     "init" >::
       (fun () ->
	  ok [1;2] @@ init [1;2;3]);
     "null" >::
       (fun () ->
	  ok true @@ null [];
	  ok false @@ null [1;2]);
     "fold1" >::
       (fun () ->
	  ok 6   @@ fold_left1 (+) [1;2;3];
	  ok ~-4 @@ fold_left1 (-) [1;2;3]);
     "foldr1" >::
       (fun () ->
	  ok 6 @@ fold_right1 (+) [1;2;3];
	  ok 2 @@ fold_right1 (-) [1;2;3]);
     "conj(and)" >::
       (fun () ->
	  ok true  @@ conj [];
	  ok true  @@ conj [true;true;true];
	  ok false @@ conj [true;false;true]);
     "disj(or)" >::
       (fun () ->
	  ok false @@ disj [];
	  ok false @@ disj [false;false;false];
	  ok true  @@ disj [false;false;true]);
     "sum" >::
       (fun () ->
	  ok 10 @@ sum [1;2;3;4]);
     "product" >::
       (fun () ->
	  ok 24 @@ product [1;2;3;4]);
     "concat_map" >::
       (fun () ->
	  ok [1;2;3] @@ concat_map (fun x->[x+1]) [0;1;2]);
     "max" >::
       (fun () ->
	  ok 10 @@ maximum [1;2;10]);
     "min" >::
       (fun () ->
	  ok 1 @@ minimum [1;2;10]);
     "scanl" >::
       (fun () ->
	  ok [0; 1; 3; 6] @@ scanl (+) 0 [1;2;3];
	  ok [0;-1;-3;-6] @@ scanl (-) 0 [1;2;3]);
     "scanl1" >::
       (fun () ->
	  ok [0; 1; 3; 6] @@ scanl1 (+) [0;1;2;3];
	  ok [0;-1;-3;-6] @@ scanl1 (-) [0;1;2;3];
	  ok []           @@ scanl1 (+) []);
     "scanr" >::
       (fun () ->
	  ok [6;5;3;0] @@ scanr (+) 0 [1;2;3]);
     "scanr1" >::
       (fun () ->
	  ok [6;5;3;0] @@ scanr1 (+) [0;1;2;3];
	  ok []        @@ scanr1 (+) []);
     "replicate" >::
       (fun () ->
	  ok [1;1;1] @@ replicate 3 1;
	  ok []      @@ replicate 0 1);
     "take" >::
       (fun () ->
	  ok [1;2;3] @@ take 3 [1;2;3;4;5];
	  ok [] @@ take 0 [1;2;3;4;5];
	  ok [] @@ take ~-1 [1;2;3;4;5];
	  ok [1;2;3] @@ take 10 [1;2;3]);
     "splitAt" >::
       (fun () ->
	  ok ([1],[2;3]) @@ splitAt 1 [1;2;3];
	  ok ([],[1;2;3]) @@ splitAt 0 [1;2;3];
	  ok ([1;2;3],[]) @@ splitAt 100 [1;2;3];
	  ok ([],[]) @@ splitAt 100 []);
     "takeWhile" >::
       (fun () ->
	  ok [1;2] @@ takeWhile (fun x -> x < 3) [1;2;3]);
     "dropWhile" >::
       (fun () ->
	  ok [3] @@ dropWhile (fun x -> x < 3) [1;2;3]);
     "span" >::
       (fun () ->
	  ok ([1;2],[3;4;1;2;3;4]) @@ span (fun x -> x < 3) [1;2;3;4;1;2;3;4]);
     "break" >::
       (fun () ->
	  ok ([1;2;3],[4;1;2;3;4]) @@ break (fun x -> x > 3) [1;2;3;4;1;2;3;4]);
     "zip" >::
       (fun () ->
	  ok [(1,2);(3,4);(5,6)] @@ zip [1;3;5] [2;4;6];
	  ok []                  @@ zip [] [2;4;6]);
     "zip3" >::
       (fun () ->
	  ok [(1,2,3);(4,5,6)]   @@ zip3 [1;4;5] [2;5;6] [3;6]);
     "unzip" >::
       (fun () ->
	  ok ([1;3],[2;4]) @@ unzip [(1,2);(3,4)]);
     "unzip3" >::
       (fun () ->
	  ok ([1;3],[2;4],[3;5]) @@ unzip3 [(1,2,3);(3,4,5)])
   ]) +> run_test_tt
