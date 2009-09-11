open OUnit
open Base

type 'a tree = Leaf of 'a | Branch of 'a tree * 'a tree

module T = GTree.Make(
  struct
    type 'a t = 'a tree

    let is_leaf =
      function
	  Leaf _ -> true
	| Branch _ -> false

    let subtree =
      function
	  Leaf _ -> []
	| Branch (l,r) -> [l;r]

  end)

let ok =
  assert_equal

let _ =
  ("gTree.ml" >:::[
     "map/leaf" >::
       (fun () ->
	  let leaf =
	    function
		Leaf _ -> Leaf 42
	      | Branch _ as x -> x in
	  let branch tree xs =
	    match tree,xs with
		Branch _ ,[l;r] ->
		  Branch (l,r)
	      | Branch _,_ | Leaf _,_ ->
		  failwith "must not happen" in
	    assert_equal (Leaf 42) @@
	      T.map ~leaf ~branch @@
	      Leaf 1;
	    assert_equal (Branch (Leaf 42,Leaf 42)) @@
	      T.map ~leaf ~branch @@
	      Branch (Leaf 1,Leaf 0));
     "fold" >::
       (fun () ->
	  let tree =
	    Branch (Leaf 0,Branch (Leaf 1,Leaf 2)) in
	  let leaf x =
	    function
		Leaf n -> x+n
	      | Branch _ -> failwith "must not happen" in
	  let branch tree subtree =
	    match tree,subtree with
	      | Branch _, xs -> 1+xs
	      | Leaf _ ,_  -> failwith "must not happen" in
	    assert_equal 5 @@
	      T.fold ~leaf ~branch 0 tree)
   ]) +> run_test_tt_main
