open Base

module type S = sig
  type 'a t
  val is_leaf : 'a t -> bool
  val subtree : 'a t -> 'a t list
end

module Make(S : S) = struct
  open S

  type 'a t = 'a S.t
  let rec map ~leaf ~branch tree =
    if is_leaf tree then
      leaf tree
    else
      branch tree @@ List.map (map ~leaf ~branch) @@ subtree tree

  let rec fold ~leaf ~branch init tree =
    if is_leaf tree then
      leaf init tree
    else
      branch tree @@ List.fold_left (fold ~leaf ~branch) init @@ subtree tree
end
