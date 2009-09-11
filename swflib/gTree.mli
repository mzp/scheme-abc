module type S =
sig
  type 'a t
  val is_leaf : 'a t -> bool
  val subtree : 'a t -> 'a t list
end

module Make :
  functor (S : S) ->
sig
  type 'a t = 'a S.t

  val map :
    leaf:('a t -> 'b) ->
    branch:('a t -> 'b list -> 'b) -> 'a t -> 'b
  val fold :
    leaf:('a -> 'b t -> 'a) ->
    branch:('b t -> 'a -> 'a) -> 'a -> 'b t -> 'a
end
