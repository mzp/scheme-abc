module type Monoid = sig
  type 'a t
  val mempty : 'a t
  val mappend : 'a t -> 'a t -> 'a t
end

module Make : functor(W: Monoid) -> sig
  type ('a,'b) m

  val bind : ('a, 'b) m -> ('a -> ('c, 'b) m) -> ('c, 'b) m
  val ret  : 'a -> ('a, 'b) m

  val pass : (('a * ('b W.t -> 'b W.t)), 'b) m -> ('a,'b) m
  val listen : ('a,'b) m -> ('a * 'b W.t, 'b) m
  val tell   : 'b W.t -> (unit,'b) m

  val runWriter : ('a,'b) m -> ('a * 'b W.t)
end
