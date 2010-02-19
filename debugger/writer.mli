module type Monoid = sig
  type t
  val mempty : t
  val mappend : t -> t -> t
end

module Make : functor(W: Monoid) -> sig
  type w = W.t
  type 'a m

  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val ret  : 'a -> 'a m

  val pass : ('a * (w -> w)) m -> 'a m
  val listen : 'a m -> ('a * w) m
  val tell   : w -> unit m

  val runWriter : 'a m -> ('a * w)
end
