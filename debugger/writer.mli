module type Monoid = sig
  type t
  val mempty : t
  val mappend : t -> t -> t
end

module Make : functor(W: Monoid) -> sig
  type 'a m

  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val ret  : 'a -> 'a m

  val pass : ('a * (W.t -> W.t)) m -> 'a m
  val listen : 'a m -> ('a * W.t) m
  val tell   : W.t -> unit m

  val runWriter : 'a m -> ('a * W.t)
end
