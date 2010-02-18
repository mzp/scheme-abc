open Base

module type Monoid = sig
  type t
  val mempty : t
  val mappend : t -> t -> t
end

module Make = functor (W : Monoid) ->
  struct
    type 'a m = 'a * W.t

    let ret a =
      (a, W.mempty)

    let bind (a,w) f =
      let (a', w') = f a in
	(a', W.mappend w w')

    let pass ((a, f), w) =
      (a, f w)

    let listen (a, w) =
      ((a,w), w)

    let tell s =
      ((), s)

    let runWriter =
      id
  end
