open Base

module type T = sig
  type ts
  type error
end

module Make : functor(T : T) -> sig
  type ts = T.ts
  type error = T.error
  type 'a m = ts -> ('a * ts, error) either
  val return : 'a -> 'a m
  val bind   : 'a m -> ('a -> 'b m) -> 'b m
  val (<|>)  : 'a m -> 'a m -> 'a m
  val many : 'a m -> 'a list m
  val opt : 'a m -> 'a option m
end
