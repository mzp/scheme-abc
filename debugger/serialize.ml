open Base
open ExtString

exception Out_of_range

module StrMonoid = struct
  type t = string
  let mempty = ""
  let mappend = (^)
end

module M = Writer.Make(StrMonoid)

let str =
  M.tell $ String.implode $ List.map Char.chr
let db n =
  if n > 0xFF then
    raise Out_of_range;
  str [
    n land 0xFF
  ]

let dw n =
  if n > 0xFFFF then
    raise Out_of_range;
  str [
    n land 0xFF;
    (n lsr 8) land 0xFF
  ]

let dd n =
  str @@ List.map Int32.to_int [
    Int32.logand n 0xFFl;
    Int32.logand (Int32.shift_right n 8) 0xFFl;
    Int32.logand (Int32.shift_right n 16)  0xFFl;
    Int32.logand (Int32.shift_right n 24)  0xFFl;
  ]

let sz s =
  M.tell @@ s ^ "\000"

