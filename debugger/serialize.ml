open Base

module StrMonoid = struct
  type t = string
  let mempty = ""
  let mappend = (^)
end

module M = Writer.Make(StrMonoid)


let db _ = assert false
let dw _ = assert false
let dd _ = assert false
let sz _ = assert false

