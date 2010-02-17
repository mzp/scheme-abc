open Base

let db n xs = xs @ [(string_of_char @@ Char.chr n)]
let (+>>) _ _ = assert false
