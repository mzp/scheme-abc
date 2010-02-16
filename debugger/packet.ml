open Base

let parse packet =
  let len =
    Char.code packet.[0] in
  let id =
    Char.code packet.[1] in
  let data =
    String.sub packet 2 len in
    (id, data)

let make  id data =
  Printf.sprintf "%s%s%s"
    (string_of_char @@ Char.chr @@ String.length data)
    (string_of_char @@ Char.chr id)
    data
