open Base

type t = [
  `End
| `SetBackgroundColor of int * int * int
| `ShowFrame
]
let to_base = function
    `End ->
      (0,[])
  | `ShowFrame ->
      (1,[])
  | `SetBackgroundColor(r,g,b) ->
      (9,[`RGB(r,g,b)])
