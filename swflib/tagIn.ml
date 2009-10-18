open Base
open SwfBaseIn

let of_base tag stream =
  match tag with
      0 ->
	`End
    | 9 ->
	`SetBackgroundColor (rgb stream)
    | _ ->
	failwith "of_base"
