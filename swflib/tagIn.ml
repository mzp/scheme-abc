open Base
open SwfBaseIn

let tag n s =
  match Stream.peek s with
      Some m when n = m ->
	Stream.junk s;
	()
    | None | Some _ ->
	raise Stream.Failure

let of_base tag stream =
  match tag with
      0 ->
	`End
    | 9 ->
	`SetBackgroundColor (rgb stream)
    | _ ->
	failwith "of_base"
