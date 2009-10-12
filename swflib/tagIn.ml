open Base
open SwfBaseIn

let tag n s =
  match Stream.peek s with
      Some m when n = m ->
	Stream.junk s;
	()
    | None | Some _ ->
	raise Stream.Failure

let of_base = parser
    [< _ = tag 0 >] ->
      `End
  | [< _ = tag 9; color = rgb >] ->
      `SetBackgroundColor color
  | [< >] ->
      raise Stream.Failure
