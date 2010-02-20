open Base
open Serialize

type quality =
    Low
  | Medium
  | High
  | AutoLow
  | AutoMedium
  | AutoHigh
  | Best

type t =
    ZoomIn
  | ZoomOut
  | Zoom100
  | Home
  | Quality of quality
  | Play
  | Loop
  | Rewind
  | Forward
  | Back
  | Print

let serialize _ =
  assert false


(*
00 zoom in
01 zoom out
02 zoom 100%
03 home
04 set quality        sz
 quality=”LOW”/”MEDIUM”/”HIGH”/”AUTOLOW”/”AUTOMEDIUM”/”AUTOHIGH”/”BEST”
05 play
06 loop
07 rewind
08 forward
09 back
0A print
*)
