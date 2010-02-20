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

let of_quality = function
    Low -> "LOW"
  | Medium -> "MEDIUM"
  | High -> "HIGH"
  | AutoLow -> "AUTOLOW"
  | AutoMedium -> "AUTOMEDIUM"
  | AutoHigh -> "AUTOHIGH"
  | Best -> "BEST"

let cmd id =
  (id,"")

let arg id m =
  (id, snd @@ runWriter m)

let serialize = function
    ZoomIn ->
      cmd 0x00
  | ZoomOut ->
      cmd 0x01
  | Zoom100 ->
      cmd 0x02
  | Home ->
      cmd 0x03
  | Quality q ->
      arg 0x04 @@ sz @@ of_quality q
  | Play ->
      cmd 0x05
  | Loop ->
      cmd 0x06
  | Rewind ->
      cmd 0x07
  | Forward ->
      cmd 0x08
  | Back ->
      cmd 0x09
  | Print ->
      cmd 0x0A

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
