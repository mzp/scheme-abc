open Genlex
open Base

exception Invalid_format of string

type t = {red:int; green:int; blue:int; alpha:float}

let rgb r g b = {
  red   = r;
  green = g;
  blue  = b;
  alpha = 1.0
}

let red     = rgb 0xFF 0x00 0x00
let lime    = rgb 0x00 0xFF 0x00
let blue    = rgb 0x00 0x00 0xFF
let white   = rgb 0xFF 0xFF 0xFF
let maroon  = rgb 0x80 0x00 0x00
let green   = rgb 0x00 0xFF 0x00
let navy    = rgb 0x00 0x00 0x80
let silver  = rgb 0xC0 0xC0 0xC0
let yellow  = rgb 0xFF 0xFF 0x00
let aqua    = rgb 0x00 0xFF 0xFF
let fuchsia = rgb 0xFF 0x00 0xFF
let gray    = rgb 0x80 0x80 0x80
let olive   = rgb 0x80 0x80 0x00
let teal    = rgb 0x00 0x80 0x80
let purple  = rgb 0x80 0x00 0x80
let black   = rgb 0x00 0x00 0x00

let name_table = [
  "red",red;
  "lime",lime;
  "blue",blue;
  "white",white;
  "maroon",maroon;
  "green",green;
  "navy",navy;
  "silver",silver;
  "yellow",yellow;
  "aqua",aqua;
  "fuchsia",fuchsia;
  "gray",gray;
  "olive",olive;
  "teal",teal;
  "purple",purple;
  "black",black]

let of_int n =
  rgb
    (n lsr 16 land 0xFF)
    (n lsr 8 land 0xFF)
    (n lsr 0 land 0xFF)

let to_int {red=r; green=g; blue=b} =
  (r lsl 16) lor  (g lsl 8) lor b

let p_name s =
  maybe (List.assoc @@ String.lowercase s) name_table

let hex =
  Str.regexp_case_fold
    "^#\\([0-9A-F][0-9A-F]\\)\\([0-9A-F][0-9A-F]\\)\\([0-9A-F][0-9A-F]\\)$"

let of_hex s =
  Scanf.sscanf s "%x" id

let p_hex s =
  if Str.string_match hex s 0 then
    Some (rgb (of_hex @@ Str.matched_group 1 s)
	      (of_hex @@ Str.matched_group 2 s)
	      (of_hex @@ Str.matched_group 3 s))
  else
    None

let r_rgb =
  Str.regexp_case_fold
    "^rgb( *\\([0-9]+\\) *, *\\([0-9]+\\) *, *\\([0-9]+\\) *)$"

let p_rgb s =
  if Str.string_match r_rgb s 0 then
    Some (rgb (int_of_string @@ Str.matched_group 1 s)
	      (int_of_string @@ Str.matched_group 2 s)
	      (int_of_string @@ Str.matched_group 3 s))
  else
    None

let parse s =
  [p_name;p_hex;p_rgb]
  +> List.map  (fun f -> lazy (f s))
  +> List.find (fun v -> Option.is_some (!$v))
  +> (!$)
  +> Option.get
