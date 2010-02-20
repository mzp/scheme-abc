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

val serialize : t -> int * string
