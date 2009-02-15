type output_type = Ho | Abc | Abcx | Swfx | Swf
type scm = { scm_cmd : string; includes : string; }
type abc = { abc_cmd : string; }
type abcx = { template : string; size : int * int; bg_color : string; }
type swfx = { swfx_cmd : string; }
type general = { verbose : bool; remain : bool; }
type t = {
  inputs : string list;
  output : string;
  output_type : output_type;
  general : general;
  scm : scm;
  abc : abc;
  abcx : abcx;
  swfx : swfx;
}
val parse : string array -> t
