type scm = { scm_cmd : string; includes : string; }
type abc = { abc_cmd : string; }
type abcx = {
  abcx_cmd : string;
  template : string;
  size : int * int;
  bg_color : string;
  main_class: string;
}

type swfx = { swfx_cmd : string; }
type general = { verbose : bool; just_print: bool; }
type t = {
  inputs : string list;
  output : string;
  general : general;
  scm : scm;
  abc : abc;
  abcx : abcx;
  swfx : swfx;
}

val parse : unit -> t
