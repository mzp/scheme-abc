type scm = {
  scm_cmd : string;
  includes : string;
  link_std:bool
}

type abc = {
  abc_cmd : string;
}

type abcx = {
  abcx_cmd : string;
  template : string;
  size : int * int;
  bg_color : Color.t;
}

type swfx = {
  swfx_cmd : string;
}

type general = {
  verbose : bool;
  just_print: bool;
  keep_files: bool;
}

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
