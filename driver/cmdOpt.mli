type scm = {
  scm_cmd : string;
  includes : string;
  link_std:bool
}

type link = {
  link_cmd : string;
  size : int * int;
  bg_color : Color.t;
  libs : string list;
  use_network: bool
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
  link : link;
}

val parse : string array -> t
