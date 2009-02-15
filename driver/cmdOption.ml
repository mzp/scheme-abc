(** command-line option *)
type filename = string
type template = string
type file =
  | Scm of filename * string
  | Ho  of filename
  | Abc of filename
  | Xml of filename
  | Swf of filename * template


type t = {
  verbose: bool;
}
