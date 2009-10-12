type rect = {
  left   : int;
  right  : int;
  top    : int;
  bottom : int;
}

type matrix = {
  scale:     (float * float) option;
  rotate:    (float * float) option;
  translate: (int*int)
}

