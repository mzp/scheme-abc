type rect = {
  left   : int;
  right  : int;
  top    : int;
  bottom : int;
}

type color = {
  red : int;
  green: int;
  blue : int;
}

type matrix = {
  scale:     (float * float) option;
  rotate:    (float * float) option;
  translate: (int*int)
}

type 'a t = {
  version:    int;
  frame_size:  rect;
  frame_rate:  float;
  frame_count: int;
  tags : 'a list
}
