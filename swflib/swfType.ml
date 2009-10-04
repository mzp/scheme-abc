type rect = {
  left   : int;
  right  : int;
  top    : int;
  bottom : int;
}

type 'a tag = {
  tag : int;
  data : 'a list
}

type 'a t = {
  version:    int;
  frame_size:  rect;
  frame_rate:  float;
  frame_count: int;
  tags : 'a tag list
}
