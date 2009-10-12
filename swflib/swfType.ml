
type 'a t = {
  version:    int;
  frame_size:  SwfBaseType.rect;
  frame_rate:  float;
  frame_count: int;
  tags : 'a list
}
