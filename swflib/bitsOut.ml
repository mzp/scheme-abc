open Base

type t = {
  pos     : int;
  content : int list;
  value   : int
}

let cByte = 8

let empty = {
  pos     = 0;
  content = [];
  value   = 0;
}

let rec update t =
  if t.pos < cByte then
    t
  else
    update {
      pos     = t.pos - cByte;
      value   = 0;
      content = t.value :: t.content
    }

let max_int n =
  (1 lsl n) - 1

let rec put t ~width ~bits =
  if bits land max_int width <> bits then
    raise (Invalid_argument "BitsStream.put");
  let remain =
    cByte - t.pos in
    if remain >= width then
      update {t with
		value = t.value lor (bits lsl (remain - width));
		pos   = t.pos + width}
    else
      let low =
	(bits lsr (width - remain)) land max_int remain in
      let high =
	bits land max_int (width-remain) in
      let t1 =
	put t  ~width:remain ~bits:low in
	put t1 ~width:(width - remain) ~bits:high

let to_list {pos; value; content} =
  if pos = 0 then
    List.rev content
  else
    List.rev @@ value::content
