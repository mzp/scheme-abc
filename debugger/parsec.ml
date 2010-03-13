open Base

module type T = sig
  type ts
  type error
end

module Make(T : T) = struct
  type ts = T.ts
  type error = T.error
  type 'a m = ts -> ('a * ts, error) either

  let return x = fun code -> Left (x, code)
  let bind p f = fun code ->
    match p code with
      | Left (x, ts) -> f x ts
      | Right err    -> Right err

  let (<|>) p1 p2 = fun code ->
    match p1 code, p2 code with
      | (Left _ as x) , _              -> x
      | _             , (Left _ as x)  -> x
      | Right _       , Right err      -> Right err

  let rec many p =
    perform
      x  <-- p;
      xs <-- (many p <|> return []);
      return (x::xs)

  let opt p =
    (perform
      x <-- p;
      return (Some x))
    <|> return None
end
