open Base
open OUnit

module S = struct
  type ts = char list
  type error = string
end
module M = Parsec.Make(S)
open M

let alpha = function
    x::xs when 'a' <= x && x <= 'z' ->
      Left (x,xs)
  | _ ->
      Right "not small-alpha"

let digit = function
    x::xs when '0' <= x && x <= '9' ->
      Left (x,xs)
  | _ ->
      Right "not digit"

let ok p expect ?(rest=[]) actual =
  match p actual with
    | Left (x,y) ->
	assert_equal expect x;
	assert_equal rest   y
    | Right _ -> assert_failure "parse fail"

let ng p actual =
  match p actual with
      Left _ -> assert_failure "success"
    | Right _ -> assert_equal true true

let _ = begin "parsec.ml" >::: [
  "name" >:: begin fun () ->
    ok alpha 'a' ['a'];
    ng alpha ['Z'];
  end;
  "bind" >:: begin fun () ->
    let p =
      perform with module M in begin
	x <-- alpha;
	y <-- alpha;
	ret (x,y)
      end in
    ok p ('a','b') ['a';'b'];
    ng p ['a'];
  end;
  "many" >:: begin fun () ->
    ok (many alpha) ['a';'a';'b'] ['a';'a';'b'];
    ok (many alpha) ~rest:['Z'] ['a';'a';'b'] ['a';'a';'b';'Z']
  end;
  "opt" >:: begin fun () ->
    ok (opt alpha) (Some 'a') ['a'];
    ok (opt alpha) ~rest:['A'] None ['A']
  end;
  "or" >:: begin fun () ->
    ok (digit <|> alpha) '0' ['0'];
    ok (digit <|> alpha) 'a' ['a'];
    ng (digit <|> alpha) ['Z']
  end;
] end +> run_test_tt_main
