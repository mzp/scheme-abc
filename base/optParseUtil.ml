open OptParse

module type Parser = sig
  val v : OptParser.t
end

module Make(Parser : Parser) = struct
  let opt_parser = Parser.v

  let str_option ~default ~metavar ?short_name ?long_name ~help () =
    let store =
      StdOpt.str_option ~default ~metavar () in
    let _ =
      OptParser.add opt_parser
	?short_name
	?long_name ~help store in
      store

  let int_option ~default ~metavar ?short_name ?long_name ~help () =
    let store =
      StdOpt.int_option ~default ~metavar () in
    let _ =
      OptParser.add opt_parser
	?short_name ?long_name ~help store in
      store

  let bool_option ~default ?short_name ?long_name ~help () =
    let store =
      if default then
	StdOpt.store_false ()
      else
	StdOpt.store_true () in
    let _ =
      OptParser.add opt_parser
	?short_name ?long_name ~help store in
      store

let str_callback ?short_name ?long_name ~help f =
  let opt = {
    Opt.option_metavars = [];
    option_defhelp = Some help;
    option_get = (fun _ -> raise Opt.No_value);
    option_set_value = (fun _ -> ());
    option_set = (fun _ _ ->
		    f ();
		    exit 0)
  } in
    OptParser.add opt_parser
      ?short_name
      ?long_name ~help opt
end
