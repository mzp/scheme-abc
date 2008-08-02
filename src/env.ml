type bind = Scope of int  | Register of int
type t  = {depth:int; binding: (string * bind) list }

let empty_env =
  {depth=0; binding=[]}

let add_scope names {depth=n;binding=xs} =
  let names' =
    List.map (fun name-> name,Scope n) names in
    {depth=n+1; binding=names' @ xs}

let add_current_scope name {depth=n;binding=xs} =
    {depth=n; binding=(name,Scope (n-1))::xs}

let add_register names env =
  let names' = 
    ExtList.List.mapi (fun i name-> name,Register (i+1)) names in
    {env with binding = names'@env.binding}

let get_bind name {binding=xs} =
  List.assoc name xs

let get_bind_sure name state =
  try
    Some (get_bind name state)
  with Not_found ->
    None

let is_bind name {binding=xs} =
  List.mem_assoc name xs

let ensure_scope name env =
  match get_bind name env with
      Scope x -> 
	x
    | _ ->
	failwith ("scope not found:"^name)
