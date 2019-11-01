
type t = {user : string; time : string; message : string}

let rec parse_help slist p =
  match slist with
  | [] -> raise (Failure "ill-formatted string")
  | h :: t -> 
    if p.user = "" then parse_help t {user = h; time = ""; message = ""} else 
    if p.time = "" then parse_help t {user = p.user; time = h; message = ""}
    else {user = p.user; time = p.time; message = h ^ (String.concat "" t)}

let parse s =
  parse_help (String.split_on_char '|' s) {user = ""; time = ""; message = ""}