open Unix

type t = {time : string; user : string; message : string}

let rec parse_help slist p =
  match slist with
  | [] -> raise (Failure "ill-formatted string")
  | h :: t -> 
    if p.time = "" 
    then 
      let tm = () |> time |> localtime in
      let hour = 
        if tm.tm_hour |> string_of_int |> String.length < 2 
        then "0" ^ (tm.tm_hour |> string_of_int) 
        else (tm.tm_hour |> string_of_int) in
      let minute = 
        if tm.tm_min |> string_of_int |> String.length < 2 
        then "0" ^ (tm.tm_min |> string_of_int) 
        else (tm.tm_min |> string_of_int) in
      parse_help (h::t) 
        {time = hour ^ ":" ^ minute; user = ""; message = ""} 
    else
    if p.user = "" 
    then parse_help t {time = p.time; user = "user" ^ h; message = ""}
    else {time = p.time; user = p.user; 
          message = h ^ (if t <> [] then "|" ^ (String.concat "|" t) else "")}

let parse s =
  parse_help (String.split_on_char '|' s) {time = ""; user = ""; message = ""}