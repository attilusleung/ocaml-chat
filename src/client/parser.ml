open Unix

type t = {time : string; user : string; message : string}

let rec parse_help slist p =
  match slist with
  | user :: h :: t -> 
    let tm = () |> time |> localtime in
    let hour = 
      if tm.tm_hour < 10 
      then "0" ^ (tm.tm_hour |> string_of_int) 
      else (tm.tm_hour |> string_of_int) in
    let minute = 
      if tm.tm_min < 10 
      then "0" ^ (tm.tm_min |> string_of_int) 
      else (tm.tm_min |> string_of_int) in
    {time = hour ^ ":" ^ minute; user = "user" ^ user; 
     message = h ^ (if t <> [] then "|" ^ (String.concat "|" t) else "")}
  | _ -> raise (Failure "ill-formatted string")

let parse s =
  parse_help (String.split_on_char '|' s) {time = ""; user = ""; message = ""}