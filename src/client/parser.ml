open Unix
open Log

type t = {time : string; user : string; message : string}

let parse s =
  log_out s;
  match (String.split_on_char '|' s) with
  | a :: b :: h :: t -> 
    {time = a; user = "user" ^ b; 
     message = h ^ (if t <> [] then "|" ^ (String.concat "|" t) else "")}
  | h :: h' :: t -> raise (Failure "why")
  | h :: t -> raise (Failure "what")
  | _ -> raise (Failure "ill-formatted string")
