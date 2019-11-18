open Unix
open Log

type t = {time : string; user : string; message : string}

let parse s =
  match (String.split_on_char '|' s) with
  | a :: b :: h :: t -> 
    {time = a; user = "user" ^ b; 
     message = h ^ (if t <> [] then "|" ^ (String.concat "|" t) else "")}
  | h :: h' :: t -> raise (Failure "why")
  | h :: t -> raise (Failure "what")
  | _ -> raise (Failure "ill-formatted string")

let format p = 
  "[" ^ p.time ^ "] " ^ p.user ^ ": " ^ p.message

let p_to_string p =
  p.time ^ "|" ^ p.user ^ "|" ^ p.message