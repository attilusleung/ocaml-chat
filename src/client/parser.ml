open Unix
open Log

type t = {time : int; user : string; message : string}

let get_time () = 
  () |> time |> localtime

let parse_time tm =
  tm.tm_hour * 100 + tm.tm_min

let parse s =
  match (String.split_on_char '|' s) with
  | a :: b :: h :: t -> 
    begin 
      try
        {time = int_of_string a; user = b; 
         message = h ^ (if t <> [] then "|" ^ (String.concat "|" t) else "")}
      with _ -> failwith "ill-formatted string"
    end
  | _ -> raise (Failure "ill-formatted string")

let format p = 
  let hour = if p.time / 100 < 10 
    then "0" ^ string_of_int (p.time / 100) 
    else string_of_int (p.time / 100) in
  let minute = if p.time mod 100 < 10
    then "0" ^ string_of_int (p.time mod 100)
    else string_of_int (p.time mod 100) in
  "[" ^ (hour ^ ":" ^ minute) ^ "] " ^ p.user ^ ": " ^ p.message

let p_to_string p =
  (string_of_int p.time) ^ "|" ^ p.user ^ "|" ^ p.message