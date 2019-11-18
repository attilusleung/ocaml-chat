open Unix
open Log

type t = {time: float; user: string; message: string}

let format_time time =
  let tm = localtime time in
  let hour =
    if tm.tm_hour < 10 then "0" ^ (tm.tm_hour |> string_of_int)
    else tm.tm_hour |> string_of_int
  in
  let minute =
    if tm.tm_min < 10 then "0" ^ (tm.tm_min |> string_of_int)
    else tm.tm_min |> string_of_int
  in
  hour ^ ":" ^ minute

let rec parse_help slist p =
  match slist with
  | time :: user :: h :: t ->
    { time= float_of_string time
    ; user= user
    ; (* TODO: malformed input ?*)
      message= (h ^ if t <> [] then "|" ^ String.concat "|" t else "") }
  | _ ->
    raise (Failure "ill-formatted string")

let parse s =
  parse_help (String.split_on_char '|' s) {time= 0.; user= ""; message= ""}

let pack time user message = string_of_float time ^ "|" ^ user ^ "|" ^ message

let pack_t t = string_of_float t.time ^ "|" ^ t.user ^ "|" ^ t.message

let get_all t = t.time, t.user, t.message

let format t = "[" ^ (format_time t.time) ^ "] " ^ t.user ^ ": " ^ t.message
