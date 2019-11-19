open Unix
open Log

type t = {to_user: string; time: float; from_user: string; message: string}

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

let rec parse_help slist =
  match slist with
  | to_user :: time :: from_user :: h :: t ->
    { to_user
    ; time= float_of_string time
    ; from_user
    ; (* TODO: malformed input ?*)
      message= (h ^ if t <> [] then "|" ^ String.concat "|" t else "") }
  | _ ->
    raise (Failure "ill-formatted string")

let parse s = parse_help (String.split_on_char '|' s)

let make to_user time from_user message = {to_user; time; from_user; message}

let pack to_user time from_user message =
  to_user ^ "|" ^ string_of_float time ^ "|" ^ from_user ^ "|" ^ message

let pack_t t =
  t.to_user ^ "|" ^ string_of_float t.time ^ "|" ^ t.from_user ^ "|"
  ^ t.message

let get_from_user t = t.from_user

let get_to_user t = t.to_user

let format t = "[" ^ format_time t.time ^ "] " ^ t.from_user ^ ": " ^ t.message
