open Unix
open Log

type form_message = {format : string; text: string}
type t = {to_user: string; time: float; from_user: string; message: form_message list}

let format_message string_list =
  let to_format string = 
    if String.length string = 1 then string
    else
      match string.[0] with
      | 'b' -> "\027[1m"
      | 'u' -> "\027[4m"
      | 'r' -> "\027[31m"
      | 'g' -> "\027[32m"
      | 'y' -> "\027[33m"
      | _ -> string
  in

  let rec format_helper list =
    match list with
    | [] -> []
    | h :: t -> if (to_format h) = h 
      then {format = "\027[0m"; text = h} :: format_helper t
      else {format = to_format h; text = String.sub h 1 ((String.length h)-1)} :: format_helper t

  in

  let remove_empty list =
    if List.hd list = "" then List.tl list else list
  in

  format_helper (remove_empty string_list)


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
  | to_user :: time :: from_user :: t ->
    { to_user
    ; time= float_of_string time
    ; from_user
    ; (* TODO: malformed input ?*)
      message= (format_message t) }
  | _ ->
    raise (Failure "ill-formatted string")

let parse s = parse_help (String.split_on_char '|' s)

let rec make_message p = 
  match p with
  | [] -> ""
  | h :: t -> h.text ^ (make_message t)

let make to_user time from_user message = {to_user; time; from_user; message}

let pack to_user time from_user message =
  to_user ^ "|" ^ string_of_float time ^ "|" ^ from_user ^ "|" ^ message

let pack_t t =
  t.to_user ^ "|" ^ string_of_float t.time ^ "|" ^ t.from_user ^ "|"
  ^ make_message t.message

let get_message t = t.message

let get_from_user t = t.from_user

let get_to_user t = t.to_user

let format t = "[" ^ format_time t.time ^ "] " ^ t.from_user ^ ": "

let rec to_string_list string = 
  match string with
  | "" -> []
  | str -> 
    if String.length str = 1 
    then (str ^ "\027[0m") :: (to_string_list (String.sub str 1 ((String.length str)-1)))
    else
      (String.sub str 0 1) :: (to_string_list (String.sub str 1 ((String.length str)-1)))

let output_list t =
  let rec message_to_string message =
    match message with
    | [] -> []
    | h :: t -> 
      (h.format ^ (String.sub h.text 0 1)) :: 
      (to_string_list (String.sub h.text 1 ((String.length h.text)-1))) @ 
      message_to_string t
  in
  to_string_list (format t) @ (message_to_string t.message)