open Unix
open Log

type form_message = {format: string; text: string}

type t =
  { to_user: string
  ; time: float
  ; from_user: string
  ; message: form_message list
  ; org_msg: string }

let make_formatted format text = {format; text}

(** [format_message strs] is the list of formatted strings from the list of
    strings [strs] that may contain formatting instructions. *)
let format_message string_list =
  let to_format string =
    if String.length string = 0 then "|"
    else if String.length string = 1 then string
    else
      match string.[0] with
      | 'b' ->
        "\027[1m"
      | 'u' ->
        "\027[4m"
      | 'r' ->
        "\027[31m"
      | 'g' ->
        "\027[32m"
      | 'y' ->
        "\027[33m"
      | 'n' ->
        "\027[0m"
      | _ ->
        string
  in
  let rec format_helper list =
    match list with
    | [] ->
      []
    | h :: t ->
      if to_format h = h || to_format h = "|" then
        {format= "\027[0m"; text= to_format h} :: format_helper t
      else
        {format= to_format h; text= String.sub h 1 (String.length h - 1)}
        :: format_helper t
  in
  (* let remove_empty list = *)
  (*   if List.hd list = "" then List.tl list else list *)
  (* in *)
  match string_list with
  | "" :: t ->
    format_helper t
  | h :: t ->
    {format= "\027[0m"; text= h} :: format_helper t
  | [] ->
    []

(** [format_time time] is the local 24-hour time that is displayed in a client's
    terminal based on float-time [time].
    Example: [format_time 1575670849.] is ["17:20"] in Eastern Standard Time. *)
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

let parse s =
  match (String.split_on_char '|' s) with
  | to_user :: time :: from_user :: t ->
    { to_user
    ; time= float_of_string time
    ; from_user
    ; message= format_message t
    ; org_msg= String.concat "|" t }
  | _ ->
    raise (Failure "ill-formatted string")

let rec make_message p =
  match p with [] -> "" | h :: t -> h.text ^ make_message t

let make to_user time from_user org_msg =
  { to_user
  ; time
  ; from_user
  ; message= org_msg |> String.split_on_char '|' |> format_message
  ; org_msg }

let pack to_user time from_user message =
  to_user ^ "|" ^ string_of_float time ^ "|" ^ from_user ^ "|" ^ message

let get_from_user t = t.from_user

let get_to_user t = t.to_user

let format t = "[" ^ format_time t.time ^ "] " ^ t.from_user ^ ": "

(** [to_string_list s] is the string [s] as a list of strings, where each string
    in the list is either a single character or a single character with
    formatting instructions. *)
let rec to_string_list string =
  match string with
  | "" ->
    []
  | str ->
    if String.length str = 1 then
      (str ^ "\027[0m")
      :: to_string_list (String.sub str 1 (String.length str - 1))
      (* then (str) :: (to_string_list (String.sub str 1 ((String.length str)-1))) *)
    else
      String.sub str 0 1
      :: to_string_list (String.sub str 1 (String.length str - 1))

let rec message_to_string message =
  match message with
  | [] ->
    []
  | h :: t ->
    ( h.format ^ String.sub h.text 0 1
      ^ if String.length h.text = 1 then "\027[0m" else "" )
    :: to_string_list (String.sub h.text 1 (String.length h.text - 1))
    @ message_to_string t

let output_list t = to_string_list (format t) @ message_to_string t.message

let pack_t t =
  t.to_user ^ "|" ^ string_of_float t.time ^ "|" ^ t.from_user ^ "|"
  ^ t.org_msg

let get_message t = t.org_msg
