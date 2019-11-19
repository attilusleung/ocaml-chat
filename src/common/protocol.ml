type t =
  | Login of string
  | Message of Parser.t
  | Status of string list * string list
  | Malformed

let strip_head msg = String.sub msg 1 (String.length msg - 1)

let decode msg =
  match msg.[0] with
  | 'M' ->
    Message (Parser.parse @@ strip_head msg)
  | 'L' ->
    let len = String.length msg in
    if len > 1 then Login (strip_head msg)
    else Malformed
  | 'S' -> (
    let rec build_status accept reject = function
      | h :: t -> (
        match h.[0] with
        | 'A' -> build_status (strip_head msg :: accept) reject t
        | 'R' -> build_status accept (strip_head msg :: reject) t
        | _ -> Malformed )
      | [] -> Status (accept, reject)
    in
    String.split_on_char '|' @@ strip_head msg |> build_status [] [] )
  | _ ->
    Malformed

let encode_msg to_user time from_user msg =
  "M" ^ Parser.pack to_user time from_user msg

let encode_parsed_msg p = "M" ^ Parser.pack_t p

let encode_login username = "L" ^ username
