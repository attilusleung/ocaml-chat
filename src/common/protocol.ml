type t = Login of string | Message of Parser.t | Malformed

let decode msg =
  match msg.[0] with
  | 'M' ->
    Message (Parser.parse (String.sub msg 1 (String.length msg - 1)))
  | 'L' ->
    let len = String.length msg in
    if len > 1 then Login (String.sub msg 1 (String.length msg - 1))
    else Malformed
  | _ ->
    Malformed

let encode_msg to_user time from_user msg =
  "M" ^ Parser.pack to_user time from_user msg

let encode_login username = "L" ^ username
