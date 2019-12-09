(** The type of a protocol message. *)
type t =
  | Login of string * string
  | Message of Parser.t
  | Status of string list * string list
  | Confirm of string
  | Fail of string
  | Malformed

(** [strip_head msg] is [msg] without its first character. *)
let strip_head msg = String.sub msg 1 (String.length msg - 1)

(** [decode msg] is the protocol message extracted from the plaintext [msg]. *)
let decode msg =
  match msg.[0] with
  | 'M' ->
    Message (Parser.parse @@ strip_head msg)
  | 'L' -> (
      match strip_head msg |> String.split_on_char '|' with
      | h :: t ->
        let pass = String.concat "|" t |> String.trim in
        Login (h, pass)
      | _ ->
        Malformed )
  | 'C' ->
    let len = String.length msg in
    if len > 1 then Confirm (strip_head msg) else Malformed
  | 'S' ->
    let rec build_status accept reject = function
      | h :: t -> (
          match h.[0] with
          | 'A' ->
            build_status (strip_head h :: accept) reject t
          | 'R' ->
            build_status accept (strip_head h :: reject) t
          | _ ->
            Malformed )
      | [] ->
        Status (accept, reject)
    in
    String.split_on_char '|' @@ strip_head msg |> build_status [] []
  | 'F' ->
    Fail (strip_head msg)
  | _ ->
    Malformed

(** [encode_msg to_user time from_user msg] encodes [to_user], the time [time],
    [from_user], and the original message [msg] into a protocol-readable 
    plaintext message. *)
let encode_msg to_user time from_user msg =
  "M" ^ Parser.pack to_user time from_user msg

(** [encode_parsed_msg p] encodes the parsed message [p] into a protocol 
    message. *)
let encode_parsed_msg p = "M" ^ Parser.pack_t p

(** [encode_login username password] encodes the user [username] and their 
    password [password] into a protocol login message. *)
let encode_login username password = "L" ^ username ^ "|" ^ password

(** [encode_confirm username] encodes the user [username] into a protocol 
    confirm message. *)
let encode_confirm username = "C" ^ username

(** The protocol fail message. *)
let encode_fail = "F"

(** [encode_status accepted rejected] encodes the accepted users [accepted] 
    and the rejected users [rejected] into a protocol status message. *)
let encode_status accepted rejected =
  let acc = Buffer.create 30 in
  let rec encode c = function
    | h :: t ->
      Buffer.add_char acc c ;
      Buffer.add_string acc h ;
      Buffer.add_char acc '|' ;
      encode c t
    | [] ->
      ()
  in
  Buffer.add_char acc 'S';
  encode 'A' accepted ;
  encode 'R' rejected ;
  Buffer.truncate acc (Buffer.length acc - 1) ;
  (* print_endline @@ Buffer.contents acc; *)
  Buffer.contents acc
