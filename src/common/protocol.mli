(** [Protocol] is a module of functions relating to commands sent betwen the
 * server and the client.
 *
 * This is not to be confused with [Parser], which handles messages and metadata
 * associated with them instead of commands. *)

(** [t] is a type representing a command *)
type t =
  | Login of string * string
  | Register of string * string
  | Message of Parser.t
  | Status of string list * string list
  | Confirm of string
  | Fail of string
  | Malformed

(** [decode msg] is the protocol message extracted from the plaintext [msg]. *)
val decode : string -> t

(** [encode_msg to_user time from_user msg] is an encoded message command from
 * [from_user] to [to_user] with the timestamp [time] and a unparsed message
 * [msg] *)
val encode_msg : string -> float -> string -> string -> string

(** [encode_parsed_msg msg] is an encode message command created from the parsed
 * message [msg] *)
val encode_parsed_msg : Parser.t -> string

(** [encode_login username password] is an encoded login command with username
 * [username] and password [password] *)
val encode_login : string -> string -> string

(** [encode_register username password] is an encoded register command with 
 * username [username] and password [password] *)
val encode_register : string -> string -> string

(** [encode_confirm username] is an encoded confirmation message confirming a
 * successful login with username [username] *)
val encode_confirm : string -> string

(** [encode_fail] is an encoded fail message signifying a failed command *)
val encode_fail : string

(** [encode_status login logout] is a status message signifing that all
 * users in [login] have just came online, and all users in [logout] have just
 * gone offline. *)
val encode_status : string list -> string list -> string
