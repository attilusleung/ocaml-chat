(** [Network] contains all networking functions used by the client. *)

(** [server_arg] is the type representing the arguements that specify the server
 * which the client should connect to. *)
type server_arg = Address of string | Alias of string

type connection
(** [connection] is the type that represents a connection to the server. *)

val create_connection : server_arg -> connection Lwt.t
(** [create_connection arg] creates a connection to the server described in
 * [arg] *)

val send_msg : connection -> string -> unit Lwt.t
(** [send_msg connection msg] sends the message [msg] to the server described in
 * [connection] *)

val send_msg_blocking : connection -> string -> unit
(** [send_msg_blocking connection msg] is the same as [send_msg], except that it
 * blocks. *)

val listen_msg :
  connection -> Client.logs -> string list ref -> unit -> 'a Lwt.t
(** [listen_msg conn logs users ()] starts a thread that listens and handles any
 * messages sent from [conn], storing incoming messages in [logs] and modifying
 * [users] whenever there are changes in the users that are online/offline *)

val get_msg : connection -> string Lwt.t
(** [get_msg conn] is the message that the server in [conn] sends to the client. *)
