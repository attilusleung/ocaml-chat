(** [Client] is the backend of the client. This includes login, setting active
 * users, or parsing incoming messages.
 *
 * This module is meant to be abstracted from the networking aspects of the
 * client, which should be located within [Network] *)

exception NotLoggedIn
(** [NotLoggedIn] is thrown when there is an attempt to access any user specific
 * information without being logged in *)

exception AlreadyLoggedIn
(** [AlreadyLoggedIn] is thrown when there is an attempt to log in when there
 * already is a user logged in to the client. *)

type logs = (string, Parser.t DoublyLinkedList.t) Hashtbl.t
(** [logs] is the type for storing chat messages from different users.
 *
 * The string key represents the username whom the logged in user is
 * communicating with, and the messages are stored as a doubly-linked list of
 * parsed messages*)

val login_user : string -> bool
(** [login_user login_message] is true if sending [login_message] to the server
 * successfully logs in the user, and false otherwise.
 *
 * The function also has the side effect of marking the user in [login_message]
 * as logged in, allowing user-specific information to be accessed. *)

val get_user : unit -> string
(** [get_user ()] is the username of the current logged in user. Throws
 * [NotLoggedIn] if no user is logged in. *)

val select_user : string -> unit
(** [select_user user] sets [user] as the "active" user that the logged in user
 * is trying to talk to.
 *
 * This is mainly used for StatusPanel, where the user may switch active users
 * to talk to.*)

val get_selected : unit -> string
(** [get_user ()] retrieves the "active" user that the logged in user is trying
 * to talk to.
 *
 * This is mainly used for MessagePanel and InputPanel, which uses the function 
 * to determine which messages to show or who to send the messages to. *)

val handle_msg : logs -> string list ref -> string -> unit
(** [handle_msg logs users msg] decodes and parses [msg], and performs actions
 * depending on its contents.
 *
 * If [msg] contains a message, then the message is logged to [logs] so it can
 * be displayed in the corresponding message panel.
 * If [msg] constains a status, then [users] is modified to represent the
 * new list of online users.
 * If [msg] is not understood or parsable, the function does nothing. *)
