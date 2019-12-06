exception NotLoggedIn

(* type user *)
(* TODO: do we need this *)

val login_user : string -> bool

val get_user : unit -> string

val select_user : string -> unit

val get_selected : unit -> string

val handle_msg :
  (string, Parser.t DoublyLinkedList.t) Hashtbl.t
  -> string list ref
  -> string
  -> unit
