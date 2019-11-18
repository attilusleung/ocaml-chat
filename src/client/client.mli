exception NotLoggedIn

(* type user *) (* TODO: do we need this *)

val login_user : string -> unit

val get_user : unit -> string

val select_user : string -> unit

val get_selected: unit -> string
