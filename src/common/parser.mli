
type form_message

type t

val pack : string -> float -> string -> string -> string

val pack_t : t -> string

val make : string -> float -> string -> string -> t

val parse : string -> t

(* val get_message : t -> form_message list *)

val get_from_user : t -> string

val get_to_user : t -> string

val get_message : t -> string

val format : t -> string

val make_message : form_message list -> string

val output_list : t -> string list

val message_to_string : form_message list -> string list

val make_formatted : string -> string -> form_message
