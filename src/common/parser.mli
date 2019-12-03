
type t

val pack : string -> float -> string -> string -> string

val pack_t : t -> string

val make : string -> float -> string -> string -> t

val parse : string -> t

val get_from_user : t -> string

val get_to_user : t -> string

val get_message : t -> string

val format : t -> string
