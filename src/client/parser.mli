
(* type t = {time : string; user : string; message : string} *)
type t

val pack : float -> string -> string -> string

val pack_t : t -> string

val parse : string -> t

val get_all : t -> (float * string * string)

val format : t -> string
