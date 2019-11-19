
type t = {time : int; user : string; message : string}

val get_time : unit -> Unix.tm

val parse_time : Unix.tm -> int

val parse : string -> t

val format : t -> string

val p_to_string : t -> string