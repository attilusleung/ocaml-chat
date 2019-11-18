
type t = {time : string; user : string; message : string}

val parse : string -> t

val format : t -> string

val p_to_string : t -> string