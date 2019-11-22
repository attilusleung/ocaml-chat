
type form_message = {format : string; text: string}

(* type t = {time : string; user : string; message : string} *)
type t

val pack : string -> float -> string -> string -> string

val pack_t : t -> string

val make : string -> float -> string -> form_message list -> t

val parse : string -> t

val get_message : t -> form_message list

val get_from_user : t -> string

val get_to_user : t -> string

val format : t -> string

val make_message : form_message list -> string

val output_list : t -> string list