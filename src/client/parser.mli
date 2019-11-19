(** The type of a parsed message. *)
type t = {time : int; user : string; message : string}

(** [get_time ()] is the current local 24-hour time. *)
val get_time : unit -> int

(** [combine id msg] is the raw string form of the current time, the user [id],
    and the message [msg]. *)
val combine : string -> string -> string

(** [parse s] is the Parser type of the raw string [s].
    Raises: [Failure] if [s] is not of the form ["time|user|message"]. *)
val parse : string -> t

(** [format p] is the formatted string of the Parser type [p]. 
    Example: [format {time = 903; user = "bob"; message = "hi"}] is
    ["[9:03] bob: hi"]*)
val format : t -> string

(** [p_to_string p] is the raw string form of Parser type [p]. *)
val p_to_string : t -> string