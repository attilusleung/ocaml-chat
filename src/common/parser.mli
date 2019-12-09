(** The type of a formatted message. *)
type form_message

(** The type of a parsed message sent by a client. *)
type t

(** [pack to_user time from_user message] is the packed plaintext sent to the
    server. 
    Example: [pack "bob" 1575670849. "alice" "hello"] is packed as 
    ["bob|1575670849.|alice|hello"]. *)
val pack : string -> float -> string -> string -> string

(** [pack_t t] is the packed plaintext from parsed message [t]. *)
val pack_t : t -> string

(** [make to_user time from_user org_msg] is the parsed message which also 
    contains the formatted message based on any formatting instructions 
    contained in [org_msg]. *)
val make : string -> float -> string -> string -> t

(** [parse s] is the parsed message extracted from the packed plaintext [s]. *)
val parse : string -> t

(** [get_from_user t] is the name of the user that sent the parsed message 
    [t]. *)
val get_from_user : t -> string

(** [get_to_user t] is the name of the user that the parsed message [t] is being
    sent to. *)
val get_to_user : t -> string

(** [get_message t] is the message of the parsed message [t] that is being 
    sent. *)
val get_message : t -> string

(** [format t] is the format of the parsed message [t] that appears in the 
    client's terminal.
    Example: [[12:15] bob: hello] *)
val format : t -> string

(** [make_message msgs] is the unformatted message contained in the list of 
    formatted messages [msgs]. *)
val make_message : form_message list -> string

(** [output_list t] is the list of characters in the parsed message [t] that
    may contain formatting. *)
val output_list : t -> string list

(** [message_to_string message] is the formatted list of characters contained 
    in the list of formatted strings [message]. *)
val message_to_string : form_message list -> string list

(** [make_formatted fmt text] is the formatted message with [fmt] formatting and
    the message [text]. *)
val make_formatted : string -> string -> form_message
