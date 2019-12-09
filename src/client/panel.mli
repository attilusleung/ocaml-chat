(** Modules for creating panels on a terminal screen. Because why use curses
    when you can do it yourself? *)

(** [PanelWidthTooLarge] is raised when attempting to draw a panel with width
    larger than the width of the terminal *)
exception PanelWidthTooLarge
(** [PanelLengthTooLarge] is raised when attempting to draw a panel with length
    larger than the length of the terminal *)
exception PanelHeightTooLarge

(** A base module included in all panels *)
module Panel: sig
  (** [t] is an abstract type of a base panel *)
  type t
  (** [make x y width height] makes a panel at ([x], [y]) with width [width] and
      height [height] *)
  val make: int -> int -> int -> int -> t
  (** [draw_border buffer t] draws the panel on the array buffer [buffer]
      The buffer is a two dimmension array that represents the column and rows
      of the terminal screen. *)
  val draw_border: t -> string array array -> bool -> unit
end

(** An input panel that acts as a text box for user input *)
module InputPanel : sig
  type t
  val make: int -> int -> int -> int -> bool -> (string -> unit) -> t
  val draw: t -> string array array -> bool -> unit
  val update: t -> Key.key -> unit Lwt.t
  val get_cursor: t -> int * int
  val get_input: t -> string
end

(** A message panel that displays messages sent to the client *)
module MessagePanel : sig
  type t
  val make: int -> int -> int -> int ->
    t * (string, Parser.t DoublyLinkedList.t) Hashtbl.t
  val draw: t -> string array array -> bool -> unit
end

module TextPanel : sig
  type t
  val make: int -> int -> Parser.form_message list -> t
  val set_text : t -> Parser.form_message list -> unit
  val draw: t -> string array array -> unit
end

module StatusPanel : sig
  type t
  val make: int -> int -> int -> int -> t * string list ref
  val draw: t -> string array array -> bool -> unit
  val get_cursor : t -> int * int
  val update_active : t -> Key.key -> unit Lwt.t
end
