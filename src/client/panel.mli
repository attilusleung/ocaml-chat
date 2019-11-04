exception PanelWidthTooLarge
exception PanelHeightTooLarge

module Panel: sig
  type t
  val make: int -> int -> int -> int -> t
  val draw_border: t -> string array array -> bool -> unit
end

module InputPanel : sig
  type t
  val make: int -> int -> int -> int -> t
  val draw: t -> string array array -> bool -> unit
  val update: t -> Network.connection -> Key.key -> unit Lwt.t
  val get_cursor: t-> int * int
end

module MessagePanel : sig
  type t
  val make: int -> int -> int -> int -> t * string DoublyLinkedList.t ref
  val draw: t -> string array array -> bool -> unit
end
