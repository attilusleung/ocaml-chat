
module Panel: sig
  type t
  val make: int -> int -> int -> int -> t
  val draw_border: t -> string array array -> bool -> unit
end

module InputPanel : sig
  type t
  val make: int -> int -> int -> int -> t
  val draw: t -> string array array -> bool -> unit
  val update: t -> Key.key -> unit
  val get_cursor: t-> int * int
end
