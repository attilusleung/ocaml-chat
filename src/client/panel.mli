(** [Panel] includes modules for creating panels on a terminal screen using an
 * array buffer. Because why use curses when you can do it yourself? *)

(** [PanelWidthTooLarge] is raised when attempting to draw a panel with width
    larger than the width of the terminal *)
exception PanelWidthTooLarge
(** [PanelHeightTooLarge] is raised when attempting to draw a panel with height
    larger than the height of the terminal *)
exception PanelHeightTooLarge

(** A base module included in all panels *)
module Panel: sig
  type t
  (** [t] is an abstract type of a base panel *)
  val make: int -> int -> int -> int -> t
  (** [make x y width height] makes a panel at ([x], [y]) with width [width] and
      height [height] *)
  val draw_border: t -> string array array -> bool -> unit
  (** [draw_border buffer t] draws the panel on the array buffer [buffer]
      The buffer is a two dimmension array that represents the column and rows
      of the terminal screen. *)
end

(** An input panel that acts as a text box for user input *)
module InputPanel : sig
  type t
  (** [t] is the type representing an input panel *)
  val make: int -> int -> int -> int -> bool -> (string -> unit) -> t
  (** [make x y width height hidden callback] creates an InputPanel where its
   *  top left corner is located at ([x], [y]) with width [width] and height
   *  [height].
   * If [hidden] is true, the input is masked by asterisks.
   * It also stores [callback], which is called whenever the user submits the
   * input (by pressing Enter). *)
  val draw: t -> string array array -> bool -> unit
  (** [draw panel buffer bold] draws [panel] to [buffer], bolding its borders if
   * [bold] is true. *)
  val update: t -> Key.key -> unit Lwt.t
  (** [update panel key] updates [panel] based on the input [key] recieved.
   *
   * It captures and records user input, and calls the callback associated with
   * the panel with all the text in the panel as an arguement when the user
   * submits the input. The panel is then cleared of all text. *)
  val get_cursor: t -> int * int
  (** [get_cursor panel] is the cursor position of the input in terms of
   * terminal coordinates. *)
  val get_input: t -> string
  (** [get_input panel] is the string inputted to [panel] *)
end

(** A message panel that displays messages sent to the client *)
module MessagePanel : sig
  type t
  (** [t] is the type representing a MessagePanel *)
  val make: int -> int -> int -> int ->
    t * Client.logs
  (** [make x y width height] is a tuple containing a message panel created with
   * top left corner located at ([x], [y]) with width [width] and height
   * [height], as well as a mutable data structure that stores the logs of all
   * messages shown by the panel. *)
  val draw: t -> string array array -> bool -> unit
  (** [draw panel buffer bold] draws [panel] onto [buffer] with bolded borders
   * if [bold] is true. *)
end

(** A text panel that displays formatted text *)
module TextPanel : sig
  type t
  (** [t] is the type representing a TextPanel *)
  val make: int -> int -> Parser.form_message list -> t
  (** [make x y text] is a TextPanel with text [text] and its first character
   * located at ([x], [y]) *)
  val set_text : t -> Parser.form_message list -> unit
  (** [set_text panel text] changes the text of [panel] to [text] *)
  val draw: t -> string array array -> unit
  (** [draw panel buffer] draws [panel] to [buffer]. *)
end

(** A status panel that displays the online status of users as well as allows
 * selecting users to be set as "active" *)
module StatusPanel : sig
  type t
  (** [t] is the type representing a status panel. *)
  val make: int -> int -> int -> int -> t * string list ref
  (** [make x y width height] is a tuple containing a status panel with its top
   * left corner located at ([x], [y]) and width [width] and height [height], as
   * well as a pointer to a list that represents all the users that are
   * currently online. *)
  val draw: t -> string array array -> bool -> unit
  (** [draw panel buffer bold] draws [panel] onto [buffer] with bolded borders
   *  if [bold] is true. *)
  val get_cursor : t -> int * int
  (** [get_cursor panel] is the location of the cursor selecting a user on the
   * panel in terminal coordinates *)
  val update_active : t -> Key.key -> unit Lwt.t
  (** [update_active panel key] updates [panel] in response to input [key].
   *
   * It determines whether the user has changed the selection of the active
   * user, and changes the current active user acordingly if it is the case. *)
end
