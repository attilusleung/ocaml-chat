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
  (** [t] is the type representing an input panel *)
  type t
  (** [make x y width height hidden callback] creates an InputPanel where its 
   *  top left corner is located at ([x], [y]) with width [width] and height 
   *  [height].
   * If [hidden] is true, the input is masked by asterisks.
   * It also stores [callback], which is called whenever the user submits the
   * input (by pressing Enter). *)
  val make: int -> int -> int -> int -> bool -> (string -> unit) -> t
  (** [draw panel buffer bold] draws [panel] to [buffer], bolding its borders if
   * [bold] is true. *)
  val draw: t -> string array array -> bool -> unit
  (** [update panel key] updates [panel] based on the input [key] recieved.
   *
   * It captures and records user input, and calls the callback associated with
   * the panel with all the text in the panel as an arguement when the user
   * submits the input. The panel is then cleared of all text. *)
  val update: t -> Key.key -> unit Lwt.t
  (** [get_cursor panel] is the cursor position of the input in terms of 
   * terminal coordinates. *)
  val get_cursor: t -> int * int
  (** [get_input panel] is the string inputted to [panel] *)
  val get_input: t -> string
end

(** A message panel that displays messages sent to the client *)
module MessagePanel : sig
  (** [t] is the type representing a MessagePanel *)
  type t
  (** [make x y width height] is a tuple containing a message panel created with
   * top left corner located at ([x], [y]) with width [width] and height 
   * [height], as well as a mutable data structure that stores the logs of all 
   * messages shown by the panel. *)
  val make: int -> int -> int -> int ->
    t * Client.logs
  (** [draw panel buffer bold] draws [panel] onto [buffer] with bolded borders
   * if [bold] is true. *)
  val draw: t -> string array array -> bool -> unit
end

(** A text panel that displays formatted text *)
module TextPanel : sig
  (** [t] is the type representing a TextPanel *)
  type t
  (** [make x y text] is a TextPanel with text [text] and its first character
   * located at ([x], [y]) *)
  val make: int -> int -> Parser.form_message list -> t
  (** [set_text panel text] changes the text of [panel] to [text] *)
  val set_text : t -> Parser.form_message list -> unit
  (** [draw panel buffer] draws [panel] to [buffer]. *)
  val draw: t -> string array array -> unit
end

(** A status panel that displays the online status of users as well as allows
 * selecting users to be set as "active" *)
module StatusPanel : sig
  (** [t] is the type representing a status panel. *)
  type t
  (** [make x y width height] is a tuple containing a status panel with its top
   * left corner located at ([x], [y]) and width [width] and height [height], as
   * well as a pointer to a list that represents all the users that are 
   * currently online. *)
  val make: int -> int -> int -> int -> t * string list ref
  (** [draw panel buffer bold] draws [panel] onto [buffer] with bolded borders 
   *  if [bold] is true. *)
  val draw: t -> string array array -> bool -> unit
  (** [get_cursor panel] is the location of the cursor selecting a user on the
   * panel in terminal coordinates *)
  val get_cursor : t -> int * int
  (** [update_active panel key] updates [panel] in response to input [key].
   *
   * It determines whether the user has changed the selection of the active
   * user, and changes the current active user acordingly if it is the case. *)
  val update_active : t -> Key.key -> unit Lwt.t
end
