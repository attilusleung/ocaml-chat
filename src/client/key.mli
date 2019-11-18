(** A module for the shared data type key.
    Separated to prevent recursive includes *)

(** A type for key presses on a terminal *)
type key =
  | Char of char
  | Up
  | Down
  | Left
  | Right
  | Delete
  | Backspace
  | VimUp
  | VimDown
  | VimLeft
  | VimRight
  | Enter
  | Escape
  | Null
