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
  | Escape (* of char list *)  (* TODO: Don't eat characters *)
  | Null
