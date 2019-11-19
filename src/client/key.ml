type key =
  | Char of char
  | Up
  | Down
  | Left
  | Right
  | Delete
  | Backspace
  | Enter
  | CtrlL
  | CtrlI
  | CtrlS
  | Escape (* of char list *)  (* TODO: Don't eat characters *)
  | Null
