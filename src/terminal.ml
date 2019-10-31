open Array
open ANSITerminal
open Unix
open Panel
open Key

let termios = tcgetattr stdin

let input_panel = InputPanel.make 0 0 80 3

let setraw () =
  let new_term = { termios with c_echo=false
                              ; c_icanon=false
                              (* ; c_isig=false *)
                              ; c_ixon=false
                              ; c_icrnl=false
                              ; c_opost=false
                              (* ; c_brkint=false *)
                              (* ; c_inpck=false *)
                              ; c_vmin=0
                              ; c_vtime=1
                 }
  in
  tcsetattr stdout TCSANOW new_term

let unsetraw () =
  tcsetattr stdout TCSANOW termios

let log_file = open_out "log.txt"


let flush_screen buffer size =
  begin
  for i = 0 to (snd size)-1 do
    for j = 0 to (fst size)-1 do
      Stdlib.print_string buffer.(j).(i)
    done;
  done;
  end;
  restore_cursor ()

let draw () =
  let s = size () in
  let b = make_matrix (fst s) (snd s) " " in
  begin
    InputPanel.draw input_panel b false;
    (* save_cursor (); *)
    set_cursor 1 1;
    flush_screen b s;
    let cursorx, cursory  = InputPanel.get_cursor input_panel in
    set_cursor cursorx cursory
  end

let rec get_escaped seq =
  (* Buffer.add_char seq (input_char Stdlib.stdin); *)
  try
    match seq ^ String.make 1 (input_char Stdlib.stdin) with
    | "\x1b[A" -> Up
    | "\x1b[B" -> Down
    | "\x1b[C" -> Right
    | "\x1b[D" -> Left
    | "\x1b[3" -> get_escaped "\x1b[3"
    | "\x1b[3~" -> Delete
    (* | _ -> Buffer.to_seq seq |> Seq.fold_left (fun a b -> String.make 1 b :: a)  [] *)
    | _ -> Escape
  with End_of_file -> Escape

let input () =
  try
    (* output_string log_file "getting input"; *)
    let c = input_char Stdlib.stdin in
    begin
      (* output_string log_file "done input"; *)
    match c with
    | '\x1b' -> let second = (input_char Stdlib.stdin) in
      if second = '[' then get_escaped "\x1b["
      else Escape
    | ' '..'~' as c -> Char c
    | '\x7f' -> Backspace
    | '\x08' -> VimLeft
    | '\x0a' -> VimDown
    | '\x0b' -> VimUp
    | '\x0c' -> VimRight
    | _ -> Null
    end
  with End_of_file -> Null
    (* Null *)

let update () =
  let i = input () in
  match i with
  | Null -> ()  (* output_string log_file "empty" *)
  | c ->
    InputPanel.update input_panel c (* ; output_string log_file "please" *)

let rec loop () =
  erase Screen;
  draw ();
  update ();
  loop ()

let () =
  setraw ();
  erase Screen;
  set_cursor 1 1;
  (* flush_screen (); *)
  (* unsetraw(); *)
  loop ()
