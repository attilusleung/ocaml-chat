open Array
open ANSITerminal
open Unix
open Panel
open Key
open Network
open Lwt
open Log

let termios = tcgetattr stdin

let input_panel = InputPanel.make 0 25 80 3

let msg_panel, msg_log = MessagePanel.make 0 0 80 20

let setraw () =
  let new_term =
    { termios with
      c_echo= false
    ; c_icanon= false (* ; c_isig=false *)
    ; c_ixon= false
    ; c_icrnl= false
    ; c_opost= false
    ; c_isig= false }
    (* c_iexten *)
    (* ; c_brkint=false *)
    (* ; c_inpck=false *)
    (* ; c_vmin= 0 *)
    (* ; c_vtime= 1 } *)
  in
  tcsetattr stdout TCSANOW new_term

let unsetraw () = return @@ tcsetattr stdout TCSANOW termios

let flush_screen buffer size =
  for i = 0 to snd size - 1 do
    for j = 0 to fst size - 1 do
      Stdlib.print_string buffer.(j).(i)
    done
  done ;
  restore_cursor ()

let rec draw () =
  let s = size () in
  let b = make_matrix (fst s) (snd s) " " in
  InputPanel.draw input_panel b false ;
  MessagePanel.draw msg_panel b false ;
  set_cursor 1 1 ;
  flush_screen b s ;
  let cursorx, cursory = InputPanel.get_cursor input_panel in
  set_cursor cursorx cursory ; return ()

let get_char_stdin () =
  pick
    [ Lwt_io.read_char Lwt_io.stdin
    ; (Lwt_unix.sleep 0.1 >>= fun _ -> return '\x00') ]

let rec get_escaped seq =
  try
    let%lwt c = get_char_stdin () in
    match seq ^ String.make 1 c with
    | "\x1b[A" ->
      return Up
    | "\x1b[B" ->
      return Down
    | "\x1b[C" ->
      return Right
    | "\x1b[D" ->
      return Left
    | "\x1b[3" ->
      get_escaped "\x1b[3"
    | "\x1b[3~" ->
      return Delete
    | _ ->
      return Escape
  with End_of_file -> return Escape

let parse_input c =
  try
    match c with
    | '\x1b' ->
      let%lwt second = get_char_stdin () in
      if second = '[' then get_escaped "\x1b[" else return Escape
    | ' ' .. '~' as c ->
      return @@ Char c
    | '\x7f' ->
      return Backspace
    | '\x08' ->
      return VimLeft
    | '\x0a' ->
      return VimDown
    | '\x0b' ->
      return VimUp
    | '\x0c' ->
      return VimRight
    | '\x0d' ->
      return Enter
    | '\x03' ->
      (* Ctrl- C *)
      erase Screen ; set_cursor 1 1 ; exit 0
    | _ ->
      return Null
  with End_of_file -> return Null

let input () = get_char_stdin () >>= parse_input

let rec term_update conn () =
  draw () >>= input >>= InputPanel.update input_panel conn >>= term_update conn

let rec get_interrupt () =
  Lwt_io.read_char Lwt_io.stdin
  >>= function '\x03' -> exit 0 | _ -> get_interrupt ()

let start () =
  let%lwt conn = pick [create_connection (); get_interrupt ()] in
  log_out "inited" ;
  ignore @@ term_update conn () ;
  listen_msg conn msg_log ()

let () =
  setraw () ;
  erase Screen ;
  set_cursor 1 1 ;
  Stdlib.print_string "connecting to server..." ;
  flush Stdlib.stdout ;
  Lwt_main.at_exit (fun _ -> unsetraw ()) ;
  Lwt_main.run @@ start ()
