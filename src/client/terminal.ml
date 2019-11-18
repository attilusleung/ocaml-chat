open Array
open ANSITerminal
open Unix
open Panel
open Key
open Network
open Lwt
open Log
open Parser
open Client
open Protocol

let termios = tcgetattr stdin

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

type login_panel_rec =
  {prompt_text: TextPanel.t; warn_text: TextPanel.t; name_input: InputPanel.t}

type msg_panel_rec = {msg_show: MessagePanel.t; msg_input: InputPanel.t}

module MessageState = struct
  let rec draw panels =
    let s = size () in
    let b = make_matrix (fst s) (snd s) " " in
    InputPanel.draw panels.msg_input b false ;
    MessagePanel.draw panels.msg_show b false ;
    set_cursor 1 1 ;
    flush_screen b s ;
    let cursorx, cursory = InputPanel.get_cursor panels.msg_input in
    set_cursor cursorx cursory ; return ()

  let rec term_update conn panels () =
    draw panels >>= input
    >>= InputPanel.update panels.msg_input
    >>= term_update conn panels

  let init conn () =
    log_out "attempt msg init" ;
    let input_callback msg =
      if msg <> "" then
        ignore
          ( encode_msg (get_selected ()) (time ()) (get_user ()) msg
            |> send_msg conn )
    in
    log_out "created callback" ;
    let msg_input = InputPanel.make 0 25 80 3 input_callback in
    let msg_show, msg_log = MessagePanel.make 0 0 80 20 in
    (* TODO *)
    let panels = {msg_input; msg_show} in
    (* TODO: wait and cleanup? *)
    ignore @@ term_update conn panels () ;
    listen_msg conn msg_log ()
end

module LoginState = struct
  let rec draw panels =
    let s = size () in
    let b = make_matrix (fst s) (snd s) " " in
    TextPanel.draw panels.prompt_text b ;
    TextPanel.draw panels.warn_text b ;
    InputPanel.draw panels.name_input b false ;
    set_cursor 1 1 ;
    flush_screen b s ;
    let cursorx, cursory = InputPanel.get_cursor panels.name_input in
    set_cursor cursorx cursory ; return ()

  let rec term_update panels () =
    draw panels >>= input
    >>= InputPanel.update panels.name_input
    >>= term_update panels

  let init conn () =
    log_out "attempt log init" ;
    let promise, resolver = Lwt.wait () in
    let prompt_text =
      TextPanel.make 0 1
        [ "C"
        ; "h"
        ; "o"
        ; "o"
        ; "s"
        ; "e"
        ; " "
        ; "a"
        ; " "
        ; "u"
        ; "s"
        ; "e"
        ; "r"
        ; "n"
        ; "a"
        ; "m"
        ; "e"
        ; ":" ]
    in
    let warn_text = TextPanel.make 0 2 [] in
    let input_callback name =
      if String.contains name '|' then
        TextPanel.set_text warn_text (* TODO: Please make this less jank *)
          [ "\u{001b}[31mT"
          ; "h"
          ; "e"
          ; " "
          ; "\'"
          ; "|"
          ; "\'"
          ; " "
          ; "c"
          ; "h"
          ; "a"
          ; "r"
          ; "a"
          ; "c"
          ; "t"
          ; "e"
          ; "r"
          ; " "
          ; "c"
          ; "a"
          ; "n"
          ; "n"
          ; "o"
          ; "t"
          ; " "
          ; "b"
          ; "e"
          ; " "
          ; "u"
          ; "s"
          ; "e"
          ; "d"
          ; " "
          ; "i"
          ; "n"
          ; " "
          ; "t"
          ; "h"
          ; "e"
          ; " "
          ; "n"
          ; "a"
          ; "m"
          ; "e\u{001b}[0m" ]
      else if name = "" then
        TextPanel.set_text warn_text
          [ "\u{001b}[31mN"
          ; "a"
          ; "m"
          ; "e"
          ; " "
          ; "c"
          ; "a"
          ; "n"
          ; "n"
          ; "o"
          ; "t"
          ; " "
          ; "b"
          ; "e"
          ; " "
          ; "e"
          ; "m"
          ; "p"
          ; "t"
          ; "y\u{001b}[0m" ]
      else (login_user name ; wakeup_later resolver ())
    in
    log_out "created callback" ;
    let name_input = InputPanel.make 1 3 80 3 input_callback in
    let panels = {prompt_text; warn_text; name_input} in
    pick [term_update panels (); promise]
    >>= fun _ -> send_msg conn (encode_login (get_user ())) (* TODO: verify
                                                               login status *)
end

let rec get_interrupt () =
  Lwt_io.read_char Lwt_io.stdin
  >>= function '\x03' -> exit 0 | _ -> get_interrupt ()

let start () =
  let%lwt conn = pick [create_connection (); get_interrupt ()] in
  log_out "connected" ;
  LoginState.init conn () >>= MessageState.init conn

(* >>= fun _ -> *)
(* log_out "done" ; *)

let () =
  setraw () ;
  erase Screen ;
  set_cursor 1 1 ;
  Stdlib.print_string "connecting to server..." ;
  flush Stdlib.stdout ;
  Lwt_main.at_exit (fun _ -> unsetraw ()) ;
  Lwt_main.run @@ start ()
