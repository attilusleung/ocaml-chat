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
    ; c_opost= false (* ; c_brkint=false *)
    }
    (* ; c_inpck=false *)
    (* ; c_vmin= 0 *)
    (* ; c_vtime= 1 } *)
  in
  tcsetattr stdout TCSANOW new_term

let unsetraw () = tcsetattr stdout TCSANOW termios


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
  (* save_cursor (); *)
  set_cursor 1 1 ;
  flush_screen b s ;
  let cursorx, cursory = InputPanel.get_cursor input_panel in
  set_cursor cursorx cursory ;
  return ()

let get_char_stdin () =
  pick
    [Lwt_io.read_char Lwt_io.stdin
    ; Lwt_unix.sleep 0.1 >>= fun _ -> return '\x00' ]

let rec get_escaped seq =
  (* Buffer.add_char seq (input_char Stdlib.stdin); *)
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
    (* | _ -> Buffer.to_seq seq |> Seq.fold_left (fun a b -> String.make 1 b :: a)  [] *)
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
    | _ ->
      return Null
  with End_of_file -> return Null

let input () =
  get_char_stdin () >>= parse_input
    (* output_string log_file "getting input"; *)
    (* let c = input_char Stdlib.stdin in *)
    (* output_string log_file "done input"; *)

(* Null *)

let rec term_update conn () =
  (* erase Screen ; *)
  draw () >>= input
  >>= InputPanel.update input_panel conn
  (* ; output_string log_file "please" *)
      (* >>= fun _ -> *)
      (* msg_log := DoublyLinkedList.insert "hmm" !msg_log; return () *)
  >>= term_update conn

(* let start_msg_update conn () = listen_msg conn msg_log () *)

(*
   let rec loop conn =
     let%lwt () = (erase Screen; return ()) in
     let%lwt () = draw () in
     let%lwt () = update conn in
     loop conn
*)

let start () = let%lwt conn = create_connection () in
  log_out "inited";
  ignore @@ term_update conn () ; listen_msg conn msg_log ()

let () =
  setraw () ;
  erase Screen ;
  set_cursor 1 1 ;
  Stdlib.print_string "connecting to server...";
  flush Stdlib.stdout;
  (* flush_screen (); *)
  (* unsetraw(); *)
  (* Lwt_main.run @@ loop conn *)
  Lwt_main.run @@ start ()
