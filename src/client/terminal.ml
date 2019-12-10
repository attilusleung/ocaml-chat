(** [Terminal] is the entrypoint for the Messaging Client. It handles most
 * terminal operations, including flushing to the screen, setting terminal
 * attributes, handling input, etc. *)

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

(** [termios] is the termios attribute of the terminal before running the
 * client.
 *
 * Used to reset terminal attributes after the client is closed. *)
let termios = tcgetattr stdin

(** [setraw ()] enables a watered-down version of "raw mode" for the terminal.
 * *)
let setraw () =
  let new_term =
    { termios with
      c_echo= false
    ; c_icanon= false (* ; c_isig=false *)
    ; c_ixon= false
    ; c_icrnl= false
    ; c_opost= false
    ; c_isig= false }
  in
  tcsetattr stdout TCSANOW new_term

(** [unsetraw ()] disables the watered-down "raw mode" and resets all terminal
 * attributes. *)
let unsetraw () = return @@ tcsetattr stdout TCSANOW termios

(** [flush_screen buffer size] flushes [buffer] to the terminal with dimensions
 * [size] *)
let flush_screen buffer size =
  for i = 0 to snd size - 1 do
    for j = 0 to fst size - 1 do
      Stdlib.print_string buffer.(j).(i)
    done
  done ;
  restore_cursor ()

(** [get_char_stdin ()] retrieves a character inputted to standard input from
 * the last 0.1 seconds. *)
let get_char_stdin () =
  pick
    [ Lwt_io.read_char Lwt_io.stdin
    ; (Lwt_unix.sleep 0.1 >>= fun _ -> return '\x00') ]

(** [get_escaped seq] attempts to parse an ansi escape sequence to the terminal
 * character by character using the accumulator [seq]. *)
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

(** [parse_input c] is the key recieved from input [c] *)
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
    | '\x0d' ->
      return Enter
    | '\x03' | '\x11' ->
      (* Ctrl- C or Ctrl-Q*)
      erase Screen ;
      set_cursor 1 1 ;
      output_string Stdlib.stdout "\x1b[?25h" ;
      (* Shows cursor *)
      exit 0
    | '\x0c' ->
      return CtrlL
    | '\x09' ->
      return CtrlI
    | '\x13' ->
      return CtrlS
    | _ ->
      return Null
  with End_of_file -> return Null

(** [input ()] is the key recieved from standard input. *)
let input () = get_char_stdin () >>= parse_input

(** [MessageState] represents the main state of the client that sends messages
 * to and from other clients through the server. *)
module MessageState = struct
  type msg_panel_rec =
    { msg_show: MessagePanel.t
    ; msg_input: InputPanel.t
    ; msg_status: StatusPanel.t
    ; login_text: TextPanel.t
    ; control_text: TextPanel.t
    ; quit_text: TextPanel.t }
  (** [msg_panel_rec] includes all the panels that are shown in this state. *)

  (** [panel_switch] is the type of the current active panel. *)
  type panel_switch = MsgShow | MsgInput | MsgStatus

  (** [active] is a pointer to the current active panel. *)
  let active = ref MsgInput

  (** [draw panels] draws [panels] onto the terminal. *)
  let rec draw panels =
    let s = size () in
    let b = make_matrix (fst s) (snd s) " " in
    InputPanel.draw panels.msg_input b (!active = MsgInput) ;
    MessagePanel.draw panels.msg_show b (!active = MsgShow) ;
    StatusPanel.draw panels.msg_status b (!active = MsgStatus) ;
    TextPanel.draw panels.login_text b ;
    TextPanel.draw panels.control_text b ;
    TextPanel.draw panels.quit_text b ;
    set_cursor 1 1 ;
    flush_screen b s ;
    ( match !active with
      | MsgInput ->
        output_string Stdlib.stdout "\x1b[?25h" (* Shows cursor *) ;
        let cursorx, cursory = InputPanel.get_cursor panels.msg_input in
        set_cursor cursorx cursory
      | MsgShow ->
        output_string Stdlib.stdout "\x1b[?25l" (* Hides cursor *)
      | MsgStatus ->
        let cursorx, cursory = StatusPanel.get_cursor panels.msg_status in
        set_cursor cursorx cursory ) ;
    return ()

  (** [switch_active key] switches the active panel based on the input [key]. *)
  let switch_active key =
    match key with
    | CtrlI ->
      active := MsgInput ;
      return Null
    | CtrlL ->
      active := MsgShow ;
      return Null
    | CtrlS ->
      active := MsgStatus ;
      return Null
    | k ->
      return k

  (** [update_active panels key] updates the current active pannel in [panels]
   * with intput [key]. *)
  let update_active panels key =
    match !active with
    | MsgShow ->
      return ()
    | MsgInput ->
      InputPanel.update panels.msg_input key
    | MsgStatus ->
      StatusPanel.update_active panels.msg_status key

  (** [term_update panels ()] is the main update loop that updates [panels]
   * repeatedly. *)
  let rec term_update panels () =
    draw panels >>= input >>= switch_active
    (* >>= InputPanel.update panels.msg_input *)
    >>= update_active panels
    >>= term_update panels

  (** [init conn ()] initializes the MessageState with connection [conn] *)
  let init conn () =
    log_out "attempt msg init" ;
    let msg_show, msg_log = MessagePanel.make 30 2 80 20 in
    let input_callback msg =
      if msg <> "" then (
        let parsed =
          Parser.pack (get_selected ()) (time ()) (get_user ()) msg
        in
        let prev_logs =
          match Hashtbl.find_opt msg_log (get_selected ()) with
          | Some l ->
            l
          | None ->
            DoublyLinkedList.empty
        in
        Hashtbl.replace msg_log (get_selected ())
          (DoublyLinkedList.insert (Parser.parse parsed) prev_logs) ;
        ignore (encode_parsed_msg (Parser.parse parsed) |> send_msg conn) )
    in
    log_out "created callback" ;
    let msg_input = InputPanel.make 30 22 80 3 false input_callback in
    let msg_status, msg_user = StatusPanel.make 2 2 25 23 in
    let login_text =
      TextPanel.make 2 1
        [ make_formatted "\027[4m\027[1m" "OcamlChat"
        ; make_formatted "" " | "
        ; make_formatted "\027[32m" ("Online: " ^ get_user ()) ]
    in
    let control_text =
      TextPanel.make 1 25
        [ make_formatted "\027[4m\027[7m" "Ctrl-I / Tab"
        ; make_formatted "" " : Switch to Input Panel    "
        ; make_formatted "\027[4m\027[7m" "Ctrl-S"
        ; make_formatted "" " : Switch to Status/Online Panel    "
        ; make_formatted "\027[4m\027[7m" "Ctrl-L"
        ; make_formatted "" " : Switch to Message Panel    " ]
    in
    let quit_text =
      TextPanel.make 1 27
        [ make_formatted "" "Exit with "
        ; make_formatted "\027[4m\027[7m" "Ctrl-Q"
        ; make_formatted "" " or "
        ; make_formatted "\027[4m\027[7m" "Ctrl-C" ]
    in
    let panels =
      {msg_input; msg_show; msg_status; login_text; control_text; quit_text}
    in
    ignore @@ term_update panels () ;
    listen_msg conn msg_log msg_user ()
end

(** [LoginState] is the state where the user is logging onto the server. *)
module LoginState = struct
  type login_panel_rec =
    { prompt_text: TextPanel.t
    ; register_text: TextPanel.t
    ; controls_text: TextPanel.t
    ; name_text: TextPanel.t
    ; pass_text: TextPanel.t
    ; warn_text: TextPanel.t
    ; name_input: InputPanel.t
    ; pass_input: InputPanel.t }
  (** [login_panel] is the type representing all panels that are drawn in this
   * state. *)

  (** [action] is the type that represents an action after recieving user input *)
  type action = Login of (string * string) | Register

  exception InvalidInput of string
  (** [InvalidInput] is thrown when there is an attempt to handle input from
   * InputPanels that are empty. *)

  (** [max_name_length] is the maximum number of characters allowed in a
   * username *)
  let max_name_length = 15

  (** [active_name] is whether or not the username input panel is the current
   * active panel. *)
  let active_name = ref true

  (** [draw panels] draws [panels] onto the terminal. *)
  let rec draw panels =
    let s = size () in
    let b = make_matrix (fst s) (snd s) " " in
    TextPanel.draw panels.prompt_text b ;
    TextPanel.draw panels.register_text b ;
    TextPanel.draw panels.controls_text b ;
    TextPanel.draw panels.warn_text b ;
    TextPanel.draw panels.name_text b ;
    TextPanel.draw panels.pass_text b ;
    InputPanel.draw panels.name_input b false ;
    InputPanel.draw panels.pass_input b false ;
    set_cursor 1 1 ;
    flush_screen b s ;
    let cursorx, cursory =
      (if !active_name then panels.name_input else panels.pass_input)
      |> InputPanel.get_cursor
    in
    set_cursor cursorx cursory ; return ()

  (** [switch_active key] toggles the active panel of the state based on input
   * [key]. *)
  let switch_active key =
    match key with
    | CtrlI ->
      active_name := not !active_name ;
      return Null
    | _ ->
      return key

  (** [term_update panels funcs ()] is the update loop that updates [panels]
   * repeatedly while sending key inputs to [funcs] . *)
  let rec term_update panels funcs () =
    draw panels >>= input >>= switch_active >>= funcs
    >>= (fun k ->
        if !active_name then InputPanel.update panels.name_input k
        else InputPanel.update panels.pass_input k)
    >>= term_update panels funcs

  (** [update conn panels promise resolver] is the main update loop that updates
   * panels repeatedly until the client submits their credentials, which then the
   * thread attempts to login to the server, repeating if it fails but
   * terminating if it succes. *)
  let rec update conn panels promise resolver =
    log_out "new update cycle" ;
    let reg_promise, reg_resolv = wait () in
    let get_ctrl_s key =
      match key with
      | CtrlS ->
        wakeup_later reg_resolv Register ;
        return Null
      | _ ->
        return key
    in
    let%lwt act =
      pick [term_update panels get_ctrl_s (); !promise; reg_promise]
    in
    catch
      (fun _ ->
         ( match act with
           | Login (name, pass) ->
             send_msg conn (encode_login name pass)
           | Register ->
             let name = InputPanel.get_input panels.name_input in
             let pass = InputPanel.get_input panels.pass_input in
             if name = "" || pass = "" then
               raise @@ InvalidInput "Username/Password cannot be empty"
             else if String.length name > max_name_length then
               raise
               @@ InvalidInput
                 ( "Username cannot be more than "
                   ^ string_of_int max_name_length
                   ^ " characters long" )
             else send_msg conn (encode_register name pass) )
         >>= (fun _ ->
             get_msg conn
             >>= fun i ->
             log_out i ;
             return i >>= fun i -> return (login_user i))
         >>= fun b ->
         if b then return ()
         else
           let p, r = Lwt.wait () in
           promise := p ;
           resolver := r ;
           TextPanel.set_text panels.warn_text
           @@ [ make_formatted "\027[31m"
                  (if act <> Register then "Invalid login" else "Username taken")
              ] ;
           update conn panels promise resolver)
      (function
        | InvalidInput e ->
          let p, r = Lwt.wait () in
          promise := p ;
          resolver := r ;
          TextPanel.set_text panels.warn_text @@ [make_formatted "\027[31m" e] ;
          update conn panels promise resolver
        | e ->
          raise e)

  (* >>= fun _ -> log_out "nani"; return () *)

  (** [init conn ()] initializes the LoginState with connection [conn] *)
  let init conn () =
    log_out "attempt log init" ;
    let promise, resolver = Lwt.wait () in
    let promise = ref promise in
    let resolver = ref resolver in
    let panel_ref = ref None in
    let prompt_text =
      TextPanel.make 29 1
        [ make_formatted "\027[4m\027[1m"
            "Welcome to Ocaml-Chat! Please log in below:" ]
    in
    let register_text =
      TextPanel.make 29 3
        [make_formatted "" "Press Enter to Login or press Ctrl-S to Register"]
    in
    let controls_text =
      TextPanel.make 29 4
        [make_formatted "" "Use Ctrl-I or Tab to switch inputs. "]
    in
    let name_text = TextPanel.make 5 8 [make_formatted "" "Username: "] in
    let pass_text = TextPanel.make 5 11 [make_formatted "" "Password: "] in
    let warn_text = TextPanel.make 5 6 [] in
    let input_callback _ =
      let name = InputPanel.get_input (Option.get !panel_ref).name_input in
      let pass = InputPanel.get_input (Option.get !panel_ref).pass_input in
      if String.contains name '|' then
        TextPanel.set_text warn_text
          [ make_formatted "\027[31m"
              "The \'|\' character cannot be used in the name" ]
      else if name = "" then
        TextPanel.set_text warn_text
          [make_formatted "\027[31m" "Name cannot be empty"]
      else wakeup_later !resolver @@ Login (name, pass)
      (* TODO: verify login status *)
    in
    log_out "created callback" ;
    let name_input = InputPanel.make 20 7 80 3 false input_callback in
    let pass_input = InputPanel.make 20 10 80 3 true input_callback in
    let panels =
      { prompt_text
      ; register_text
      ; controls_text
      ; name_text
      ; pass_text
      ; warn_text
      ; name_input
      ; pass_input }
    in
    panel_ref := Some panels ;
    log_out "update now" ;
    update conn panels promise resolver
end

(** [get_interrupt ()] listesns only for the interupt input and exits the
 * program if it detects it.
 *
 * It actually doesn't detect SIGINT, but the CTRL-C key. Sorry. You should
 * probably be using a modern terminal anyways. *)
let rec get_interrupt () =
  Lwt_io.read_char Lwt_io.stdin
  >>= function '\x03' -> exit 0 | _ -> get_interrupt ()

(** [parse_args ()] is the arguments sent to the program. It is used to
 * determine which server to connect to.
 *
 * The function prints a help message if a help option is passed and terminates.
 * It also terminates if an unrecognized option is passed to the program. *)
let parse_args () =
  let arg_length = Array.length Sys.argv in
  if arg_length = 1 then Alias "default"
  else
    match Sys.argv.(1) with
    | "-i" ->
      if arg_length = 2 then (
        print_endline "missing ip address parameter for -i" ;
        exit 1 )
      else Address Sys.argv.(2)
    | "-a" ->
      if arg_length = 2 then (
        print_endline "missing alias parameter for -a" ;
        exit 1 )
      else Alias Sys.argv.(2)
    | "--help" | "-h" ->
      print_endline
        "OcamlChat: A minimalistic chat client written in Ocaml.\n\
         Use: terminal [-i address]\n\
        \ or: terminal [-a alias]\n\n\
         Options:\n\
         -i address  connect to server hosted at [address]\n\
         -a alias    connect to the server with the alias [alias]\n\n\
         Possible aliases:\n\
         local       the server hosted locally at localhost\n\
         remote      the official hosted server on a public network\n" ;
      exit 0
    | s ->
      print_endline @@ "unrecognized option " ^ s ;
      exit 1

(** [start args] starts the client with arguement [args]. *)
let start args =
  let%lwt conn = pick [create_connection args; get_interrupt ()] in
  log_out "connected" ;
  setraw () ;
  LoginState.init conn () >>= MessageState.init conn

let () =
  let args = parse_args () in
  erase Screen ;
  set_cursor 1 1 ;
  Stdlib.print_string "connecting to server..." ;
  flush Stdlib.stdout ;
  Lwt_main.at_exit (fun _ -> unsetraw ()) ;
  Lwt_main.run @@ start args
