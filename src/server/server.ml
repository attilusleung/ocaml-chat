(** [Server] is a multithreaded dedicated messaging server that allows
 * communication between clients. It supports login and sending messages, as
 * well as a debugging interface. *)

open Lwt
open Lwt_io
open Printexc
open Parser
open Protocol
open ChatLog

(** [ClosedConnection] is the exception thrown when a connection is closed
 * either by the client or the server. *)
exception ClosedConnection

type connection =
  { file: Lwt_unix.file_descr
  ; in_channel: Lwt_io.input_channel
  ; out_channel: Lwt_io.output_channel
  ; sockadd: Lwt_unix.sockaddr }
(** [connection] is a type representing the connection between a server and a
 * client. *)

(** [listen_address] is the ip address which the server listens to. *)
let listen_address = Unix.inet_addr_any

(** [port] is the port the server is hosted on *)
let port = 9000

(** [backlog] is the maximum number of pending requests the a single connection
 * allows *)
let backlog = 5

(** [active] is the map of all connected users.
 *
 * The key of [active] represents the username of each user, and the value is
 * the active connection of that user. *)
let active = Hashtbl.create 5

(** [passwords] is a collection of all passwords of all users.
 *
 * The key represents the username and the value is the password stored as
 * plaintext. *)
let passwords = Hashtbl.create 20

(** [get_passwords ()] populates [passwords] with the passwords stored locally
 * on the server. *)
let get_passwords () =
  let file = open_in_gen [Open_rdonly; Open_creat] 0o640 "passwd.txt" in
  let rec get_user () =
    try
      ( match input_line file |> String.split_on_char '|' with
        | h :: t ->
          Hashtbl.add passwords h @@ (String.concat "|" t |> String.trim)
        | [] ->
          () ) ;
      get_user ()
    with End_of_file -> close_in file
  in
  get_user ()

(** [write_passwords user password] adds a new user with username [user]
    and password [password] to the passwords file. *)
let write_passwords user password =
  let file = open_out_gen [Open_append; Open_creat] 0o640 "passwd.txt" in
  output_string file @@ user ^ "|" ^ password ^ "\n";
  Stdlib.flush file;
  close_out file

(** [broadcast msg] sends [msg] to all users in [active]. *)
let broadcast msg =
  Hashtbl.iter
    (fun user conn ->
       ignore @@ Lwt_io.write_line conn.out_channel msg)
    active

(** [bounce msg parsed] bounces [msg] and its parsed variant [parsed] back to
 * its recipient. *)
let bounce msg parsed =
  return
    ( get_to_user parsed |> Hashtbl.find_all active
      |> List.iter (fun c ->
          ignore @@ Lwt_io.write_line c.out_channel ("M" ^ msg)) )

(** [send_chatlogs oc user] sends all the chatlogs belonging to [user] through
 * the outchannel [oc] *)
let send_chatlogs oc user =
  return
  @@ List.iter
    (fun m -> ignore @@ Lwt_io.write_line oc ("M" ^ m))
    (retrieve_chatlog user)

(* adapted from https://baturin.org/code/lwt-counter-server/ *)

(** [handle_connection ic oc id ()] is a thread that handles all incoming
 * messages from the connection with inchannel [ic], outchannel [oc] and
 * identifier [id].
 *
 * Fails with [ClosedConnection] at end of file. *)
let rec handle_connection ic oc id () =
  let%lwt line = read_line_opt ic in
  match line with
  | Some msg ->
    ( match decode msg with
      | Message m ->
        Lwt_io.write_line stdout @@ 
        get_from_user m ^ " sent \"" ^ get_message m ^ "\" to " ^ get_to_user m
        >>= fun _ ->
        log_out (pack_t m) (get_to_user m) ;
        log_out (pack_t m) (get_from_user m) ;
        bounce msg m
      | _ ->
        return () )
    >>= handle_connection ic oc id
  | None ->
    fail ClosedConnection

(** [login_password u p id oc ic connection_rec] logs the user [u] in with
    password [p]. If password is incorrect, client can try again. If username is
    nonexistent, client can try again. *)
let rec login_password u p id oc ic connection_rec =
  match Hashtbl.find_opt passwords u with
  | None ->
    print_endline @@ id ^ " logged in as nonexistent user " ^ u ;
    ignore @@ Lwt_io.write_line oc encode_fail ;
    login_connection ic oc id connection_rec ()
  | Some pass ->
    if not @@ String.equal pass p
    then begin
      print_endline @@ id ^ " attempted login with invalid password for " ^ u ;
      ignore @@ Lwt_io.write_line oc encode_fail ;
      login_connection ic oc id connection_rec ()
    end
    else
      Lwt_io.write_line oc @@ encode_confirm u
      >>= fun _ ->
      Lwt_io.write_line stdout @@ id ^ " logged in as " ^ u
      >>= fun _ ->
      return @@ broadcast (encode_status [u] [])
      >>= fun _ ->
      Lwt_io.write_line oc
        (encode_status (Hashtbl.fold (fun a b c -> a :: c) active []) [])
      >>= fun _ ->
      return @@ Hashtbl.add active u connection_rec
      >>= fun _ -> send_chatlogs oc u >>= fun _ -> return u

(** [login_register u p id oc ic connection_rec] registers the user [u] with
    their password [p], then logs them in. If username is taken, client can
    try again. *)
and login_register u p id oc ic connection_rec =
  if Hashtbl.mem passwords u
  then begin
    print_endline @@ id ^ " attempted to register as existing user " ^ u ;
    ignore @@ Lwt_io.write_line oc encode_fail ;
    login_connection ic oc id connection_rec ()
  end
  else begin
    write_passwords u p ;
    Hashtbl.replace passwords u p ;
    Lwt_io.write_line oc @@ encode_confirm u
    >>= fun _ ->
    Lwt_io.write_line stdout @@ id ^ " registered as " ^ u
    >>= fun _ ->
    return @@ broadcast (encode_status [u] [])
    >>= fun _ ->
    Lwt_io.write_line oc
      (encode_status (Hashtbl.fold (fun a b c -> a :: c) active []) [])
    >>= fun _ ->
    return @@ Hashtbl.add active u connection_rec
    >>= fun _ -> send_chatlogs oc u >>= fun _ -> return u
  end

(** [login_connection ic oc id connection ()] waits for the login command from
 * the connection with inchannel [ic], outchannel [oc], identifier [id] and
 * connection [connection]. The function loops until either the client logins
 * successfully, the connection closes, or an unexpected message is recieved.
 *
 * Fails with [ClosedConnection] if the connection closes at any point during
 * the thread. *)
and login_connection ic oc id connection_rec () =
  let%lwt line = read_line_opt ic in
  match line with
  | Some msg -> begin
      match decode msg with
      | Login (u, p) -> login_password u p id oc ic connection_rec
      | Register (u, p) -> login_register u p id oc ic connection_rec
      | _ ->
        Lwt_unix.close connection_rec.file
        >>= fun _ ->
        Lwt_io.write_line stdout @@ id ^ " sent invalid login message " ^ msg
        >>= fun _ ->
        Lwt_unix.close connection_rec.file >>= fun _ -> fail ClosedConnection
    end
  | None ->
    fail ClosedConnection

(** [accept_connection conn] is the entrypoint of a new incoming connection
 * [conn]. It adds [conn] to the active connections, then spawns a new thread
 * that attempts to log in the user, and then listens for any other commands. *)
let accept_connection conn =
  let fd, sa = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  let id =
    match sa with
    | Unix.ADDR_INET (a, p) ->
      Unix.string_of_inet_addr a ^ ":" ^ string_of_int p
    | Unix.ADDR_UNIX _ ->
      failwith "unreachable code"
  in
  let connection_rec =
    {file= fd; in_channel= ic; out_channel= oc; sockadd= sa}
  in
  ignore @@ Lwt.try_bind (login_connection ic oc id connection_rec)
    (fun user ->
       Lwt.catch (handle_connection ic oc id)
         (fun e ->
            begin
              match e with
              | ClosedConnection ->
                print_endline @@
                "Connection with " ^ user ^ " (" ^ id ^ ") closed" ;
                Hashtbl.remove active user ;
                broadcast (encode_status [] [user])
              | e ->
                print_endline @@ "An error occured :" ^ to_string e end ;
            return ()))
    (fun e ->
       match e with
       | ClosedConnection ->
         return ()
       | e ->
         return (print_endline @@ "An error occured: " ^ to_string e)) ;
  let%lwt () = Lwt_io.write_line stdout @@ "new connection from " ^ id in
  return ()

(** [debug_input ()] is a thread that allows the server to send messages to
 * clients through the terminal interface. *)
let rec debug_input () =
  let%lwt input = Lwt_io.read_line_opt Lwt_io.stdin in
  ( match input with
    | Some s -> (
        match String.split_on_char ' ' s with
        | h :: t -> (
            match Hashtbl.find_opt active h with
            | Some {out_channel= oc} ->
              Lwt_io.write_line oc @@ String.concat " " t
            | None ->
              Lwt_io.write_line Lwt_io.stdout @@ "No active connection " ^ h )
        | _ ->
          Lwt_io.write_line Lwt_io.stdout "Malformed function" )
    | None ->
      return () )
  >>= debug_input

(** [create_server sock] is the main thread of the server that accepts incoming
 * connections and handles them. *)
let create_server sock =
  ignore @@ debug_input () ;
  clear_chatlogs () ;
  let rec serve () = Lwt_unix.accept sock >>= accept_connection >>= serve in
  serve

(** [create_socket ()] is the socket used by the server to listen to incoming
 * connections. *)
let create_socket () =
  Lwt_unix.(
    let sock = socket PF_INET SOCK_STREAM 0 in
    ignore @@ bind sock @@ ADDR_INET (listen_address, port) ;
    listen sock backlog ;
    sock)

let () =
  let sock = create_socket () in
  let serve = create_server sock in
  print_endline @@ "Server started at "
                   ^ Unix.string_of_inet_addr listen_address
                   ^ ":" ^ string_of_int port ;
  get_passwords () ;
  Lwt_main.at_exit
    (fun _ -> return @@
      Hashtbl.iter (fun _ conn -> ignore @@ Lwt_unix.close conn.file) active) ;
  Lwt_main.run
  @@ (serve () >>= fun _ -> Lwt_io.write_line Lwt_io.stdout "ended")
