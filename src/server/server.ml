open Lwt
open Lwt_io
open ChatLog
open Printexc
open Parser

exception ClosedConnection

module Server = struct
  type connection =
    { file: Lwt_unix.file_descr
    ; in_channel: Lwt_io.input_channel
    ; out_channel: Lwt_io.output_channel
    ; sockadd: Lwt_unix.sockaddr
    }

end

type connection =
  { file: Lwt_unix.file_descr
  ; in_channel: Lwt_io.input_channel
  ; out_channel: Lwt_io.output_channel
  ; sockadd: Lwt_unix.sockaddr }

(* TODO: Get an actual ip address (currently points to localhost) *)
let listen_address = Unix.inet_addr_loopback

let port = 9000

let backlog = 5

(* https://baturin.org/code/lwt-counter-server/ *)
let rec handle_connection ic oc id () =
  let%lwt line = read_line_opt ic in
  match line with
  | Some msg -> 
    let p = parse msg in
    log_out (p_to_string p);
    Lwt_io.write_line stdout @@ 
    ("received: \"" ^ p.message ^ "\" from " ^ p.user)
    >>= fun () ->
    Lwt_io.write_line oc msg
    >>= handle_connection ic oc id
  | None ->
    Lwt_io.write_line stdout @@ "connection " ^ id ^ " terminated"
    >>= fun () -> fail ClosedConnection

let write_log oc n = 
  let logs = retrieve_chatlog n in
  let rec write lst =
    match lst with 
    | h::t -> Lwt_io.write_line oc h >>= fun () -> write t
    | [] -> return () in
  write logs


let accept_connection active conn =
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
  write_log oc 20
  >>= fun () ->
  Hashtbl.add active id connection_rec ;
  Lwt.on_failure (handle_connection ic oc id ()) (fun e ->
      ( match e with
        | ClosedConnection ->
          print_endline @@ "Connection with " ^ id ^ " closed"
        | e ->
          print_endline @@ "An error occured :" ^ to_string e ) ;
      Hashtbl.remove active id) ;
  let%lwt () = Lwt_io.write_line stdout @@ "new connection from " ^ id in
  return ()

let rec debug_input active () =
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
  >>= debug_input active

let create_server sock =
  let active = Hashtbl.create 5 in
  ignore @@ debug_input active () ;
  let rec serve active () =
    Lwt_unix.accept sock >>= accept_connection active >>= serve active
  in
  serve active

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
  Lwt_main.run @@ serve ()
