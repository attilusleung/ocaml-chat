open Lwt
open Lwt_io
open Printexc
open Parser
open Protocol
open ChatLog

exception ClosedConnection

type connection =
  { file: Lwt_unix.file_descr
  ; in_channel: Lwt_io.input_channel
  ; out_channel: Lwt_io.output_channel
  ; sockadd: Lwt_unix.sockaddr }

(* TODO: Get an actual ip address (currently points to localhost) *)
(* let listen_address = Unix.inet_addr_loopback *)
let listen_address = Unix.inet_addr_any

let port = 9000

let backlog = 5

let active = Hashtbl.create 5

let passwords = Hashtbl.create 20

let get_passwords () =
  let file = open_in_gen [Open_rdonly] 0o640 "passwd.txt" in
  let rec get_user () =
    try
      ( match input_line file |> String.split_on_char '|' with
        | h :: t ->
          Hcashtbl.add passwords h @@ (String.concat "|" t |> String.trim)
        | [] ->
          () ) ;
      get_user ()
    with End_of_file -> ()
  in
  get_user ()

let broadcast msg =
  Hashtbl.iter
    (fun user conn ->
       ignore @@ Lwt_io.write_line conn.out_channel msg)
    active

let bounce msg parsed =
  return
    ( get_to_user parsed |> Hashtbl.find_all active
      |> List.iter (fun c ->
          ignore @@ Lwt_io.write_line c.out_channel ("M" ^ msg)) )

let send_chatlogs oc user =
  return
  @@ List.iter
    (fun m -> ignore @@ Lwt_io.write_line oc ("M" ^ m))
    (retrieve_chatlog user)

(* https://baturin.org/code/lwt-counter-server/ *)
let rec handle_connection ic oc id () =
  let%lwt line = read_line_opt ic in
  match line with
  | Some msg ->
    ( match decode msg with
      | Message m ->
        Lwt_io.write_line stdout @@ get_from_user m ^ " sent \""
                                    ^ get_message m ^ "\" to " ^ get_to_user m
        >>= fun _ ->
        log_out (pack_t m) (get_to_user m) ;
        log_out (pack_t m) (get_from_user m) ;
        bounce msg m
      | _ ->
        return () )
    >>= handle_connection ic oc id
  | None ->
    fail ClosedConnection

let rec login_connection ic oc id connection_rec () =
  let%lwt line = read_line_opt ic in
  match line with
  | Some msg -> (
      match decode msg with
      | Login (u, p) -> (
          (* TODO: Currently accepts all passwords *)
          match Hashtbl.find_opt passwords u with
          | None ->
            print_endline @@ id ^ " logged in as nonexistent user " ^ u ;
            ignore @@ Lwt_io.write_line oc encode_fail ;
            login_connection ic oc id connection_rec ()
          | Some pass ->
            if not @@ String.equal pass p then (
              print_endline @@ id ^ " attempted login with invalid password for "
                               ^ u ;
              ignore @@ Lwt_io.write_line oc encode_fail ;
              login_connection ic oc id connection_rec () )
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
              >>= fun _ -> send_chatlogs oc u >>= fun _ -> return u )
      (* TODO: Move this *)
      | _ ->
        Lwt_unix.close connection_rec.file
        >>= fun _ ->
        Lwt_io.write_line stdout @@ id ^ " sent invalid login message " ^ msg
        >>= fun _ -> fail ClosedConnection )
  | None ->
    fail ClosedConnection

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
  (* Hashtbl.add active id connection_rec ; *)
  ignore
  @@ Lwt.try_bind
    (login_connection ic oc id connection_rec)
    (fun user ->
       Lwt.catch (handle_connection ic oc id) (fun e ->
           ( match e with
             | ClosedConnection ->
               print_endline @@ "Connection with " ^ user ^ " (" ^ id
                                ^ ") closed" ;
               Hashtbl.remove active user ;
               broadcast (encode_status [] [user])
             | e ->
               print_endline @@ "An error occured :" ^ to_string e ) ;
           return ()))
    (fun e ->
       match e with
       | ClosedConnection ->
         return ()
       | e ->
         return (print_endline @@ "An error occured: " ^ to_string e)) ;
  let%lwt () = Lwt_io.write_line stdout @@ "new connection from " ^ id in
  return ()

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

let create_server sock =
  ignore @@ debug_input () ;
  let rec serve () = Lwt_unix.accept sock >>= accept_connection >>= serve in
  serve

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
  Lwt_main.at_exit (fun _ ->
      return
      @@ Hashtbl.iter (fun _ conn -> ignore @@ Lwt_unix.close conn.file) active) ;
  Lwt_main.run
  @@ (serve () >>= fun _ -> Lwt_io.write_line Lwt_io.stdout "ended")
