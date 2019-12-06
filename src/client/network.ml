open Lwt
open Lwt_io
open Log
open Parser
open Unix
open Protocol
open Client

type server_arg = Address of string | Alias of string

let client_address = Unix.(ADDR_INET (inet_addr_loopback, 9001))

let local_address = Unix.(ADDR_INET (inet_addr_loopback, 9000))
let remote_address = Unix.(ADDR_INET (inet_addr_of_string "142.93.193.196", 9000))
let default_address = local_address

let port = 9000

let backlog = 10

type connection =
  { socket: Lwt_unix.file_descr
  ; in_channel: Lwt_io.input_channel
  ; out_channel: Lwt_io.output_channel }

let rec while_connect sock server_address =
  catch
    (fun _ -> Lwt_unix.connect sock server_address)
    (fun _ -> while_connect sock server_address)

let create_connection address =
  Lwt_unix.(
    log_out "try connect" ;
    let server_address = match address with
      | Address a -> Unix.(ADDR_INET (inet_addr_of_string a, 9000))
      | Alias "default" -> default_address
      | Alias "local" -> local_address
      | Alias "remote" -> remote_address
      | _ -> print_endline "unrecognized alias or ip address"; exit 1
    in
    let sock = socket PF_INET SOCK_STREAM 0 in
    let%lwt () = while_connect sock server_address in
    (* ignore @@ bind sock client_address; *)
    return
      { socket= sock
      ; in_channel= Lwt_io.of_fd Lwt_io.Input sock
      ; out_channel= Lwt_io.of_fd Lwt_io.Output sock })

let send_msg conn msg = write_line conn.out_channel msg

let send_msg_blocking conn msg =
  let out_channel =
    Unix.out_channel_of_descr @@ Lwt_unix.unix_file_descr conn.socket
  in
  output_string out_channel msg ; Stdlib.flush out_channel

let rec listen_msg conn logs users () =
  let%lwt msg =
    catch
      (fun _ -> read_line conn.in_channel)
      (function
        | End_of_file ->
          ignore @@ Lwt_io.close conn.in_channel ;
          ANSITerminal.(erase Screen) ;
          ANSITerminal.set_cursor 1 1 ;
          print_endline "connection lost with server" ;
          exit 1 (* TODO: just raise the error and catch it later *)
        | e ->
          raise e)
  in
  handle_msg logs users msg;
  listen_msg conn logs users ()

let get_msg conn =
  read_line conn.in_channel
