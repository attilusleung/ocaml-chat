open Lwt
open Lwt_io
open Log
open Parser
open Unix

let client_address = Unix.(ADDR_INET (inet_addr_loopback, 9001))

let server_address = Unix.(ADDR_INET (inet_addr_loopback, 9000))

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

let create_connection () =
  Lwt_unix.(
    log_out "try connect";
    let sock = socket PF_INET SOCK_STREAM 0 in
    let%lwt () = while_connect sock server_address in
    (* ignore @@ bind sock client_address; *)
    return
      { socket= sock
      ; in_channel= Lwt_io.of_fd Lwt_io.Input sock
      ; out_channel=
          Lwt_io.of_fd Lwt_io.Output sock })

let send_msg conn msg =
  write_line conn.out_channel msg

let rec listen_msg conn t () =
  let%lwt msg =
    catch (fun _ -> read_line conn.in_channel)
      (function
        |End_of_file ->
           ignore @@ Lwt_io.close conn.in_channel;
           ANSITerminal.(erase Screen);
           ANSITerminal.set_cursor 1 1;
           print_endline "connection lost with server";
           exit 1  (* TODO: just raise the error and catch it later *)
        | e -> raise e
      )
  in
  t := DoublyLinkedList.insert (parse msg) !t ;
  listen_msg conn t ()
