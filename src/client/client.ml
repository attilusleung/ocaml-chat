open Parser
open Protocol
open Printexc
open Log

exception NotLoggedIn

exception AlreadyLoggedIn

(** [user] is a type that represents the user using the client. It is either
 * logged in with a username or logged out. *)
type user = LoggedIn of string | LoggedOut

type logs = (string, Parser.t DoublyLinkedList.t) Hashtbl.t

(** [current_user] is the pointer referencing the user using the client. *)
let current_user = ref LoggedOut

(** [selected_user] is a pointer referencing the "active" or selected user of
 * the client, which is the user the client is currently communicating with
 * directly. All messages will be sent to the selected user, and only mesages to
 * and from the selected_user is displayed. *)
let selected_user = ref "" (* TODO *)

let login_user message =
  log_out "login" ;
  if !current_user != LoggedOut then raise AlreadyLoggedIn ;
  match decode message with
  | Confirm user ->
    current_user := LoggedIn user ;
    true
  | Fail _ ->
    false
  | _ ->
    false

let get_user () =
  match !current_user with LoggedOut -> raise NotLoggedIn | LoggedIn s -> s

let select_user user = selected_user := user

let get_selected () = !selected_user

let handle_msg logs users msg =
  try
    match decode msg with
    | Message p ->
      let user =
        if get_user () = get_from_user p then get_to_user p
        else get_from_user p
      in
      let prev_logs =
        match Hashtbl.find_opt logs user with
        | Some l ->
          l
        | None ->
          DoublyLinkedList.empty
      in
      Hashtbl.replace logs user (DoublyLinkedList.insert p prev_logs)
    | Status (a, r) ->
      log_out "status" ;
      let rec mem elem = function
        | h :: t -> 
          if String.equal (String.trim h) (String.trim elem) 
          then true else mem elem t
        | [] -> false
      in
      let rec remove_from_list lst rem acc =
        match lst with
        | [] ->
          acc
        | h :: t ->
          log_out h;
          if mem h rem then (
            log_out "remove" ; remove_from_list t rem acc )
          else (log_out @@ "nope " ^ h; remove_from_list t rem (h :: acc))
      in
      let remed = (remove_from_list !users r [])
      in
      users := remed @ a
    | _ ->
      log_out msg
  with e ->
    log_out @@ "Unhandled exception occured during decode: " ^ to_string e
