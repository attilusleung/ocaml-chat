open Parser
open Protocol
open Printexc
open Log

exception NotLoggedIn

exception AlreadyLoggedIn

type user = LoggedIn of string | LoggedOut

let current_user = ref LoggedOut

let selected_user = ref "hmm" (* TODO *)

let login_user name =
  if !current_user = LoggedOut then current_user := LoggedIn name
  else raise AlreadyLoggedIn

let get_user () =
  match !current_user with LoggedOut -> raise NotLoggedIn | LoggedIn s -> s

let select_user user = selected_user := user

let get_selected () = !selected_user

let handle_msg logs users msg =
  try
    match decode msg with
    | Message p ->
      let user = 
        if (get_user ()) = get_from_user p 
        then get_to_user p 
        else get_from_user p in
      let prev_logs =
        match Hashtbl.find_opt logs user with
        | Some l ->
          l
        | None ->
          DoublyLinkedList.empty
      in
      Hashtbl.replace logs user (DoublyLinkedList.insert p prev_logs)
    | Status (a, r) ->
      let rec remove_from_list lst rem acc =
        match lst with
        | [] ->
          acc
        | h :: t ->
          if List.mem h rem then remove_from_list t rem acc
          else remove_from_list t rem (h :: acc)
      in
      users := List.append (remove_from_list !users r []) a
    | _ ->
      ()
  with e ->
    log_out @@ "Unhandled exception occured during decode: " ^ to_string e
