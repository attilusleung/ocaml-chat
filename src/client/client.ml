exception NotLoggedIn

exception AlreadyLoggedIn

type user = LoggedIn of string | LoggedOut

let current_user = ref LoggedOut

let selected_user = ref "yeet" (* TODO *)

let login_user name =
  if !current_user = LoggedOut then current_user := LoggedIn name
  else raise AlreadyLoggedIn

let get_user () =
  match !current_user with LoggedOut -> raise NotLoggedIn | LoggedIn s -> s

let select_user user = selected_user := user

let get_selected () = !selected_user
