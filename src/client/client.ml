exception NotLoggedIn

exception AlreadyLoggedIn

type user = LoggedIn of string | LoggedOut

let current_user = ref LoggedOut

let login_user name =
  if !current_user = LoggedOut then current_user := LoggedIn name
  else raise AlreadyLoggedIn

let get_user () =
  match !current_user with LoggedOut -> raise NotLoggedIn | LoggedIn s -> s
