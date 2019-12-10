(** [ChatLog] contains functions for storing and retrieving logs for messages
 * between clients. *)
open Parser
open DoublyLinkedList

(** [log_out msg user] records [msg] in the chatlog file of [user] *)
let log_out msg user =
  let chatlog_file = open_out_gen [Open_creat; Open_text; Open_append] 0o640
      ("./chatlogs/" ^ user ^ "_chatlog.txt") in
  output_string chatlog_file @@ msg ^ "\n"; flush chatlog_file

(** [retrieve_chatlog user] is the chatlogs of [user] *)
let retrieve_chatlog user =
  try
    let chatlog_file_read = open_in_gen [Open_text] 0o640
        ("./chatlogs/" ^ user ^ "_chatlog.txt") in
    let rec chatlog acc =
      try
        chatlog (acc @ [input_line chatlog_file_read])
      with End_of_file -> close_in chatlog_file_read; acc
    in
    chatlog []
  with _ -> []

(** [clear_chatlogs ()] removes all chatlog files in the server. *)
let clear_chatlogs () =
  let chatlogs = Sys.readdir "./chatlogs/" in
  for i = 0 to Array.length chatlogs - 1 do
    Sys.remove @@ "./chatlogs/" ^ chatlogs.(i) done
