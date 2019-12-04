open Parser
open DoublyLinkedList

let log_out msg user = 
  let chatlog_file = open_out_gen [Open_creat; Open_text; Open_append] 0o640 
      (user ^ "_chatlog.txt") in
  output_string chatlog_file @@ msg ^ "\n"; flush chatlog_file

(* TODO *)
let retrieve_chatlog user =
  let chatlog_file_read = open_in_gen [Open_creat; Open_text] 0o640 
      (user ^ "_chatlog.txt") in
  let rec chatlog acc =
    try
      chatlog (acc @ [input_line chatlog_file_read])
    with End_of_file -> close_in chatlog_file_read; acc
  in
  chatlog []

(* let retrieve_chatlog user =
   let chatlog_file_read = open_in_gen [Open_creat; Open_text] 0o640 
      (user ^ "_chatlog.txt") in
   let rec chatlog acc =
    try
      let msg = parse (input_line chatlog_file_read) in
      let from_user = get_from_user msg in
      let to_user = get_to_user msg in
      let new_acc = 
        if from_user = user 
        then
          match Hashtbl.find_opt acc to_user with
          | Some lst -> Hashtbl.replace acc to_user (insert msg lst); acc
          | None -> Hashtbl.add acc to_user (insert msg empty); acc
        else
          match Hashtbl.find_opt acc from_user with
          | Some lst -> Hashtbl.replace acc from_user (insert msg lst); acc
          | None -> Hashtbl.add acc from_user (insert msg empty); acc in
      chatlog new_acc
    with End_of_file -> close_in chatlog_file_read; acc
   in
   chatlog (Hashtbl.create 5) *)