let chatlog_file = open_out_gen [Open_creat; Open_text; Open_append] 0o640 "chatlog.txt"

let log_out msg = output_string chatlog_file @@ msg ^ "\n"; flush chatlog_file

(* TODO *)
let retrieve_chatlog n =
  let chatlog_file_read = open_in_gen [Open_rdonly] 0o640 "chatlog.txt" in
  let rec chatlog n acc =
    try
      if List.length acc >= n
      then chatlog n ((List.tl acc) @ [input_line chatlog_file_read])
      else chatlog n (acc @ [input_line chatlog_file_read])
    with End_of_file -> close_in chatlog_file_read; acc
  in
  chatlog n []
