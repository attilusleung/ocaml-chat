(** [Log] is a simple logging module that writes everything to a log file *)

(** [log_file] is the file descriptor of the log file *)
let log_file = open_out "log.txt"

(** [log_out msg] writes [msg] to a new line in the log file *)
let log_out msg = output_string log_file @@ msg ^ "\n"; flush log_file
