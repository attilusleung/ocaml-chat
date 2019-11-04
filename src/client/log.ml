
let log_file = open_out "log.txt"

let log_out msg = output_string log_file @@ msg ^ "\n"; flush log_file
