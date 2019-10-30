open Array
open ANSITerminal
open Unix
open Panel

let termios = tcgetattr stdin

(*
   let test_panel = Panel.make 0 0 20 50
   let test_panel_2 = Panel.make 30 0 20 50
*)

let input_panel = InputPanel.make 0 0 80 3

let setraw () =
  let new_term = { termios with c_echo=false
                              ; c_icanon=false
                              (* ; c_isig=false *)
                              ; c_ixon=false
                              ; c_icrnl=false
                              ; c_opost=false
                              (* ; c_brkint=false *)
                              (* ; c_inpck=false *)
                              ; c_vmin=0
                              ; c_vtime=1
                 }
  in
  tcsetattr stdout TCSANOW new_term

let unsetraw () =
  tcsetattr stdout TCSANOW termios


let flush_screen buffer size =
  begin
    (* print_endline @@ string_of_int (fst s) ^ " " ^ string_of_int (snd s); *)
    (*
       for i = 0 to (snd s)-1 do
         for j = 0 to (fst s)-1 do
           b.(j).(i) <- "x"
         done
       done;
    *)
  for i = 0 to (snd size)-1 do
    for j = 0 to (fst size)-1 do
      Stdlib.print_string buffer.(j).(i)
    done;
  done;
  end;
  restore_cursor ()

let draw () =
  save_cursor ();
  set_cursor 1 1;
  let s = size () in
  let b = make_matrix (fst s) (snd s) " " in
  begin
    (* draw_border test_panel b false; *)
    InputPanel.draw input_panel b false;
    flush_screen b s
  end

let input () =
  try
    let c = input_byte Stdlib.stdin in
    Some c
  with End_of_file -> None

let update () =
  match input () with
  | None -> ()
  | Some c ->
    print_endline "hmm";
    InputPanel.update input_panel c; ()

let rec loop () =
  erase Screen;
  draw ();
  restore_cursor ();
  (*
     (try
       (match input_byte Stdlib.stdin with
         | 104 -> move_cursor ~-1 0
         | 106 -> move_cursor 0 1
         | 107 -> move_cursor 0 ~-1
         | 108 -> move_cursor 1 0
         | _ -> ());
  *)
    (* print_int c; *)
    (* set_cursor 1 2; *)
  update ();
  (* with End_of_file -> ()); *)
  loop ()

let () =
  setraw ();
  erase Screen;
  set_cursor 1 1;
  (* flush_screen (); *)
  (* unsetraw(); *)
  loop ()
