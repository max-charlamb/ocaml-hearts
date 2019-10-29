open Unix
open ANSITerminal
open Command

let erase print = 
  erase Screen;
  move_cursor 0 (-1);
  print_string [] (print^"\n");
  move_cursor 0 1

let rec read_line_safe () = 
  match parse (read_line ()) with 
  | exception Empty
  | exception Malformed -> 
    erase "Error";
    read_line_safe ()
  | c -> c

let rec home_loop state = 
  match read_line_safe () with 
    c -> erase "Sucsess"; 
    home_loop state


let main () = 
  home_loop 1


let () = main ()