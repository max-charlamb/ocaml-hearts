open Unix
open ANSITerminal
open Command
open Card
open Partialdeck

let erase print = 
  erase Screen;
  move_cursor 0 (-1);
  match PartialDeck.full |> PartialDeck.random_card with
  | Some c -> print_card c;
    move_cursor 0 1;
    move_bol ();
  | None -> failwith "error"
(* print_string [] (print^"\n"); *)


let rec read_line_safe () = 
  match parse (read_line ()) with 
  | exception Empty
  | exception Malformed -> 
    erase "Error";
    read_line_safe ()
  | c -> c

let rec home_loop state = 
  match read_line_safe () with 
    c -> erase "Succsess"; 
    home_loop state


let main () = 
  home_loop 1


let () = main ()