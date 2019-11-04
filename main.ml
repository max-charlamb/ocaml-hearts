open Unix
open ANSITerminal
open Command
open Card
open Partialdeck


let hand = PartialDeck.empty |> PartialDeck.insert {rank = Three; suite = Spade}
           |> PartialDeck.insert {rank= King; suite=Diamond} |> PartialDeck.insert {rank= Four; suite=Diamond} |> PartialDeck.insert {rank=Queen;suite=Club}

let print_hand d x y = 
  let rec aux hand_lst=
    match hand_lst with
    | (c, i)::t -> 
      print_card c;
      print_string [on_white;black] (" "^(string_of_int i));
      move_cursor (-5) (1);
      aux t
    | [] -> ()
  in
  save_cursor ();
  set_cursor x y;
  aux (PartialDeck.to_list d);
  restore_cursor ()

let print_start_menu () =
  let (w,h) = size () in
  erase Screen;
  set_cursor (1) (h/3);
  print_string [red; on_white] "Hearts";
  set_cursor 1 (h/2)

let print_help_menu () =
  let (w,h) = size () in
  erase Screen;
  set_cursor (1) (h/3);
  print_string [red; on_white] "\nRules of Hearts: ";
  set_cursor (1) (h/5);
  print_string [red; on_white] "Commands: \n    score, restart, quit, help, play [index], pass [index] [index] [index]";
  set_cursor 1 (h/2)



let erase print = 
  erase Screen;
  move_cursor 0 (-2);
  print_string [] print;
  move_cursor 0 2;
  move_bol ()



let rec read_line_safe () = 
  match parse (read_line ()) with 
  | exception Empty
  | exception Malformed -> 
    erase "Error";
    let (w,h) = size () in
    set_cursor (1) (h/2);
    read_line_safe ()
  | c -> c

let rec home_loop state = 
  match read_line_safe () with 
  | Quit -> erase "Quit";
    let (w,h) = size () in
    set_cursor (1) (h/2);
    home_loop state
  | Pass (i1,i2,i3) -> erase "Pass";
    let (w,h) = size () in
    set_cursor (1) (h/2);
    home_loop  state
  | Play (i) -> erase "Play";
    print_hand hand i 1;
    let (w,h) = size () in
    set_cursor (1) (h/2);
    home_loop state
  | Help ->
    erase "Help";
    let (w,h) = size () in
    set_cursor (1) (h/2);
    print_help_menu ();
    home_loop state
  | Restart ->
    erase "Restart";
    let (w,h) = size () in
    set_cursor (1) (h/2);
    home_loop state
  | Score ->
    erase "Score";
    let (w,h) = size () in
    set_cursor (1) (h/2);
    home_loop state


let main () = 
  print_start_menu ();
  Unix.sleep 2;
  ANSITerminal.erase Screen;
  home_loop 1


let () = main ()