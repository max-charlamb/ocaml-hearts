open Unix
open ANSITerminal
open Command
open Card
open Partialdeck


let hand = PartialDeck.empty |> PartialDeck.insert {rank = Three; suite = Spade}
           |> PartialDeck.insert {rank= King; suite=Diamond} |> PartialDeck.insert {rank=Queen;suite=Club}

let print_hand d x y = 
  let rec aux hand_lst=
    match hand_lst with
    | (c, i)::t -> 
      let cx,cy = pos_cursor () in
      print_card c;
      print_string [on_white;black] (" "^(string_of_int i)); 
      set_cursor cx cy;
      move_cursor 0 (1);
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
  set_cursor 1 (h/2);
  print_string [red; on_white] "Hearts"



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
    read_line_safe ()
  | c -> c

let rec home_loop state = 
  match read_line_safe () with 
  | Quit -> erase "Quit";
    home_loop state
  | Pass (i1,i2,i3) -> erase "Pass";
    home_loop  state
  | Play (i) ->
    print_hand PartialDeck.full i 1;
    home_loop state
  | Help ->
    erase "Help";
    home_loop state
  | Restart ->
    erase "Restart";
    home_loop state
  | Score ->
    erase "Score";
    home_loop state


let main () = 
  home_loop 1


let () = main ()