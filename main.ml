open Unix
open ANSITerminal
open Command
open Card
open Partialdeck
open Round


let hand = PartialDeck.empty |> PartialDeck.insert {rank = Three; suite = Spade}
           |> PartialDeck.insert {rank= King; suite=Diamond} |> PartialDeck.insert {rank= Four; suite=Diamond} |> PartialDeck.insert {rank=Queen;suite=Club}
let midpile = [({rank=Jack; suite = Club}, 1);({rank=King; suite = Club}, 0); ({rank=Six; suite=Spade}, 3);({rank=Ten; suite = Heart},2)]

let print_hand d x y = 
  let rec aux hand_lst=
    match hand_lst with
    | (c, i)::t -> 
      set_cursor (1) (-1);
      print_card c;
      print_string [on_white;black] (" "^(string_of_int i));
      move_cursor (0) 1;
      aux t
    | [] -> ()
  in
  save_cursor ();
  set_cursor x y;
  aux (PartialDeck.to_list d);
  restore_cursor ()


let adjust_cursor w h i = 
  match i with 
  | 0 -> set_cursor (w/2) (h/2)
  | 1 -> set_cursor (w/3) (h/3)
  | 2 -> set_cursor (w/2) (h/4)
  | 3 -> set_cursor (2*w/3) (h/3)
  | _ -> failwith ""

let print_pile lst_cards x y = 
  let rec aux lst_cards = 
    match lst_cards with 
    | (c, i) :: t -> let () = let (w, h) = size () in adjust_cursor w h i in
      print_card c;
      print_string [on_white;black] (" ");
      aux t 
    | [] -> ()
  in save_cursor ();
  set_cursor x y; 
  aux lst_cards;
  restore_cursor ()

let print_start_menu () =
  let (w,h) = size () in
  erase Screen;
  set_cursor (1) (2*h/3);
  print_string [red; on_white] "Hearts";
  set_cursor 1 (2*h/3)

let print_help_menu () =
  let (w,h) = size () in
  erase Screen;
  set_cursor (1) (h/3);
  print_string [red; on_white] "\nRules of Hearts: ";
  set_cursor (1) (h/5);
  print_string [red; on_white] 
    "Commands: \n    score, restart, quit, help, back, play [index]";
  set_cursor 1 (2*h/3)



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
    set_cursor (1) (2*h/3);
    read_line_safe ()
  | c -> c

let rec display_history state = 
  if Round.is_next state then
    let ()  = erase "" in
    let new_state = Round.next state in 
    let (w,h) = size () in 
    print_pile (match Round.pile new_state with 
        | exception Failure _ -> []
        | x -> x ) (w/2) (2*h/3);
    set_cursor (1) (2*h/3);
    print_hand (match Round.hand new_state 0 with 
        | exception Failure _ -> PartialDeck.empty
        | x -> x) 1 1;
    set_cursor (w/2) (h/2);
    print_string [on_black; white] (Round.description new_state);
    set_cursor (1) (2*h/3);
    display_history new_state;
  else ()

let get_card i state = 
  match PartialDeck.find i (Round.hand state 0) with 
  | None -> failwith ""
  | Some x -> x

let rec home_loop state =
  display_history state;
  let (w,h) = size () in
  set_cursor (1) (2*h/3);
  match read_line_safe () with 
  | Quit -> erase "Quit";
    set_cursor (1) (2*h/3);
    exit 0
  | Pass (i1,i2,i3) -> erase "Pass";
    set_cursor (1) (2*h/3);
    home_loop state
  | Play (i) -> let new_st = Round.play (get_card i state) state in 
    let new_st' = match new_st with 
      | Invalid x -> failwith x
      | Valid t -> t in 
    let (w,h) = size () in 
    print_pile (match Round.pile state with 
        | exception Failure _ -> []
        | x -> x ) (w/2) (2*h/3);
    let (w,h) = size () in
    set_cursor (1) (2*h/3);
    print_hand (match Round.hand state 0 with 
        | exception Failure _ -> PartialDeck.empty
        | x -> x) 1 1;
    set_cursor (1) (2*h/3);
    home_loop new_st'
  | Help ->
    erase "Help";
    set_cursor (1) (2*h/3);
    print_help_menu ();
    home_loop state
  | Restart ->
    erase "Restart";
    set_cursor (1) (2*h/3);
    main ()
  | Score ->
    erase "Score";
    set_cursor (1) (2*h/3);
    home_loop state
  | Back -> erase "Back";
    let (w,h) = size () in 
    print_pile (match Round.pile state with 
        | exception Failure _ -> []
        | x -> x ) (w/2) (2*h/3);
    let (w,h) = size () in
    set_cursor (1) (2*h/3);
    print_hand (match Round.hand state 0 with 
        | exception Failure _ -> PartialDeck.empty
        | x -> x) 1 1;
    set_cursor (1) (2*h/3);
    home_loop state
  | Start -> erase "Start"; let (w,h) = size () in 
    print_pile (match Round.pile state with 
        | exception Failure _ -> []
        | x ->  x ) (w/2) (2*h/3);
    let (w,h) = size () in
    set_cursor (1) (2*h/3);
    print_hand (match Round.hand state 0 with 
        | exception Failure _ -> PartialDeck.empty
        | x -> x) 1 1;
    set_cursor (1) (2*h/3);
    home_loop state
and 
  main () = 
  print_start_menu ();
  Unix.sleep 1;
  ANSITerminal.erase Screen;
  Round.new_round |> Round.deal |> home_loop


let () = main ()