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

let print_table () = 
  let (w,h) = size () in
  set_cursor (w/4) (h/4);
  move_cursor (0) (-2);
  set_cursor (w/4) (h/4);
  move_cursor 0(-1);
  print_string [on_green] "                                          ";
  set_cursor (w/4) (h/4);
  move_cursor 0 (0);
  print_string [on_green] "                                          ";
  set_cursor (w/4) (h/4);
  move_cursor 0 1;
  print_string [on_green] "                                          ";
  set_cursor (w/4) (h/4);
  move_cursor 0 2;
  print_string [on_green] "                                          ";
  set_cursor (w/4) (h/4);
  move_cursor 0 3;
  print_string [on_green] "                                          ";
  set_cursor (w/4) (h/4);
  move_cursor 0 4;
  print_string [on_green] "                                          ";
  set_cursor (w/4) (h/4);
  move_cursor 0 5;
  print_string [on_green] "                                          ";
  set_cursor (w/4) (h/4);
  move_cursor 0 6;
  print_string [on_green] "                                          ";
  set_cursor (w/4) (h/4);
  move_cursor 0 7;
  print_string [on_green] "                                          ";
  set_cursor (w/4) (h/4);
  move_cursor 0 8;
  print_string [on_green] "                                          ";
  set_cursor (w/4) (h/4);
  move_cursor 0 9;
  print_string [on_green] "                                          ";
  set_cursor (w/4) (h/4);
  move_cursor 0 (10)

let print_pile lst_cards x y = 
  print_table ();
  let rec aux lst_cards = 
    match lst_cards with 
    | (c, i) :: t -> let () = let (w, h) = size () in adjust_cursor w h i in
      print_card_tall c;
      aux t 
    | [] -> ()
  in save_cursor ();
  set_cursor x y; 
  aux lst_cards;
  restore_cursor ()

let print_start_menu () =
  let (w,h) = size () in
  erase Screen;
  set_cursor (w/2 - 10) (2*h/3);
  print_string [red; on_white] "♡♡♡♡♡ Hearts ♡♡♡♡♡";
  set_cursor 1 (h)

let print_help_menu () =
  let (w,h) = size () in
  erase Screen;
  set_cursor (1) (h/3);
  print_string [red; on_white] "\nRules of Hearts: ";
  set_cursor (1) (h/5);
  print_string [red; on_white] 
    "Commands: \n    score, restart, quit, help, back, play [index]";
  set_cursor 1 (2*h/3)

let score_table t = 
  let (w,h) = size () in
  set_cursor (3*w/5) (4*h/5);
  print_string [on_default] "      Total game score";
  set_cursor (3*w/5) (4*h/5);
  move_cursor 0 (1);
  print_string [on_default] "______________________________";
  set_cursor (3*w/5) (4*h/5);
  move_cursor 2 (2);
  let rec aux_n = function
    | [] -> ""
    | a :: [] -> a
    | a :: t -> a ^ " | " ^ aux_n t in
  print_string [on_default] (aux_n (Round.names t));
  set_cursor (3*w/5) (4*h/5);
  move_cursor 3 (3);
  let scores = 
    List.fold_right (fun a acc -> " " ^ ( a |> string_of_int) ^ "     " ^ acc) 
      (Round.end_of_round_score t) "" in
  print_string [on_default] scores

let erase_print print = 
  erase Screen;
  move_cursor 0 (-2);
  print_string [] print;
  move_cursor 0 2;
  move_bol ()

let rec read_line_safe state = 
  match parse (print_string [on_default] "> "; read_line ()) with 
  | exception Empty
  | exception Malformed -> 
    erase_print "Error";
    score_table state;
    let (w,h) = size () in
    print_pile (Round.pile state) (w/2) (2*h/3);
    print_hand (Round.hand state ) 1 1;
    set_cursor (1) (h);
    read_line_safe state
  | c -> c

let rec internal_display_history state = 
  if Round.is_next state then
    begin
      erase Screen;
      let state' = Round.next state in 
      let (w,h) = size () in 
      print_pile (Round.pile state') (w/2) (2*h/3);
      set_cursor (1) (2*h/3);
      print_hand (Round.hand state') 1 1;
      set_cursor (1) (h-1);
      print_string [on_black; white] (Round.description state');
      score_table state';
      set_cursor (1) (h);
      Unix.sleepf 0.5;
      internal_display_history state';
    end
  else state

let rec display_history state = 
  match internal_display_history state with 
  | exception _ -> display_history state
  | v -> v

let get_card i state = 
  match PartialDeck.find i (Round.hand state) with 
  | None -> failwith ""
  | Some x -> x

let rec home_loop bl state =
  let state' = if bl then let s = score_table state; display_history state in s else state in
  let (w,h) = size () in
  set_cursor (1) (h);
  match read_line_safe state with 
  | Quit -> erase_print "Quit";
    set_cursor (1) (h);
    exit 0
  | Pass (i1,i2,i3) -> erase_print "Pass";

    set_cursor (1) (h);
    home_loop true state'
  | Play (i) -> begin let new_st = Round.play (get_card i state') state' in 
      match new_st with 
      | Invalid msg -> 
        erase_print msg;
        score_table state';
        print_pile (Round.pile state') (w/2) (2*h/3);
        print_hand (Round.hand state') 1 1;
        home_loop true state'
      | Valid t -> 
        score_table t;
        print_pile (Round.pile state') (w/2) (2*h/3);
        print_hand (Round.hand state') 1 1;
        home_loop true t
    end
  | Help ->
    erase_print "Help";
    set_cursor (1) (2*h/3);
    print_help_menu ();
    home_loop false state'
  | Restart ->
    erase_print "Restart";
    set_cursor (1) (2*h/3);
    main ()
  | Score ->
    erase_print "Score";
    home_loop true state'
  | Back -> erase_print "Back";
    let (w,h) = size () in 
    print_pile (match Round.pile state with 
        | exception Failure _ -> []
        | x -> x ) (w/2) (2*h/3);
    let (w,h) = size () in
    set_cursor (1) (2*h/3);
    print_hand (match Round.hand state with 
        | exception Failure _ -> PartialDeck.empty
        | x -> x) 1 1;
    set_cursor (1) (2*h/3);
    home_loop true state'
  | Start -> erase_print "Start"; let (w,h) = size () in 
    score_table state;
    print_pile (match Round.pile state with 
        | exception Failure _ -> []
        | x ->  x ) (w/2) (2*h/3);
    let (w,h) = size () in
    set_cursor (1) (2*h/3);
    print_hand (match Round.hand state with 
        | exception Failure _ -> PartialDeck.empty
        | x -> x) 1 1;
    set_cursor (1) (2*h/3);
    home_loop true state'
and 
  main () = 
  print_start_menu ();
  Unix.sleep 2;
  erase Screen;
  match Round.new_round |> Round.deal with 
  | Valid(t) -> home_loop false t
  | Invalid(_) -> failwith "error"



let () = main ()