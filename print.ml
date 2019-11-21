open Unix
open ANSITerminal
open Command
open Card
open Partialdeck
open Round

module type PrintSig = sig 

  val print_hand : Partialdeck.PartialDeck.t -> int -> int -> unit
  val adjust_cursor : int -> int -> int -> unit
  val print_table : int -> unit
  val print_start_menu : unit -> unit
  val print_help_menu : unit -> unit
  val print_bot_levels : unit -> unit
  val score_table : Round.t -> unit
  val erase_print : string -> unit
  val print_pile : (Card.card * int) list -> int -> int -> unit
  val internal_display_history : Round.t -> Round.t
  val read_line_safe : unit -> Command.command

end

module Print:PrintSig = struct 

  let print_hand d x y = 
    let rec aux hand_lst =
      match hand_lst with
      | (c, i)::t -> 
        set_cursor (1) (-1);
        print_card c;
        print_string [on_white;black] (" | "^(string_of_int i));
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

  let rec spaces s l = 
    if l > 0 then s ^ spaces (s) (l-1) else ""

  let rec print_border height n leading = 
    if n >= 0 then 
      let (w,h) = size () in
      set_cursor (leading) (height + n);
      print_string [white; on_red] "♡♡";
      set_cursor (w - leading) (height + n);
      print_string [white; on_red] "♡♡";
      print_border height (n-1) leading
    else ()

  let rec print_table n = 
    if n >= 0 then 
      let (w,h) = size () in
      set_cursor (w/4) (h/4 + (n - 2));
      print_string [on_green] (spaces " " (w/2 + 4));
      print_table (n - 1)
    else ()

  let print_pile lst_cards x y = 
    print_table 12;
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
    set_cursor (w/2 - 12) (h/4);
    print_string [white; on_red] (spaces "♡" (8));
    print_string [] (spaces " " 8);
    print_string [white; on_red] (spaces "♡" (8));
    set_cursor (1) (h-1);
    Unix.sleepf 0.1;
    set_cursor (w/2 - 12) (h/4);
    move_cursor (-2) 1;
    print_string [white; on_red] (spaces "♡" (12));
    print_string [] (spaces " " 4);
    print_string [white; on_red] (spaces "♡" (12));
    set_cursor (1) (h-1);
    Unix.sleepf 0.1;
    set_cursor (w/2 - 12) (h/4);
    move_cursor (-4) 2;
    print_string [white; on_red] (spaces "♡" (32));
    set_cursor (1) (h-1);
    Unix.sleepf 0.1;
    set_cursor (w/2 - 12) (h/4);
    move_cursor (-4) 3;
    print_string [white; on_red] (spaces "♡" (32));
    set_cursor (1) (h-1);
    Unix.sleepf 0.1;
    set_cursor (w/2 - 12) (h/4);
    move_cursor (-4) 4;
    print_string [white; on_red] (spaces "♡" (32));
    set_cursor (1) (h-1);
    Unix.sleepf 0.1;
    set_cursor (w/2 - 12) (h/4);
    move_cursor (-4) 5;
    print_string [white; on_red] (spaces "♡" (12));
    print_string [white; on_red] (" HEARTS ");
    print_string [white; on_red] (spaces "♡" (12));
    set_cursor (1) (h-1);
    Unix.sleepf 0.1;
    set_cursor (w/2 - 12) (h/4);
    move_cursor (-4) 6;
    print_string [white; on_red] (spaces "♡" (32));
    set_cursor (1) (h-1);
    Unix.sleepf 0.1;
    set_cursor (w/2 - 12) (h/4);
    move_cursor (-2) 7;
    print_string [white; on_red] (spaces "♡" (28));
    set_cursor (1) (h-1);
    Unix.sleepf 0.1;
    set_cursor (w/2 - 12) (h/4);
    move_cursor (0) 8;
    print_string [white; on_red] (spaces "♡" (24));
    set_cursor (1) (h-1);
    Unix.sleepf 0.1;
    set_cursor (w/2 - 12) (h/4);
    move_cursor (2) 9;
    print_string [white; on_red] (spaces "♡" (20));
    set_cursor (1) (h-1);
    Unix.sleepf 0.1;
    set_cursor (w/2 - 12) (h/4);
    move_cursor (4) 10;
    print_string [white; on_red] (spaces "♡" (16));
    set_cursor (1) (h-1);
    Unix.sleepf 0.1;
    set_cursor (w/2 - 12) (h/4);
    move_cursor (6) 11;
    print_string [white; on_red] (spaces "♡" (12));
    set_cursor (1) (h-1);
    Unix.sleepf 0.1;
    set_cursor (w/2 - 12) (h/4);
    move_cursor (8) 12;
    print_string [white; on_red] (spaces "♡" (8));
    set_cursor (1) (h-1);
    Unix.sleepf 0.1;
    set_cursor (w/2 - 12) (h/4);
    move_cursor (10) 13;
    print_string [white; on_red] (spaces "♡" (4));
    set_cursor (1) (h-1);
    Unix.sleepf 0.1;
    set_cursor 1 (h-1)

  let print_help_menu () =
    let (w,h) = size () in
    erase Screen;
    set_cursor (1) (1);
    print_string [white; on_red] (spaces "♡" (w));
    set_cursor (4) (2);
    print_string [on_default] "Rules of Hearts:";
    set_cursor (4) (3);
    print_string [on_default] 
      "The objective of Hearts is to get as few points as possible. 
   Each heart gives one penalty point. There is also one special card, the 
   Queen of spades, which gives 13 penalty points. Each turn starts with 
   one player playing a single card, also called leading. The suit of that 
   card determines the suit of the trick. The other players then play one 
   card each. If they have a card in the same suit as the first card then 
   they must play that. If they don't then they can play one of their other 
   cards. Once four cards have been played, the player who played the highest 
   ranking card in the original suit takes the trick, i.e. they take the four 
   cards on the table and he then starts the next turn. Any penalty cards in 
   the trick (hearts or queen of spades) are added to the players penalty 
   score. So you want to avoid taking any tricks that have hearts or the 
   queen of spades.";
    set_cursor (4) 17;
    print_string [on_default]
      "Commands - | restart | quit | help | back | play [index] |";
    set_cursor (4) 19;
    print_string [on_default] "To return back to the previous screen type ";
    print_string [black; on_white] "back";
    set_cursor (1) (20);
    print_string [white; on_red] (spaces "♡" (w));
    print_border (1) 18 1;
    set_cursor 1 (2*h/3)

  let print_bot_levels () = 
    let (w,h) = size () in
    erase Screen;
    print_border  (h/3) 6 1;
    set_cursor (1) (h/3);
    print_string [white; on_red] (spaces "♡" (w));
    set_cursor (w/2 - 32) (h/3 + 2);
    print_string [on_default] "Select one of the three difficulty levels by typing ";
    print_string [black; on_white] "select [level]";
    set_cursor (w/2 - 9) (h/3 + 3);
    print_string [on_default]  "easy | medium | hard";
    set_cursor (w/2 - 15) (h/3 + 5);
    print_string [on_default]  "If you don't know the rules, type ";
    print_string [black; on_white] "help";
    set_cursor (1) (h/3 + 7);
    print_string [white; on_red] (spaces "♡" (w));
    set_cursor 1 (h-1)

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

  let rec internal_display_history state = 
    if Round.is_next state then
      begin
        erase Screen;
        let state' = Round.next state in 
        let (w,h) = size () in 
        print_pile (Round.pile state') (w/2) (2*h/3);
        set_cursor (1) (2*h/3);
        print_hand (Round.hand state') 1 1;
        set_cursor (1) (h-2);
        print_string [on_black; white] (Round.description state');
        score_table state';
        set_cursor (1) (h-1);
        Unix.sleepf 0.75;
        internal_display_history state';
      end
    else begin
      let (w,h) = size () in 
      print_pile (Round.pile state) (w/2) (2*h/3);
      set_cursor (1) (2*h/3);
      print_hand (Round.hand state) 1 1;
      score_table state;
      set_cursor (1) (h-1); 
      state
    end 

  let erase_print print = 
    erase Screen;
    move_cursor 0 (-2);
    print_string [] print;
    move_cursor 0 2;
    move_bol ()


  let rec read_line_safe () = 
    let (w,h) = size () in
    set_cursor (1) (h-1);
    match parse (print_string [on_default] "> "; read_line ()) with 
    | exception Empty
    | exception Malformed -> 
      set_cursor (1) (h-1);
      erase Eol;
      read_line_safe ()
    | c -> c

end