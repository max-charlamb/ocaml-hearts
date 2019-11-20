open Unix
open ANSITerminal
open Command
open Card
open Partialdeck
open Round
open Print

let rec display_history state = 
  match Print.internal_display_history state with 
  | exception _ -> display_history state
  | v -> v

let get_card i state = 
  PartialDeck.find i (Round.hand state)

let get_difficulty = function
  | "easy" -> Round.Easy
  | "medium" -> Round.Medium
  | "hard" -> Round.Hard
  | _ -> Round.Invalid

let rec home_loop bl state =
  let state' = if bl then (Print.score_table state; display_history state) 
    else state in
  let (w,h) = size () in
  match Print.read_line_safe () with 
  | Quit -> Print.erase_print "Quit";
    set_cursor (1) (h-1);
    exit 0
  | Pass (i1,i2,i3) -> Print.erase_print "Pass";
    set_cursor (1) (h-1);
    home_loop true state'
  | Play (i) ->
    begin
      match get_card i state' with 
      | Some x -> 
        begin
          match Round.play x state' with 
          | Invalid msg -> 
            Print.erase_print msg;
            Print.score_table state';
            Print.print_pile (Round.pile state') (w/2) (2*h/3);
            Print.print_hand (Round.hand state') 1 1;
            home_loop true state'
          | Valid t -> 
            Print.score_table t;
            Print.print_pile (Round.pile state') (w/2) (2*h/3);
            Print.print_hand (Round.hand state') 1 1;
            home_loop true t
        end
      | None -> 
        Print.erase_print "Card not found";
        Print.score_table state';
        Print.print_pile (Round.pile state') (w/2) (2*h/3);
        Print.print_hand (Round.hand state') 1 1;
        home_loop true state'
    end
  | Help ->
    Print.erase_print "Help";
    set_cursor (1) (2*h/3);
    Print.print_help_menu ();
    home_loop false state'
  | Restart ->
    Print.erase_print "Restart";
    set_cursor (1) (2*h/3);
    main ()
  | Score ->
    Print.erase_print "Score";
    home_loop true state'
  | Back -> Print.erase_print "Back";
    let (w,h) = size () in 
    Print.print_pile (match Round.pile state with 
        | exception Failure _ -> []
        | x -> x ) (w/2) (2*h/3);
    let (w,h) = size () in
    set_cursor (1) (2*h/3);
    Print.print_hand (match Round.hand state with 
        | exception Failure _ -> PartialDeck.empty
        | x -> x) 1 1;
    set_cursor (1) (2*h/3);
    home_loop true state'
  | Select s -> begin Print.erase_print "Select"; end
  | Start -> Print.erase_print "Start"; let (w,h) = size () in 
    Print.score_table state;
    Print.print_pile (match Round.pile state with 
        | exception Failure _ -> []
        | x ->  x ) (w/2) (2*h/3);
    let (w,h) = size () in
    set_cursor (1) (2*h/3);
    Print.print_hand (match Round.hand state with 
        | exception Failure _ -> PartialDeck.empty
        | x -> x) 1 1;
    set_cursor (1) (2*h/3);
    home_loop true state'
  | Deal -> 
    begin 
      let new_st = Round.deal state' in 
      match new_st with 
      | Invalid msg -> 
        Print.erase_print msg;
        Print.score_table state';
        Print.print_pile (Round.pile state') (w/2) (2*h/3);
        Print.print_hand (Round.hand state') 1 1;
        home_loop true state'
      | Valid t -> 
        Print.score_table t;
        Print.print_pile (Round.pile state') (w/2) (2*h/3);
        Print.print_hand (Round.hand state') 1 1;
        home_loop true t
    end
and 
  main () = 
  Print.print_start_menu ();
  Unix.sleepf 1.5;
  erase Screen;
  difficulty ();

and difficulty () = 
  let (w,h) = size () in
  Print.print_bot_levels ();
  begin match Print.read_line_safe () with 
    | Select s ->  
      begin match get_difficulty s with
        | Round.Invalid ->
          print_string [on_black; white] "Not a valid level!"; 
          Unix.sleep 2; set_cursor (1) (h-1);
          difficulty ();
        | d -> Print.print_start_prompt ();
          begin  match Round.new_round (d) |> Round.deal with 
            | Valid (t) -> home_loop false t
            | Invalid(_) -> difficulty(); end 
      end
    | Quit -> Print.erase_print "Quit";
      set_cursor (1) (h-1);
      exit 0
    | _ -> erase Screen; difficulty () end

let () = main ()