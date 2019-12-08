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
  | _ -> Round.InvalidD

let rec home_loop bl state =
  let state' = if bl then (Print.score_table state; display_history state) 
    else state in
  if fst (Round.game_over state) then
    begin if snd (Round.game_over state) then Print.print_player_win ()
      else Print.print_bot_win () end else 
    let (w,h) = size () in
    match Print.read_line_safe () with 
    | Quit -> Print.erase_print "Quit";
      exit 0
    | Pass (i1,i2,i3) -> 
      begin
        match (get_card i1 state'), (get_card i2 state'), (get_card i3 state') with 
        | Some x1, Some x2, Some x3 -> 
          begin
            match Round.pass [x1;x2;x3] state' with 
            | Invalid msg -> 
              Print.erase_print msg;
              Print.score_table state';
              Print.print_pile (Round.pile state') (w/2) (2*h/3);
              Print.print_hand (Round.hand state') 1 1;
              home_loop false state'
            | Valid t -> 
              home_loop true t
          end
        | _,_,_ -> 
          Print.erase_print "Card not found";
          Print.score_table state';
          Print.print_pile (Round.pile state') (w/2) (2*h/3);
          Print.print_hand (Round.hand state') 1 1;
          home_loop true state'
      end
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
              home_loop false state'
            | Valid t -> 
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
      Print.print_help_menu ();
      home_loop false state' 
    | Debug -> 
      erase Screen;
      save_cursor ();
      set_cursor 1 (h/2);
      print_string [] (Round.string_of_round state);
      restore_cursor ();  
      home_loop false state'
    | Restart ->
      Print.erase_print "Restart";
      main ()
    | Back -> Print.erase_print "Back";
      Print.print_pile (match Round.pile state with 
          | exception Failure _ -> []
          | x -> x ) (w/2) (2*h/3);
      Print.print_hand (match Round.hand state with 
          | exception Failure _ -> PartialDeck.empty
          | x -> x) 1 1;
      home_loop true state'
    | Select s -> Print.erase_print "Invalid Command"; 
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
  name_select ();

and name_select () = 
  Print.print_name_prompt ();
  let (w,h) = size () in
  set_cursor (1) (h-1);
  match (print_string [on_default] "> "; read_line ()) with 
  | s -> if String.length s > 8 && s <> "" 
    then name_select () else difficulty () true s

and safe_create d name = 
  match Round.new_round (d) (name) |> Round.deal with 
  | Valid t -> erase Screen; home_loop true t
  | _ -> safe_create d name

and difficulty () b name = 
  let () = if b then (Print.print_bot_levels ()) 
    else Print.print_help_menu () in
  begin match Print.read_line_safe () with 
    | Select s ->  
      begin match get_difficulty s with
        | Round.Invalid -> difficulty () true s
        | d -> safe_create d name
      end
    | Quit -> Print.erase_print "Quit";
      exit 0
    | Help ->
      Print.erase_print "Help";
      Print.print_help_menu ();
      difficulty () false name
    | _ -> erase Screen; difficulty () true name end

let () = main ()