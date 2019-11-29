open Card
open Partialdeck
open Command
open Bot
open ListQueue

module type RoundSig = sig
  type t
  type difficulty = Easy | Medium | Hard | Invalid
  type result = Valid of t | Invalid of string
  val new_round : difficulty -> t
  val deal : t -> result
  val play : card -> t -> result
  val pass : card list -> t -> result
  val hand : t -> PartialDeck.t
  val pile : t -> (card * int) list
  val next : t -> t
  val description : t -> string
  val is_next : t -> bool
  val bot_hand : t -> int -> PartialDeck.t
  val bot_pile : t -> (card * int) list
  val round_score : t -> int list
  val total_score : t -> int list 
  val names : t -> string list
  val string_of_round : t -> string
  val next_action : t -> string
end


module Round:RoundSig = struct

  exception Default of string
  exception InvalidCardPlayed
  exception BotError

  type difficulty = Easy | Medium | Hard | Invalid
  type action = Play | Lead | Pass | Deal

  type player = {
    name : string;
    id : int;
    hand : PartialDeck.t;
    score : int;
    p_cards : PartialDeck.t;
  }

  type historySegment = {
    hands : PartialDeck.t list;
    pile : (card * int) list;
    round_score : int list;
    description : string;
  }

  type t = {
    players : player list;
    pile : (card * int) list;
    is_over : bool;
    hearts_broken : bool;
    first_round: bool;
    next_player: int;
    next_action: action;
    total_scores : int list;
    history : historySegment ListQueue.t;
    difficulty : difficulty;
    round_number : int;
  }

  type result = Valid of t | Invalid of string

  let debug = true

  let create_player name id = 
    {
      name = name;
      id = id;
      hand = PartialDeck.empty;
      score = 0;
      p_cards = PartialDeck.empty;
    }

  let new_round diff = {
    players = [create_player "Henry" 0; 
               create_player "Bot1" 1; 
               create_player "Bot2" 2; 
               create_player "Bot3" 3];
    pile = [];
    is_over = false;
    hearts_broken = false;
    first_round = true;
    next_player = 0;
    total_scores = [0;0;0;0];
    next_action = Deal;
    history = ListQueue.empty;
    difficulty = diff;
    round_number = 0;
  }

  let insert_hand h p = 
    {
      p with
      hand = h
    }

  let rec deal_round (players: player list) d = 
    match (PartialDeck.random_card d, players) with
    | None, _ -> d, players
    | Some c, h :: t -> 
      let changed_d = PartialDeck.move c d h.hand in 
      let new_deck = changed_d |> fst in
      let new_hand = changed_d |> snd in
      let rec_call = deal_round t new_deck in
      rec_call |> fst , 
      insert_hand new_hand h :: (rec_call |> snd)
    | Some _, [] -> d, players

  let player_with_leading_card players = 
    let card_to_find = {suite = Club; rank = Two} in
    List.fold_left 
      (fun (id, found) player -> 
         if found then (id,found) else
           (id + 1, PartialDeck.mem card_to_find player.hand) 
      ) 
      (-1,false) players |> fst


  let id_to_name id t = 
    (List.nth t.players id).name

  let rec deal_helper players d = 
    match deal_round players d with
    | d', p' -> if PartialDeck.is_empty d' then p' else deal_helper p' d'

  let check_deal t = 
    if t.next_action <> Deal then raise (Default "Next action is not to deal")


  (** [hand_size t] is the hand size of all the players. Can only be
      called between tricks. If [debug] then fails if not all equal. *)
  let hand_size t = 
    let hand_size = PartialDeck.size (List.hd t.players).hand in
    if debug then 
      if List.for_all 
          (fun player -> PartialDeck.size player.hand = hand_size) t.players
      then hand_size else failwith "player hand sizes are not equal"
    else hand_size

  (** [update_action t] is [t] with the next action that should be done. *)
  let update_action t =
    let next_action = 
      match t.next_action with 
      | _ when hand_size t = 0 -> Deal
      | Deal when (t.round_number mod 4) = 0 -> Lead
      | Deal -> Pass 
      | Pass -> Lead
      | Lead -> Play 
      | Play when List.length t.pile = 0 -> Lead
      | Play -> Play
    in
    let next_player = 
      match t.next_action with 
      | _ when hand_size t = 0 -> 0
      | Deal when (t.round_number mod 4) = 0 -> player_with_leading_card t.players
      | Deal -> 0 
      | Pass -> player_with_leading_card t.players
      | Lead -> t.next_player 
      | Play -> t.next_player
    in
    { 
      t with 
      next_action = next_action;
      next_player = next_player;
    }


  let check_voided id card t =
    let user = List.nth t.players id in 
    let leading_suite = 
      (List.nth t.pile ((List.length t.pile) - 1) |> fst).suite in
    if card.suite <> leading_suite && 
       not (PartialDeck.voided leading_suite user.hand) then 
      raise (Default( "played a " ^ 
                      (suite_to_string card.suite) ^ 
                      " but expected a " ^ (suite_to_string leading_suite)))

  let check_in_hand id card t =
    let user = List.nth t.players id in 
    if not (PartialDeck.mem card user.hand) then
      raise (Default "problem in check_in_hand")

  let check_play_in_turn id card t = 
    if t.next_player <> id || t.next_action <> Play then 
      raise (Default "problem in check_play_in_turn")

  let check_play_first_round id card t = 
    if t.first_round then 
      match card with
      | {suite=Spade; rank=Queen}
      | {suite=Heart} -> raise InvalidCardPlayed
      | _ -> ()

  let check_lead_in_turn id card t = 
    if t.next_action <> Lead || t.next_player <> id then 
      raise (Default ("problem in check_lead_in_turn")) else ()

  let check_lead_first_round id card t =
    if t.first_round && card <> {suite=Club;rank=Two} then 
      raise (Default "You must play the two of clubs!") else ()

  let check_pass_in_turn id card_l t = 
    if t.next_action <> Pass || t.next_player <> id then 
      raise (Default ("Not the time to pass")) else ()

  let check_lead_hearts_broken id card t = 
    let user = List.nth t.players id in 
    let contains_other_cards = (PartialDeck.voided Club user.hand) && 
                               (PartialDeck.voided Diamond user.hand) &&
                               (PartialDeck.voided Spade user.hand) in
    if not t.hearts_broken && card.suite = Heart && not contains_other_cards 
    then 
      raise (Default "Hearts are not yet broken! Play another card.") else ()

  let add_to_pile id card t = 
    let pile' = (card, id)::t.pile in
    let players' = List.map 
        (fun player -> if player.id = id then 
            {
              player with
              hand = PartialDeck.remove card player.hand
            } else player) t.players in
    let new_history = {
      hands = List.map (fun player -> player.hand) players';
      pile = pile';
      round_score = (List.map (fun player -> player.score) players');
      description = (id_to_name id t) ^ " played the " 
                    ^ card_to_string card ^ "."
    } in
    {
      t with 
      pile = pile';
      players = players';
      history = ListQueue.push new_history t.history;
    } 

  let get_hand id t = 
    (List.nth t.players id).hand

  let set_hand id pd t = 
    {
      t with 
      players = List.map 
          (fun player -> if player.id = id then 
              {
                player with 
                hand = pd;
              } else 
              player)
          t.players
    }

  let increment_actions_play t = 
    match t.next_player with 
    | 3 when List.length t.pile < 4 -> 
      {t with next_player=0; next_action=Play;}
    | _ when List.length t.pile < 4 ->  
      {t with next_player=(t.next_player + 1); next_action=Play;}
    | _ -> 
      raise (Default "error in incrementing")

  let clean_up_trick t = 
    let leading_suite = 
      (List.nth t.pile ((List.length t.pile) - 1) |> fst).suite in
    let winner = t.pile |> List.filter (fun (c,_) -> c.suite = leading_suite) 
                 |> List.sort (fun (c1,_) (c2,_) -> compare c1 c2) 
                 |> List.rev |> List.hd in
    let pile_partialdeck = List.fold_left 
        (fun p (c,_) -> PartialDeck.insert c p) 
        PartialDeck.empty t.pile in
    let hearts_broken' = t.hearts_broken || 
                         PartialDeck.contains_hearts pile_partialdeck in 
    let players' = 
      List.map 
        (fun player -> if player.id = snd winner then 
            {
              player with 
              score = player.score + 
                      (PartialDeck.count_points pile_partialdeck);
              p_cards = PartialDeck.merge pile_partialdeck player.p_cards;
            } 
          else player) t.players 
    in
    let new_history = {
      hands = List.map (fun player -> player.hand) players';
      pile = [];
      round_score = List.map (fun player -> player.score) players';
      description = (id_to_name (snd winner) t) ^ " won the trick with a " 
                    ^ card_to_string (fst winner) ^ ".";
    }
    in
    if hand_size t = 0 then 
      { 
        t with 
        total_scores = List.map2 
            (fun player old_score -> player.score + old_score) 
            t.players t.total_scores;
        pile = [];
        players = List.map (fun player -> 
            {
              player with 
              hand = PartialDeck.empty; 
              p_cards = PartialDeck.empty;
              score = 0;
            }) players';
        is_over = true;
        next_player = 0;
        first_round = true;
        hearts_broken = false;
        round_number = t.round_number + 1;
        history = ListQueue.push new_history t.history;
      } |> update_action
    else
      {
        t with
        pile = [];
        players = players';
        next_player = snd winner;
        first_round = false;
        hearts_broken = hearts_broken';
        history = ListQueue.push new_history t.history;
      } |> update_action

  let cards_passed_string c_ll order = 
    let cards = (List.map2 (fun a b -> (a,b)) order c_ll) |> List.assoc 0 in 
    List.fold_left (fun acc card -> acc ^ (card_to_string card) ^ ", ") "" cards

  let get_difficulty t = 
    match t.difficulty with
    | Easy -> "easy"
    | Medium -> "medium"
    | Hard -> "hard"
    | Invalid -> "easy"

  let get_passing_order t = 
    match t.round_number with 
    | n when (n mod 4) = 1 -> [1;2;3;0]
    | n when (n mod 4) = 2 -> [3;2;1;0]
    | n when (n mod 4) = 3 -> [2;3;0;1]
    | n -> failwith "should not be passing on forth rounds"

  let rec bot_actions t = 
    match t.next_action,t.next_player with 
    | (_,0) -> t
    | (Play,id) -> 
      internal_play t.next_player 
        (Bot.play (List.nth t.players id).hand t.pile (get_difficulty t)) t
    | (Lead,id) -> 
      internal_lead t.next_player 
        (Bot.lead (List.nth t.players id).hand t.pile (get_difficulty t)) t
    | (Pass,_) -> t
    | (Deal,_) -> failwith "uni"
  and 
    internal_play id card t =
    check_play_in_turn id card t; 
    check_in_hand id card t;
    check_voided id card t;
    check_play_first_round id card t; 
    let t' = add_to_pile id card t in
    if List.length t'.pile >= 4
    then clean_up_trick t' |> bot_actions 
    else increment_actions_play t' |> bot_actions
  and 
    internal_lead id card t = 
    check_lead_in_turn id card t;
    check_lead_hearts_broken id card t;
    check_lead_first_round id card t;
    check_in_hand id card t;
    let t' = add_to_pile id card t in
    increment_actions_play t' |> bot_actions
  and internal_deal t = 
    let players' = deal_helper t.players PartialDeck.full in
    let new_history = {
      hands = List.map (fun player -> player.hand) players';
      pile = t.pile;
      round_score = List.map (fun player -> player.score) players';
      description = "Cards were dealt."
    } in
    {
      t with
      players = players';
      history = ListQueue.push new_history t.history;
      next_player = 0;
    } |> update_action |> bot_actions
  and internal_pass id card_l t = 
    check_pass_in_turn id card_l t;
    let get_bot_pass id = Bot.pass (get_hand id t) in
    let rec remove_cards c_ll inc (st:t) = 
      match c_ll with 
      | [] -> st
      | h::t -> 
        let new_hand = PartialDeck.remove_cards h (get_hand inc st) in 
        remove_cards t (inc + 1) (set_hand inc new_hand st)
    in
    let rec add_cards c_ll order (st:t) = 
      match c_ll, order with 
      | [],_ -> st
      | h::t, h1::t1 -> 
        let new_hand = PartialDeck.add_cards h (get_hand h1 st) in 
        add_cards t (t1) (set_hand h1 new_hand st)
      | _,_ -> failwith "order not long enough"
    in
    let pass_cards = [card_l; get_bot_pass 1; get_bot_pass 2; get_bot_pass 3] in
    let t' = t |> (remove_cards pass_cards 0 ) |> (add_cards pass_cards (get_passing_order t)) in
    let new_history = 
      {
        hands = List.map (fun player -> player.hand) t'.players;
        pile = t.pile;
        round_score = List.map (fun player -> player.score) t'.players;
        description = "You were passed:" ^ (cards_passed_string pass_cards (get_passing_order t));
      }
    in
    {
      t' with
      next_player = player_with_leading_card t'.players;
      history = ListQueue.push new_history t.history;
    } |> update_action |> bot_actions

  let deal t = 
    match internal_deal t with 
    | exception Default s -> Invalid s
    | v -> Valid v

  let play card t =
    match t.next_action with 
    | Lead ->
      begin
        match internal_lead 0 card t with 
        | exception Default(s) -> Invalid s
        | exception InvalidCardPlayed -> Invalid "Can't play bad card first round"
        | t -> Valid t
      end
    | Play ->
      begin
        match internal_play 0 card t with 
        | exception Default(s) -> Invalid s
        | exception InvalidCardPlayed -> Invalid "Can't play bad card first round"
        | t -> Valid t
      end
    | _ -> failwith "error"

  let pass card_l t = 
    match internal_pass 0 card_l t with 
    | exception Default(s) -> Invalid s
    | t -> Valid t


  let get_round_ t = 
    List.map (fun p -> p.name, p.score) t.players

  let next t = 
    {
      t with 
      history = ListQueue.pop t.history;
    }

  let next_action t = 
    match t.next_action with 
    | Lead -> "Lead"
    | Play -> "Play"
    | Pass -> "Pass"
    | Deal -> "Deal"

  let hand t = 
    List.nth (ListQueue.peek t.history).hands 0

  let total_score t = 
    t.total_scores

  let round_score t = 
    (ListQueue.peek t.history).round_score

  let pile t = 
    (ListQueue.peek t.history).pile

  let description t = 
    (ListQueue.peek t.history).description

  let is_next t =
    ListQueue.size t.history > 1

  let bot_hand t id =
    (List.nth t.players id).hand

  let bot_pile t = 
    t.pile

  let names t = 
    List.map (fun player -> player.name) t.players


  (* ----TESTING FUNCTIONS---- *)

  let string_of_player p = 
    "<Player :" ^
    "; Name = " ^ p.name ^
    "; ID = " ^ string_of_int p.id ^
    "; Score = " ^ string_of_int p.score ^
    "; P_Cards = " ^ PartialDeck.string_of_partialdeck p.p_cards ^
    "; Hand = " ^ PartialDeck.string_of_partialdeck p.hand ^ ">"

  let rec string_of_players p_list = 
    match p_list with 
    | h::t -> string_of_player h ^ "\n" ^ string_of_players t
    | [] -> ""

  let rec string_of_pile pile = 
    match pile with 
    | h::t -> "(" ^ (card_to_string (fst h)) ^ "," ^  string_of_int (snd h) ^ "), "
    | [] -> ""

  let rec string_of_int_list l = 
    match l with 
    | h::t -> string_of_int h ^ ", "
    | [] -> ""

  let string_of_action a = 
    match a with 
    | Deal -> "Deal"
    | Pass -> "Pass"
    | Lead -> "Play"
    | Play -> "Play"

  let string_of_round r = 
    "<Round :" ^
    "; Players = " ^ string_of_players r.players ^
    "; Pile = " ^ string_of_pile r.pile ^ 
    "; is_over = " ^ string_of_bool r.is_over ^ 
    "; hearts_broken = " ^ string_of_bool r.hearts_broken ^ 
    "; first_round = " ^ string_of_bool r.first_round ^ 
    "; next_player = " ^ string_of_int r.next_player ^ 
    "; total_scores = " ^ string_of_int_list r.total_scores ^
    "; next_action = " ^ string_of_action r.next_action ^ 
    ">"
end