open Card
open Partialdeck
open Command
open Bot
open ListQueue



module type RoundSig = sig
  type t
  type result = Valid of t | Invalid of string
  val new_round : t
  val deal : t -> t
  val play : card -> t -> result
  val pass : card list -> t -> result
  val hand : t -> int -> PartialDeck.t
  val pile : t -> (card * int) list
  val next : t -> t
  val description : t -> string
  val is_next : t -> bool
  val bot_hand : t -> int -> PartialDeck.t
  val bot_pile : t -> (card * int) list
  val get_scores : t -> (string * int) list
end


module Round:RoundSig = struct

  exception Default of string
  exception InvalidCardPlayed
  exception BotError

  type action = Play | Lead | Pass

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
    history : historySegment ListQueue.t;
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

  let new_round = {
    players = [create_player "Henry" 0; 
               create_player "Bot1" 1; 
               create_player "Bot2" 2; 
               create_player "Bot3" 3];
    pile = [];
    is_over = false;
    hearts_broken = false;
    first_round = true;
    next_player = 0;
    next_action = Lead;
    history = ListQueue.empty;
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


  let rec deal_helper players d = 
    match deal_round players d with
    | d', p' -> if PartialDeck.is_empty d' then p' else deal_helper p' d'

  let deal t = 
    let players' = deal_helper t.players PartialDeck.full in
    let new_history = {
      hands = List.map (fun player -> player.hand) players';
      pile = t.pile;
      description = "Cards were dealt."
    } in
    {
      t with
      players = players';
      history = ListQueue.push new_history t.history;
    }
  (** [hand_size t] is the hand size of all the players. Can only be
      called between tricks. If [debug] then fails if not all equal. *)
  let hand_size t = 
    let hand_size = PartialDeck.size (List.hd t.players).hand in
    if debug then 
      if List.for_all 
          (fun player -> PartialDeck.size player.hand = hand_size) t.players
      then hand_size else failwith "player hand sizes are not equal"
    else hand_size

  let id_to_name id t = 
    (List.nth t.players id).name

  let check_voided id card t =
    let user = List.nth t.players id in 
    let leading_suite = (List.hd t.pile |> fst).suite in
    if card.suite <> leading_suite && 
       not (PartialDeck.voided leading_suite user.hand) then 
      raise (Default "problem in check_voided")

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
      raise (Default "problem in check_lead_in_turn") else ()

  let check_lead_first_round id card t =
    if t.first_round && card <> {suite=Club;rank=Two} then 
      raise (Default "problem in check_lead_first_round") else ()

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
      description = (id_to_name id t) ^ " played the " 
                    ^ card_to_string card ^ "."
    } in
    {
      t with 
      pile = pile';
      players = players';
      history = ListQueue.push new_history t.history;
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
    if hand_size t = 0 then 
      t 
      (* TODO: implement what to do when hand size is over*)
    else
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
          (fun player -> 
             if player.id = snd winner then 
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
        description = (id_to_name (snd winner) t) ^ " won the trick with a " 
                      ^ card_to_string (fst winner) ^ ".";
      }
      in
      {
        t with
        pile = [];
        players = players';
        next_action = Lead;
        next_player = snd winner;
        first_round = false;
        hearts_broken = hearts_broken';
        history = ListQueue.push new_history t.history;
      }

  let rec bot_actions t = 
    match t.next_action,t.next_player with 
    | (_,0) -> t
    | (Play,id) -> 
      internal_play t.next_player 
        (Bot.play (List.nth t.players id).hand t.pile) t
    | (Lead,id) -> 
      internal_lead t.next_player 
        (Bot.lead (List.nth t.players id).hand t.pile) t
    | (Pass,_) -> t
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
    (* check_lead_first_round id card t; *)
    check_in_hand id card t;
    let t' = add_to_pile id card t in
    increment_actions_play t' |> bot_actions


  let play card t =
    if List.length t.pile > 0 then 
      match internal_play 0 card t with 
      | exception Default(s) -> Invalid s
      | exception InvalidCardPlayed -> Invalid "Can't play bad card first round"
      | t -> Valid t
    else 
      match internal_lead 0 card t with 
      | exception Default(s) -> Invalid s
      | exception InvalidCardPlayed -> Invalid "Can't play bad card first round"
      | t -> Valid t

  let get_scores t = 
    List.map (fun p -> p.name, p.score) t.players

  let pass c = failwith "uni"

  let next t = 
    {
      t with 
      history = ListQueue.pop t.history;
    }

  let hand t id = 
    List.nth (ListQueue.peek t.history).hands id

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

end