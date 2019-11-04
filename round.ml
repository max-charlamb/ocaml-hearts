open Card
open Partialdeck
open Command
open Bot
open ListQueue

module type RoundSig = sig
  type player 
  type t
  type result
  val new_round : unit -> t
  val deal : t -> t
  val play : command -> t -> result
  val pass : command -> t -> result
  val hand : t -> int -> PartialDeck.t
  val pile : t -> (card * int) list
  val next : t -> t
end



module Round = struct

  exception Default
  exception InvalidCardPlayed

  type action = Play | Lead | Pass

  type player = {
    name : int;
    hand : PartialDeck.t;
    score : int;
    p_cards : PartialDeck.t;
  }

  type historySegment = {
    hands : PartialDeck.t list;
    pile : (card * int) list;
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


  let create_player name = 
    {
      name = name;
      hand = PartialDeck.empty;
      score = 0;
      p_cards = PartialDeck.empty;
    }

  let new_round () = {
    players = [create_player 0; 
               create_player 1; 
               create_player 2; 
               create_player 3];
    pile = [];
    is_over = false;
    hearts_broken = false;
    first_round = true;
    next_player = 0;
    next_action = Lead;
    history = ListQueue.empty;
  }

  let deal t = 
    failwith "unimplemented"

  let check_voided num card t =
    let user = List.nth t.players num in 
    let leading_suite = (List.hd t.pile |> fst).suite in
    if card.suite <> leading_suite && 
       not (PartialDeck.voided leading_suite user.hand) then 
      raise Default

  let check_in_hand num card t =
    let user = List.nth t.players num in 
    if PartialDeck.mem card user.hand then
      raise Default

  let check_play_in_turn num card t = 
    if t.next_player <> num || t.next_action <> Play then 
      raise Default

  let check_play_first_round num card t = 
    if t.first_round then 
      match card with
      | {suite=Spade; rank=Queen}
      | {suite=Heart} -> raise InvalidCardPlayed
      | _ -> ()

  let check_lead_in_turn num card t = 
    if t.next_action <> Lead || t.next_player <> num then 
      raise Default

  let check_lead_first_round num card t =
    if t.first_round && card <> {suite=Club;rank=Two} then 
      raise Default

  let add_to_pile num card t = 
    let pile' = (card, num)::t.pile in
    let players' = List.map 
        (fun player -> if player.name = num then 
            {
              player with
              hand = PartialDeck.remove card player.hand
            } else player) t.players in
    let new_history = {
      hands = List.map (fun player -> player.hand) players';
      pile = pile';
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
      {t with next_player=0}
    | _ when List.length t.pile < 4 ->  
      {t with next_player=(t.next_player + 1)}
    | _ -> 
      raise Default

  let clean_up_trick t = 
    let leading_suite = (List.hd t.pile |> fst).suite in
    let winner_name = t.pile |> List.filter (fun (c,_) -> c.suite = leading_suite) 
                      |> List.sort (fun (c1,_) (c2,_) -> compare c1 c2) 
                      |> List.rev |> List.hd |> snd in
    let pile_partialdeck = List.fold_left 
        (fun p (c,_) -> PartialDeck.insert c p) 
        PartialDeck.empty t.pile in
    let hearts_broken' = t.hearts_broken || 
                         PartialDeck.contains_hearts pile_partialdeck in 
    let players' = 
      List.map 
        (fun player -> 
           if player.name = winner_name then 
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
    }
    in
    {
      t with
      pile = [];
      players = players';
      next_action = Lead;
      next_player = winner_name;
      first_round = false;
      hearts_broken = hearts_broken';
      history = ListQueue.push new_history t.history;
    }

  let rec bot_actions t = 
    match t.next_action,t.next_player with 
    | (_,0) -> t
    | (Play,_) -> internal_play t.next_player (Bot.play t) t
    | (Lead,_) -> internal_play t.next_player (Bot.lead t) t
    | (Pass,_) -> t
  and 
    internal_play num card t =
    check_play_in_turn num card t; 
    check_in_hand num card t;
    check_voided num card t;
    check_play_first_round num card t;
    let t' = add_to_pile num card t in
    if List.length t'.pile >= 4
    then clean_up_trick t' |> bot_actions 
    else increment_actions_play t' |> bot_actions
  and 
    internal_lead num card t = 
    check_lead_in_turn num card t;
    check_lead_first_round num card t;
    check_in_hand num card t;
    let t' = add_to_pile num card t in
    if List.length t'.pile >= 4
    then clean_up_trick t' |> bot_actions 
    else increment_actions_play t' |> bot_actions


  let play card t =
    if List.length t.pile > 0 then 
      match internal_play 0 card t with 
      | exception Default -> Invalid "somethign went wrong" 
      | exception InvalidCardPlayed -> Invalid "Can't play bad card first round"
      | t -> Valid t
    else 
      match internal_lead 0 card t with 
      | exception Default -> Invalid "somethign went wrong" 
      | exception InvalidCardPlayed -> Invalid "Can't play bad card first round"
      | t -> Valid t


  let pass c = failwith "uni"

  let next t = 
    {
      t with 
      history = ListQueue.pop t.history;
    }

  let hand num t = 
    let segment =  ListQueue.peek t.history in
    List.nth segment.hands num

  let pile  t = 
    (ListQueue.peek t.history).pile

end