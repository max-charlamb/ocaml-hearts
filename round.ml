open Card
open Partialdeck
open Command
open Bot

module type RoundSig = sig
  type player 
  type t
  type result
  val new_round : unit -> t
  val add_player : t -> t
  val deal : t -> t
  val play : command -> t -> result
  val pass : command -> t -> result
end



module Round = struct

  exception Default
  exception InvalidCardPlayed

  type player = {
    name : int;
    hand : PartialDeck.t;
    score : int;
    p_cards : PartialDeck.t;
  }

  type action = Play | Lead | Pass

  type t = {
    players : player list;
    pile : (card * int) list;
    is_over : bool;
    hearts_played: bool;
    first_round: bool;
    next_player: int;
    next_action: action;
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
    hearts_played = false;
    first_round = true;
    next_player = 0;
    next_action = Lead;
  }


  (* [insert_hand p c] is player [p] with the card [c] inserted into their hand.*)
  let insert_hand (p:player) c = 
    match PartialDeck.insert c p.hand with
    | exception (DuplicateCard) -> p
    | h -> {
        hand = h;
        score = p.score;
        penalty_cards = p.penalty_cards;
      }

  (* [deal_ round p d] deals one card from [d] to all the players [p].*)
  let rec deal_round p d = 
    match (PartialDeck.random_card d, p) with
    | None, _ -> p, d
    | Some c, h::t -> 
      (match PartialDeck.move c d h.hand with 
       | (d' , h') -> 
         let new_player = {
           hand = h';
           score = h.score;
           penalty_cards = h.penalty_cards;
         } in
         new_player :: (deal_round t d' |> fst), 
         deal_round t d' |> snd)
    | Some c, [] -> 
      let d' = PartialDeck.remove c d in 
      p , d' 

  (* [deal_ helper p d] deals all the cards from [d] to the players [p]. *)
  let rec deal_helper p d = 
    match deal_round p d with
    | (p', d') -> if PartialDeck.is_empty d' then p else deal_helper p' d'

  let deal t = 
    {
      players = deal_helper t.players PartialDeck.full ;
      pile = t.pile;
      is_over = t.is_over;
    }

  let player_helper = ""


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

  let check_first_round num card t = 
    if t.first_round then 
      match card with
      | {suite=Spade; rank=Queen}
      | {suite=Heart} -> raise InvalidCardPlayed
      | _ -> ()

  let add_to_pile num card t = 
    let players' = List.map 
        (fun player -> if player.id = num then 
            {
              player with
              hand = PartialDeck.remove card player.hand
            } else player) t.players in
    let pile' = (card, num)::t.pile in
    {
      t with 
      pile = pile';
      players = players';
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
    let p = List.fold_left 
        (fun p (c,_) -> PartialDeck.insert c p) 
        PartialDeck.empty t.pile in
    let players' = List.map 
        (fun player -> if player.name = winner_name
          then 
            {
              player with 
              score = player.score + (PartialDeck.count_points p)
            } 
          else player) 
    in
    {
      t with
      pile = [];
      players = players';
      next_action = Lead;
      next_player = winner_name;
    }


  let rec bot_actions t = 
    match t.next_action,t.next_player with 
    | (_,0) -> t
    | (Play,_) -> internal_play t.next_player (Bot.play t) t
    | (Lead,_) -> t
    | (Pass,_) -> t
  and 
    internal_play num card t =
    check_play_in_turn num card t; 
    check_in_hand num card t;
    check_voided num card t;
    check_first_round num card t;
    let t' = add_to_pile num card t in
    if List.length t'.pile >= 4
    then clean_up_trick t' |> bot_actions 
    else increment_actions_play t' |> bot_actions

  let play card t =
    match internal_play 0 card t with 
    | exception Default -> Invalid "somethign went wrong" 
    | exception InvalidCardPlayed -> Invalid "Can't play bad card first round"
    | t -> Valid t




  let lead card t = failwith "uni"



  let pass c = failwith "uni"

end