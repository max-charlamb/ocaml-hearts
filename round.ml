open Card
open Partialdeck
open Command

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

  type player = {
    name : string;
    hand : PartialDeck.t;
    score : int;
    p_cards : PartialDeck.t;
  }

  type t = {
    players : player list;
    pile : (card * player) list;
    is_over : bool;
  }

  type result = Valid of t | Invalid of string

  let new_round () = {
    players = [Player.create "Henry"; 
               Player.create "Bot1"; 
               Player.create "Bot2"; 
               Player.create "Bot3"];
    pile = [];
    is_over = false
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

  let play card t = 
    let user = List.nth t.players 0 in 
    if List.



         let pass c = failwith "uni"

end