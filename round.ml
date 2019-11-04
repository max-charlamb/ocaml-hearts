open Card
open Partialdeck

module type RoundSig = sig
  type pDeck
  type player 
  type t
  type result
  val init : t
  val add_player : t -> t
  val deal : t -> pDeck -> t
  val play : int -> player -> t -> result
  val pass : card list -> t
end


module Round:RoundSig = struct

  type pDeck = PartialDeck.t

  type player = {
    hand : pDeck;
    score : int;
    penalty_cards : pDeck;
  }

  type t = {
    players : player list;
    pile : (card * player) list;
    is_over : bool;
  }

  type result = Valid of t | Invalid

  let init = {
    players = [];
    pile = [];
    is_over = false
  }


  let add_player t =
    let p = {
      hand = PartialDeck.empty;
      score =  0;
      penalty_cards = PartialDeck.empty;
    } in 
    {
      players = p :: t.players;
      pile = t.pile;
      is_over = t.is_over;
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

  let deal t d = 
    {
      players = deal_helper t.players d ;
      pile = t.pile;
      is_over = t.is_over;
    }

  let player_helper = ""

  let play n p g = Invalid
  (* match PartialDeck.find n p.hand with 
     | None -> Invalid
     | Some c -> 
     let new_hand = PartialDeck.remove c p.hand in
     Valid {
      players = g.players;
      pile = (c,p) :: g.pile;
      is_over = g.is_over;
     } *)

  let pass c = failwith"uni"

end