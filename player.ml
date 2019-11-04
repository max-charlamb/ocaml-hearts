open Partialdeck
open Card

module type PlayerSig = sig 

  type t 
  val create : string -> t
  val add_card : Card.card -> t -> t
  val add_penalty_card : Card.card -> t -> t
  val play_card : t -> Card.card 
  val pass_cards : t -> (Card.card * Card.card * Card.card)


end

module Player : PlayerSig = struct

  type t = {
    name : string;
    hand : PartialDeck.t;
    score : int;
    penalty_cards : PartialDeck.t
  }

  let create n = 
    {
      name = n;
      hand = PartialDeck.empty;
      score = 0;
      penalty_cards = PartialDeck.empty
    }

  let add_card card player = 
    {
      player with 
      hand = PartialDeck.insert card player.hand
    }

  let add_penalty_card card player = 
    {
      player with 
      penalty_cards = PartialDeck.insert card player.penalty_cards
    }

  let play_card player = 
    failwith "unimplemented"

  let pass_cards player = 
    failwith "unimplemented"

end