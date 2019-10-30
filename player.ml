open Partialdeck
open Card

module type PlayerSig = sig 
  type t 
  val create : string -> t
  val add_card : Card.card -> t -> t
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




end