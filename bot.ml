open Partialdeck
open Card

module Bot : Player.PlayerSig = struct

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

  let play_card player index = 
    match PartialDeck.random_card player.hand with 
    | None -> failwith ""
    | Some x -> x

  let pass_cards player index = 
    failwith "unimplemented"

end