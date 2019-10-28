open Card
open Partialdeck

module type GameSig = sig
  type pDeck
  type player 
  type t
  type result
  val init : t
  val deal : t -> pDeck -> t
  val play : int -> player -> t -> result
  val pass : card list -> t
end

module Game: GameSig = struct

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

  let deal_helper = ""

  let rec deal t d = 
    match (PartialDeck.random_card d) with
    | None -> t
    | Some c -> t

  let play n p g = 
    match PartialDeck.find n p.hand with 
    | None -> Invalid
    | Some c -> Valid {
        players = g.players;
        pile = (c,p) :: g.pile;
        is_over = g.is_over;
      }

  let pass c = failwith "unImplemented"

end