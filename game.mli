open Card
open Partialdeck


module type GameSig = sig
  type pDeck
  type player 
  type t
  type result
  (* [init] is a new game. *)
  val init : t

  (* [deal] deals the cards from a partial deck to all the players. *)
  val deal : t -> pDeck -> t

  (*[play c p] plays the card [c] that player [p] chooses to play.*)
  val play : int -> player -> t -> result

  (*[pass c p] plays the card [c] that player [p] chooses to play.*)
  val pass : card list -> t

end