open Card
open Partialdeck



module type RoundSig = sig
  type pDeck
  type player 
  type t
  type result

  (** [new_round ()] is a new game. *)
  val new_round : unit -> t

  (* [deal t] deals the cards from a partial deck to all the players. *)
  val deal : t -> result

  (*[play c p] plays the card [c] that player [p] chooses to play.*)
  val play : card -> player -> t -> result

  (*[pass c p] plays the card [c] that player [p] chooses to play.*)
  val pass : card list -> player -> t

  val next_step : t -> result

end

module Round:RoundSig


