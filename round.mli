open Card
open Partialdeck


module type RoundSig = sig
  type t
  type result = Valid of t | Invalid of string

  (** [new_round] is a new game. *)
  val new_round : t

  (* [deal t] deals the cards from a partial deck to all the players. *)
  val deal : t -> t

  (*[play c t] plays the card [c].*)
  val play : card -> t -> result

  (*[pass c t] passes the cards [c].*)
  val pass : card list -> t -> result

  (** [hand t i] is the [i]th players hand at the current state. *)
  val hand : t -> int -> PartialDeck.t

  (** [pile t] is the pile at the current state. *)
  val pile : t -> (card * int) list

  (** [next t] is [t] with the state incremented. *)
  val next : t -> t

  (** [description t] is the description for the current state. *)
  val description : t -> string

  (** [is_next t] is true if there is a next state, otherwise false. *)
  val is_next : t -> bool

  (** [bot_hand t id] is bot [id]'s current hand. 
      Meant only for use with bots. *)
  val bot_hand : t -> int -> PartialDeck.t

  (** [bot_pile t] is the current pile. 
      Meant only for use with bots. *)
  val bot_pile : t -> (card * int) list

end

module Round:RoundSig


