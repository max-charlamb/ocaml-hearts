open Card
open Partialdeck

module type RoundSig = sig
  type t
  type result = Valid of t | Invalid of string
  type difficulty = Easy | Medium | Hard | Invalid

  (** [new_round d] is a new game of difficulty [d]. *)
  val new_round : difficulty -> t

  (* [deal t] deals the cards from a partial deck to all the players. *)
  val deal : t -> result

  (*[play c t] plays the card [c].*)
  val play : card -> t -> result

  (*[pass c t] passes the cards [c].*)
  val pass : card list -> t -> result

  (** [hand t] is the players hand at the current state. *)
  val hand : t -> PartialDeck.t

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

  (** [score t] is the players current score. *)
  val score : t -> int 

  (** [end_of_round_score t] is a list of all the players score. 
      In order of their id. *)
  val end_of_round_score : t -> int list 

  (** [names t] is the names of the players in id order.*)
  val names : t -> string list

  val string_of_round : t -> string

  (** [next_action] is the string of the next action. *)
  val next_action : t -> string

end

module Round:RoundSig


