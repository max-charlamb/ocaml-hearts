(** Round holds the state of our game and functions to 
    update it. *)

open Card
open Partialdeck

module type RoundSig = sig

  (** [t] is the type of a round. *)
  type t

  (** [result] is the result of making a move on a round. Either
      valid or invalid. *)
  type result = Valid of t | Invalid of string

  (** [difficulty] are the possible levels of difficulty for a round. *)
  type difficulty = Easy | Medium | Hard | Invalid

  (** [new_round d] is a new game of difficulty [d]. *)
  val new_round : difficulty -> string -> t

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

  (** [round_score t] is a list of all the players round score. 
      In order of their id. *)
  val round_score : t -> int list

  (** [total_score t] is a list of all the players cumulative score. 
      In order of their id. *)
  val total_score : t -> int list 

  (** [names t] is the names of the players in id order.*)
  val names : t -> string list

  val string_of_round : t -> string

  (** [next_action] is the string of the next action. *)
  val next_action : t -> string

  (** [test_deal pdl] is a new round where [pdl] are dealt as each hand.
      The length of [pdl] must be 4. *)
  val test_deal : PartialDeck.t list -> t

  (** [game_over t] is [(false,_)] if no players score is above the
      winning condition score. [(true,_)] if there is a player 
      above the score. [(true,true)] if the player won. 
      [(ture,false)] if the bot won. *)
  val game_over : t -> bool * bool

end

module Round:RoundSig


