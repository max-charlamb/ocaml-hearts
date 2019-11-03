(* *)

module type PlayerSig = sig 

  type t 

  (** [create n] is a player with name [n]. *)
  val create : string -> t

  (** [add_card card player] is the [player] with the [card] added to its 
      hand. *)
  val add_card : Card.card -> t -> t

  val add_penalty_card : Card.card -> t -> t

  val play_card : t -> (Card.card * t)

  val pass_cards : t -> (Card.card * Card.card * Card.card)

end

module Player : PlayerSig


