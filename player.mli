(* *)

module type PlayerSig = sig 

  type t 

  (** [create n] is a player with name [n]. *)
  val create : string -> t

  (** [add_card card player] is the [player] with the [card] added to its 
      hand. *)
  val add_card : Card.card -> t -> t

  (** [add_penalty_card card player] is the [player] with the [card] added to 
      its penalty hand. *)
  val add_penalty_card : Card.card -> t -> t

  (** [play_card player index] is a card from the hand of [player]. *)
  val play_card : t -> int -> Card.card 


  (** [pass_cards player inds] is a triple of cards that [player] has 
      elected to pass. *)
  val pass_cards : t -> (int * int *int) -> (Card.card * Card.card * Card.card)

end

module Player : PlayerSig


