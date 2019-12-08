(** A partial deck is a set of any number of cards. It can be created empty
    or as a full deck. *)
open Card

module type PartialDeckSig = sig

  (** [t] is the type of a PartialDeck. *)
  type t

  (** [empty] is an empty partial deck. *)
  val empty : t 

  (** [full] is a partial deck filled with all the cards in a standard deck. *)
  val full : t

  (** [insert card t] is deck [t] with [card] inserted. 
      Raises DuplicateCard if card was already in the deck. *)
  val insert : card -> t -> t 

  (** [remove card t] is a partial deck [t] without [card]. 
      Raises CardNotFound if [card] is not in [t]. *)
  val remove : card -> t -> t

  (** [move card t1 t2] is a tuple of ([t1],[t2]) where [card] is moved 
      from [t1] to [t2]. 
      Raises CardNotFound if [card] is not in [t1].
      Raises DuplicateCare if [card] is already in [t2]. *)
  val move : card -> t -> t -> (t * t)

  (** [move i t1 t2] is a tuple of ([t1],[t2]) where the card at index [i] 
      is moved from [t1] to [t2].
      Raises CardNotFound if [card] is not in [t1].
      Raises DuplicateCare if [card] is already in [t2]. *)
  val move_at_index : int -> t -> t -> (t * t)

  (** [mem card t] is true if [card] is in [t], otherwise false. *)
  val mem : card -> t -> bool

  (** [size t] is the number of cards in [t]. *)
  val size: t -> int

  (** [is_empty t] is true if [t] contains zero cards. Otherwise false. *)
  val is_empty: t -> bool

  (** [random_card t] is a random card option in [t].
      None if [t] is empty. *)
  val random_card: t -> card option

  (** [to_list t] is a list of the cards in [t] in a tuple with their index. *)
  val to_list: t -> (card * int) list

  (** [find i t] is the [i]th Some card, 
      if the [i]th card does not exist, None.*)
  val find: int -> t -> card option

  (** [voided suite t] is true if there are none of suite in [t]. *)
  val voided: suite -> t -> bool

  (** [count_points t] is the number of points in partial deck [t]. *)
  val count_points: t -> int

  (** [merge t1 t2] is the partial deck containing all the cards in t1 and t2.*)
  val merge: t -> t -> t

  (** [contains_hearts t] is true if [t] contains any hearts. Otherwise false.*)
  val contains_hearts: t -> bool

  (** [string_of_partialdeck t] is a string representing the deck [t]. *)
  val string_of_partialdeck: t -> string

  (** [lowest t suite] is the lowest card in [t] of [suite]. *)
  val lowest: t -> suite -> card

  (** [highest t suite] is the highest card in [t] of [suite]. *)
  val highest: t -> suite -> card

  (** [remove_cards c_l t] is t without any cards from [c_l]. *)
  val remove_cards : card list -> t -> t

  (** [add_cards c_l t] is t with all the cards from [c_l]. *)
  val add_cards : card list -> t -> t

  (** [shoot_the_moon t] is true if [t] has all the cards needed to shoot
      the moon. Otherwise false. *)
  val shoot_the_moon : t -> bool

  (** [only_bad t] is true if [t] contains only cards that give 
      points. Otherwise false. *)
  val only_bad : t -> bool 

end

module PartialDeck:PartialDeckSig