open Card

(*
 A partial deck is a set of any number of cards. It can be created empty
or as a full deck.
*)

module type PartialDeckSig = sig

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

  (** [mem card t] is true if [card] is in [t], otherwise false. *)
  val mem : card -> t -> bool

  (** [size t] is the number of cards in [t]. *)
  val size: t -> int

  (** [is_empty t] is true if [t] contains zero cards. Otherwise false. *)
  val is_empty: t -> bool

  (** [random_card t] is a random card option in [t].
      None if [t] is empty. *)
  val random_card: t -> card option

  (** [find n t] is the [n]th card of [t]. 
      None if the list is not long enough. *)
  val find: int -> t -> card option

  (** [to_list t] is a list of the cards in [t]. *)
  val to_list: t -> card list


end

module PartialDeck:PartialDeckSig