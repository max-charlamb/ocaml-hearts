(** Bot is the computer players of the game. *)
open Partialdeck
open Card

module type BotSig = sig 

  (** [play hand pile diff qs_played] is the card played by the bot from [hand]
      with current [pile] on the table corresponding to difficulty [diff]. *)
  val play : PartialDeck.t -> (card * int) list -> string -> bool -> card

  (** [lead hand pile diff] is the leading card played by the bot from [hand]
      corresponding to difficulty [diff]. *)
  val lead : PartialDeck.t -> (card * int) list -> string -> card

  (** [pass hand diff] is a list of three cards passed by the bot from its
      [hand] corresponding to difficulty level [diff]. *)
  val pass : PartialDeck.t -> string -> card list

end

module Bot:BotSig