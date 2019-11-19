open Partialdeck
open Card

module type BotSig = sig 

  val play : PartialDeck.t -> (card * int) list -> string -> card
  val lead : PartialDeck.t -> (card * int) list -> string -> card
  val pass : PartialDeck.t -> (card * int) list -> card list

end

module Bot:BotSig