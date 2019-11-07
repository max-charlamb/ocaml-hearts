open Partialdeck
open Card

module type BotSig = sig 

  val play : PartialDeck.t -> (card * int) list -> card
  val lead : PartialDeck.t -> (card * int) list -> card

end

module Bot:BotSig