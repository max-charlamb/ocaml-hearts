(* *)
module type PlayerSig = sig 

  type t 

  val create : string -> t

  val add_card : Card.card -> t -> t

end

module Player : PlayerSig


