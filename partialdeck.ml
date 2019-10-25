
module type PartialDeckSig = sig

  exception DuplicateCard

  exception CardNotFound

  type suite = 
    | Spade 
    | Heart 
    | Club 
    | Diamond

  type rank = 
    | One 
    | Two 
    | Three 
    | Four 
    | Five 
    | Six 
    | Seven 
    | Eight 
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

  type card = Card of suite * rank

  type t

  val empty : t 

  val full : t

  val insert : card -> t -> t 

  val remove : card -> t -> t

  val move : card -> t -> t -> (t * t)

  val mem : card -> t -> bool

  val size: t -> int

  val is_empty: t -> bool

  val random_card: t -> card option

  val to_list: t -> card list


end

module PartialDeck = struct

  exception DuplicateCard

  exception CardNotFound

  type suite = 
    | Spade 
    | Heart 
    | Club 
    | Diamond

  type rank =  
    | Two 
    | Three 
    | Four 
    | Five 
    | Six 
    | Seven 
    | Eight 
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

  type card = Card of suite * rank

  type t = card list

  let rank_to_int = function 
    | Two -> 2 | Three -> 3 | Four -> 4 | Five -> 5 | Six -> 6
    | Seven -> 7 | Eight -> 8 | Nine -> 9 | Ten -> 10 
    | Jack -> 11 | Queen -> 12  | King -> 13 | Ace -> 14

  let int_to_rank = function 
    | 2 -> Two | 3 -> Three | 4 -> Four | 5 -> Five | 6 -> Six
    | 7 -> Seven | 8 -> Eight | 9 -> Nine | 10 -> Ten 
    | 11 -> Jack | 12 -> Queen  | 13 -> King | 14 -> Ace
    | _ -> failwith "not allowed value"

  let empty = []

  let insert card t = 
    card :: t

  let full = 
    let rec aux rank suite deck = 
      if rank > 1 then
        aux (rank - 1) suite (insert (Card(suite, int_to_rank rank)) deck)
      else deck
    in
    aux 14 Spade empty |> aux 14 Heart |> aux 14 Diamond |> aux 14 Club

  let to_list t =
    t



end