(** Card is the representation of a single card in our game. *)

(** [DeuplicateCard] raised when Duplicate card is found. *)
exception DuplicateCard

(** [CardNotFound] raised when card can not be found in a deck. *)
exception CardNotFound

(** [suite] are the possible suites for a card. *)
type suite = 
  | Spade 
  | Heart 
  | Club 
  | Diamond

(** [rank] are the possible ranks of a card. *)
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

(** [card] is a single card. *)
type card = {
  suite:suite;
  rank:rank;
}

(** [suite_to_int s] is the integer representation of [s] *)
val suite_to_int : suite -> int

(** [rank_to_int r] is the integer representation of [r]*)
val rank_to_int : rank -> int

(** [int_to_rank i] is the rank represented by [i]. *)
val int_to_rank : int -> rank

(** [suite_to_string s] is the string representing suite [s]. *)
val suite_to_string: suite -> string

(** [rank_to_string r] is the string representing rank [r]. *)
val rank_to_string: rank -> string

(** [card_to_string c] is the string representation of card [c]. *)
val card_to_string: card -> string

(** [print_card ?backgnd c] prints the string representation of card [c]
    using ANSITerminal.style [?backgnd]. *)
val print_card: ?bckgnd:ANSITerminal.style -> card -> unit

(** [print_card_tall c] prints a larger version of the card [c]. *)
val print_card_tall : card -> unit

(** [compare c1 c2] is positive if [c1 > c2], 0 if [c1 = c2], and 
    raises exception if [c1] is [c2]. *)
val compare: card -> card -> int