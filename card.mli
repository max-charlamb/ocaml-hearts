

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

type card = {
  suite:suite;
  rank:rank;
}


val suite_to_int : suite -> int

val rank_to_int : rank -> int

val int_to_rank : int -> rank

val compare: card -> card -> int