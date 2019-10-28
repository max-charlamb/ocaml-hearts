(** Parses user commands *)

(** The type [index] represents the index of a card in a player's hand. *)
type index = int

(** The type [card_triple] represents the index of three unique cards in a
    player's hand. Thus, no index in a [card_triple] may be the same. *)
type card_triple = index * index * index 

(** The type [command] represents a command inputted by the player. It is
    composed of a verb and optional index arguments. *)
type command = 
  | Quit
  | Pass of card_triple
  | Play of index
  | Help
  | Restart

(** Raised when the user inputs an empty command. *)
exception Empty

(** Raised when the user inputs a malformed command. *)
exception Malformed

(** Raised when the user inputs an index outside of the range of indices in the 
    player's hand of cards. *)
exception InvalidIndex

val parse : string -> command