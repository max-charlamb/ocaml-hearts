(** Parses user commands *)

(** The type [index] represents the index of a card in a player's hand. *)
type index = int

(** The type [card_triple] represents the index of three unique cards in a
    player's hand. Thus, no index in a [card_triple] may be the same. *)
type card_triple = index * index * index 

(** The type [command] represents a command inputted by the player. It is
    composed of a verb and optional index arguments or difficulty string. *)
type command = 
  | Quit
  | Pass of card_triple
  | Play of index
  | Select of string
  | Help
  | Restart
  | Back
  | Deal
  | Debug

(** Raised when the user inputs an empty command. *)
exception Empty

(** Raised when the user inputs a malformed command. *)
exception Malformed

(** Raised when the user inputs an index outside of the range of indices in the 
    player's hand of cards. *)
exception InvalidIndex

(** [parse str] is the command corresponding to the input entered by the user
    [str]. Raises Malformed if [str] does not represent a valid command. That
    is, if [str] does not contain a valid combination of command verbs and 
    optional index arguments or difficulty string argument. If [str] is empty, 
    raises Empty. *)
val parse : string -> command