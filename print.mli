open Unix
open ANSITerminal
open Command
open Card
open Partialdeck
open Round

module type PrintSig = sig 
  (** [print hand h x y] prints a player's hand [h] at locationdifficulty [d]. *)
  val print_hand : Partialdeck.PartialDeck.t -> int -> int -> unit

  (** [new_round d] is a new game of difficulty [d]. *)
  val adjust_cursor : int -> int -> int -> unit

  (** [new_round d] is a new game of difficulty [d]. *)
  val print_table : int -> unit

  (** [new_round d] is a new game of difficulty [d]. *)
  val print_start_menu : unit -> unit

  (** [new_round d] is a new game of difficulty [d]. *)
  val print_help_menu : unit -> unit

  (** [new_round d] is a new game of difficulty [d]. *)
  val print_bot_levels : unit -> unit

  (** [new_round d] is a new game of difficulty [d]. *)
  val score_table : Round.t -> unit

  (** [new_round d] is a new game of difficulty [d]. *)
  val erase_print : string -> unit

  (** [new_round d] is a new game of difficulty [d]. *)
  val print_pile : (Card.card * int) list -> int -> int -> unit

  (** [new_round d] is a new game of difficulty [d]. *)
  val internal_display_history : Round.t -> Round.t

  (** [new_round d] is a new game of difficulty [d]. *)
  val read_line_safe : unit  -> Command.command

end

module Print:PrintSig