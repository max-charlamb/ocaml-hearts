(** Print is a collection of printing functions to assist in our GUI. *)

open Unix
open ANSITerminal
open Command
open Card
open Partialdeck
open Round

module type PrintSig = sig 
  (** [print hand h x y] prints a player's hand [h] at locationdifficulty 
      [d]. *)
  val print_hand : Partialdeck.PartialDeck.t -> int -> int -> unit

  (** [adjust_cursor w h i] moves the cursor to the appropriate location 
      to print a card on the table. *)
  val adjust_cursor : int -> int -> int -> unit

  (** [print_table n] prints a green table with [n] rows. *)
  val print_table : int -> unit

  (** [print_start_menu ()] prints the start menu. *)
  val print_start_menu : unit -> unit

  (** [print_help_menu ()] prints the help menu. *)
  val print_help_menu : unit -> unit

  (** [print_bot_levels ()] prints the bot levels for the game. *)
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

  val print_player_win : unit -> unit

  val print_bot_win : unit -> unit

  val print_name_prompt : unit -> unit

end

module Print:PrintSig