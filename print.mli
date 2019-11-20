open Unix
open ANSITerminal
open Command
open Card
open Partialdeck
open Round

module type PrintSig = sig 

  val print_hand : Partialdeck.PartialDeck.t -> int -> int -> unit
  val adjust_cursor : int -> int -> int -> unit
  val print_table : unit -> unit
  val print_start_menu : unit -> unit
  val print_help_menu : unit -> unit
  val print_start_prompt : unit -> unit
  val print_bot_levels : unit -> unit
  val score_table : Round.t -> unit
  val erase_print : string -> unit
  val print_pile : (Card.card * int) list -> int -> int -> unit
  val internal_display_history : Round.t -> Round.t
  val read_line_safe : unit -> Command.command

end

module Print:PrintSig