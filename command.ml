
type index = int

type card_triple = index * index * index 

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

exception Empty

exception Malformed

exception InvalidIndex

(** [hd_safe lst] is the first word in [lst]. If [lst] is empty, raises
    Empty exception.  *)
let hd_safe = function 
  | [] -> raise Empty
  | h :: t -> if h = "" then raise Empty else h

(** [tl_safe lst] is the [lst] with the first word removed. If [lst] is
    empty, raises Empty exception. *)
let tl_safe = function 
  | [] -> raise Empty
  | _ :: t -> t

(** [get_words acc lst] is all the words in [lst], which does not include
    the empty string. *)
let rec get_words acc = function
  | [] -> List.rev acc
  | h :: t -> if h = "" then get_words acc t else get_words (h::acc) t

(** [get_index ind] is the integer represented by [ind]. If [ind] does not
    represent a valid integer, then raises Malformed. *)
let get_index ind = 
  match int_of_string ind with 
  | exception Failure _ -> raise Malformed
  | _ -> int_of_string ind

(** [make_play rest] is the Play command with the index of the card to play 
    in [rest]. If [rest] does not contain one integer index, then raises
    Malformed. *)
let make_play rest = 
  match get_words [] rest with 
  | h :: [] -> Play (get_index h)
  | _ -> raise Malformed

(** [make_select rest] is the Select command with the corresponding difficult
    contained in [rest]. If rest does not contain a single word representing a 
    difficulty, then raises Malformed.  *)
let make_select rest = 
  match get_words [] rest with 
  | h :: [] -> Select (h)
  | _ -> raise Malformed

(** [make_pass rest] is the Pass command with the indices of the three cards
    to pass in [rest]. If [rest] does not contain three valid indices, then 
    a Malformed exception is raised. *)
let make_pass rest = 
  match get_words [] rest with 
  | a :: b :: c :: [] -> 
    let a', b', c' = (get_index a, get_index b, get_index c) in 
    if a' = b' || b' = c' || c' = a' then raise Malformed 
    else Pass (a', b', c')
  | _ -> raise Malformed

(** [make_command rest lead] is the Command represented by the lead word 
    [lead] and the command phrase in [rest]. If these do not correspond to a 
    valid command, then raises Malformed. *)
let make_command rest lead = 
  match lead, rest with 
  | "quit", [] -> Quit
  | "help", [] -> Help
  | "restart", [] -> Restart
  | "select", _ -> make_select rest
  | "pass", _ -> make_pass rest
  | "play", _ -> make_play rest
  | "back", [] -> Back
  | "deal", [] -> Deal
  | "debug", [] -> Debug
  | _ -> raise Malformed

let parse s = 
  let lst = String.trim s |> String.split_on_char ' ' in
  let first = hd_safe lst in 
  let rest = tl_safe lst in 
  make_command rest first
