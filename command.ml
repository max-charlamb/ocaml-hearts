type index = int

type card_triple = index * index * index 

type command = 
  | Quit
  | Pass of card_triple
  | Play of index
  | Help
  | Restart

exception Empty

exception Malformed

exception InvalidIndex

let get_first = function 
  | [] -> raise Empty
  | h :: t -> h

let get_rest = function 
  | [] -> raise Empty
  | _ :: t -> t

let rec get_words acc = function
  | [] -> List.rev acc
  | h :: t -> if h = "" then get_words acc t else get_words (h::acc) t

let get_index ind = 
  match int_of_string ind with 
  | exception Failure _ -> raise Malformed
  | _ -> int_of_string ind

let make_play rest = 
  match get_words [] rest with 
  | h :: [] -> Play (get_index h)
  | _ -> raise Malformed

let make_pass rest = 
  match get_words [] rest with 
  | a :: b :: c :: [] -> Pass (get_index a, get_index b, get_index c)
  | _ -> raise Malformed

let make_command rest = function
  | "quit" -> Quit
  | "help" -> Help
  | "restart" -> Restart
  | "pass" -> make_pass rest
  | "play" -> make_play rest
  | _ -> raise Malformed

let parse s = 
  let lst = String.trim s |> String.split_on_char ' ' in
  let first = get_first lst in 
  let rest = get_rest lst in 
  make_command rest first