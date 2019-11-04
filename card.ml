open ANSITerminal


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

let suite_to_int = function 
  | Spade -> 3
  | Heart -> 2
  | Club -> 1
  | Diamond -> 0

let rank_to_int = function 
  | Two -> 2 | Three -> 3 | Four -> 4 | Five -> 5 | Six -> 6
  | Seven -> 7 | Eight -> 8 | Nine -> 9 | Ten -> 10 
  | Jack -> 11 | Queen -> 12  | King -> 13 | Ace -> 14

let int_to_rank = function 
  | 2 -> Two | 3 -> Three | 4 -> Four | 5 -> Five | 6 -> Six
  | 7 -> Seven | 8 -> Eight | 9 -> Nine | 10 -> Ten 
  | 11 -> Jack | 12 -> Queen  | 13 -> King | 14 -> Ace
  | _ -> failwith "not allowed value"

let suite_to_string = function
  | Spade -> "♤"
  | Heart -> "♡"
  | Club -> "♧"
  | Diamond -> "♢"

let rank_to_string = function 
  | Two -> "2" | Three -> "3" | Four -> "4" | Five -> "5" | Six -> "6"
  | Seven -> "7" | Eight -> "8" | Nine -> "9" | Ten -> "10"
  | Jack -> "J" | Queen -> "Q"  | King -> "K" | Ace -> "A"

let card_to_string c = 
  let len = String.length ((suite_to_string c.suite)^(rank_to_string c.rank))
  in
  match len with 
  | 4 -> (suite_to_string c.suite) ^ "  " ^ (rank_to_string c.rank)
  | 5 -> (suite_to_string c.suite) ^ " " ^ (rank_to_string c.rank)
  | x -> failwith "error in card_to_string"


let print_card ?bckgnd:(background=on_white) c = 
  match c.suite with
  | Heart
  | Diamond -> print_string [red; background] (card_to_string c)
  | Club 
  | Spade -> print_string [black; background] (card_to_string c)

let print_card_tall c = 
  let x, y = pos_cursor () in
  print_string [on_white] "     ";
  move_cursor (-5) 1;
  print_card c;
  print_string [on_white] "     ";
  set_cursor x y


let compare (x:card) (y:card) = 
  match (suite_to_int x.suite,suite_to_int y.suite) with
  | (s1,s2) when s1 > s2 -> 1
  | (s1,s2) when s1 < s2 -> -1
  | (s1,s2) -> 
    begin
      match (rank_to_int x.rank,rank_to_int y.rank) with
      | (r1,r2) when r1 > r2 -> 1
      | (r1,r2) when r1 < r2 -> -1
      | (r1,r2) -> raise DuplicateCard
    end